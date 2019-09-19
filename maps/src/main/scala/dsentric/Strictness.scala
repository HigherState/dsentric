package dsentric

trait Retrieve[+T] {
  def map[S](f: T => S):Retrieve[T]
  def mapWithEmpty[S](f: Option[T] => S):Retrieved[S]
}

trait Retrieved[+T]

case object Empty extends Retrieve[Nothing] {
  def map[S](f: Nothing => S): Retrieve[Nothing] = Empty

  def mapWithEmpty[S](f: Option[Nothing] => S): Retrieved[S] =
    Found(f(None))
}

case object CantDecode extends Retrieve[Nothing] with Retrieved[Nothing] {

  def map[S](f: Nothing => S): Retrieve[Nothing] = CantDecode

  def mapWithEmpty[S](f: Option[Nothing] => S): Retrieved[S] = CantDecode
}

case class Found[T](t:T) extends Retrieve[T] with Retrieved[T] {
  def map[S](f: T => S): Retrieve[S] = Found(f(t))

  def mapWithEmpty[S](f: Option[T] => S): Retrieved[S] = Found(f(Some(t)))
}


trait Strictness {
  def apply[T](value:RawObject, path:Path, codec:DCodec[T]):Retrieve[T] =
    PathLensOps.traverse(value, path).fold[Retrieve[T]](Empty){apply(_, codec)}

  def apply[T](value:Raw, codec:DCodec[T]):Retrieve[T]
}

/*
  An incorrect type will return a match with None
 */
object EmptyOnWrongType extends Strictness {
  def apply[T](value:Raw, codec:DCodec[T]):Retrieve[T] =
    codec.unapply(value).fold[Retrieve[T]](Empty)(Found.apply)
}

/*
  An incorrect type will fail to return a match
 */
object IgnoreOnWrongType extends Strictness {
  def apply[T](value:Raw, codec:DCodec[T]):Retrieve[T] =
    codec.unapply(value).fold[Retrieve[T]](CantDecode)(Found.apply)
}