package dsentric

trait PathResult[+T] {
  def map[S](f: T => S):PathResult[S]
  def flatMap[S](f: T => PathResult[S]):PathResult[S]
  def liftEmpty:CodecResult[Option[T]]

  def fold[S](d:S)(f: T => S):S
  def toOption:Option[T]
  def toMatcher:Option[Option[T]]
}

trait CodecResult[+T] extends PathResult[T]{
  def map[S](f: T => S):CodecResult[S]
}

case object Empty extends PathResult[Nothing] {
  def map[S](f: Nothing => S): PathResult[Nothing] =
    Empty
  def flatMap[S](f: Nothing => PathResult[S]):PathResult[S] =
    Empty

  def liftEmpty:CodecResult[Option[Nothing]] =
    DCodeSuccess(None)

  def fold[S](d:S)(f: Nothing => S):S = d

  def toOption:Option[Nothing] = None

  def toMatcher:Option[Option[Nothing]] = Some(None)
}

case object DCodeFailure extends PathResult[Nothing] with CodecResult[Nothing] {

  def map[S](f: Nothing => S): CodecResult[Nothing] =
    DCodeFailure
  def flatMap[S](f: Nothing => PathResult[S]):PathResult[S] =
    DCodeFailure

  def liftEmpty:CodecResult[Option[Nothing]] =
    DCodeFailure

  def fold[S](d:S)(f: Nothing => S):S = d

  def toOption:Option[Nothing] = None

  def toMatcher:Option[Option[Nothing]] = None
}

case class DCodeSuccess[T](t:T) extends PathResult[T] with CodecResult[T] {
  def map[S](f: T => S): CodecResult[S] =
    DCodeSuccess(f(t))
  def flatMap[S](f: T => PathResult[S]):PathResult[S] =
    f(t)

  def liftEmpty:CodecResult[Option[T]] =
    DCodeSuccess(Some(t))

  def fold[S](d:S)(f: T => S):S = f(t)

  def toOption:Option[T] = Some(t)

  def toMatcher: Option[Option[T]] = Some(Some(t))
}

object PathResult {
  def apply[T](v:Option[T]):PathResult[T] =
    v.fold[PathResult[T]](Empty)(DCodeSuccess(_))
}


trait Strictness {
  def apply[T](value:RawObject, path:Path, codec:DCodec[T]):PathResult[T] =
    PathLensOps.traverse(value, path).fold[PathResult[T]](Empty){apply(_, codec)}

  def apply[T](value:Raw, codec:DCodec[T]):PathResult[T]
}

/*
  An incorrect type will return a match with None
 */
object EmptyOnWrongType extends Strictness {
  def apply[T](value:Raw, codec:DCodec[T]):PathResult[T] =
    codec.unapply(value).fold[PathResult[T]](Empty)(DCodeSuccess.apply)
}

/*
  An incorrect type will fail to return a match
 */
object IgnoreOnWrongType extends Strictness {
  def apply[T](value:Raw, codec:DCodec[T]):PathResult[T] =
    codec.unapply(value).fold[PathResult[T]](DCodeFailure)(DCodeSuccess.apply)
}