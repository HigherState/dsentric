package dsentric.failure

import dsentric._

object IncorrectTypeBehaviour {
  private val empty = Right(None)
  private val none = Some(None)
}

sealed trait IncorrectTypeBehaviour {

  @inline
  protected def empty[T]:ValidResult[Option[T]] =
    IncorrectTypeBehaviour.empty.asInstanceOf[ValidResult[Option[T]]]

  protected def none[T]:Option[Option[T]] =
    IncorrectTypeBehaviour.none.asInstanceOf[Option[Option[T]]]

  def apply[T](value:Raw, path:Path, codec:DCodec[T]):ValidResult[Option[T]]

  def matcher[T](value:Raw, path:Path, codec:DCodec[T]):Option[Option[T]]

  def traverse[T](value:RawObject, path:Path, codec:DCodec[T]):ValidResult[Option[T]] =
    PathLensOps.traverse(value, path).fold(empty[T])(apply(_, path, codec))

  def traverseMatcher[T](value:RawObject, path:Path, codec:DCodec[T]):Option[Option[T]] =
    PathLensOps.traverse(value, path).fold(none[T])(matcher(_, path, codec))
}

/*
  An incorrect type will return a match with None
 */
object EmptyOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[T](value:Raw, path:Path, codec:DCodec[T]):ValidResult[Option[T]] =
    codec.unapply(value).fold(empty[T])(t => ValidResult.success(Some(t)))

  def matcher[T](value:Raw, path:Path, codec:DCodec[T]):Option[Option[T]] =
    Some(codec.unapply(value).fold(Option.empty[T])(t => Some(t)))

}

/*
  An incorrect type will fail to return a match but copy or modify operations will be ignored
 */
object IgnoreOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[T](value:Raw, path:Path, codec:DCodec[T]):ValidResult[Option[T]] =
    codec.unapply(value).fold(ValidResult.failure[Option[T]](IncorrectTypeFailure(path, codec)))(t => ValidResult.success(Some(t)))

  def matcher[T](value:Raw, path:Path, codec:DCodec[T]):Option[Option[T]] =
    codec.unapply(value).fold(Option.empty[Option[T]])(t => Some(Some(t)))
}

/*
  An incorrect type will fail to return a match and copy or modify operations will fail
 */
object FailOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[T](value:Raw, path:Path, codec:DCodec[T]):ValidResult[Option[T]] =
    codec.unapply(value).fold(ValidResult.failure[Option[T]](IncorrectTypeFailure(path, codec)))(t => ValidResult.success(Some(t)))

  def matcher[T](value:Raw, path:Path, codec:DCodec[T]):Option[Option[T]] =
    codec.unapply(value).fold(Option.empty[Option[T]])(t => Some(Some(t)))
}