package dsentric.failure

import dsentric._
import dsentric.contracts.{ContractFor, PropertyLens}

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

  def apply[D <: DObject, T](value:Raw, property:PropertyLens[D, T]):ValidResult[Option[T]] =
    apply(value, property._root, property._path, property._codec)

  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):ValidResult[Option[T]]

  def matcher[D <: DObject, T](value:Raw, property:PropertyLens[D, T]):Option[Option[T]] =
    matcher(value, property._root, property._path, property._codec)

  def matcher[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]]

  def traverse[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):ValidResult[Option[T]] =
    PathLensOps.traverse(value, property._path).fold(empty[T])(apply(_, property._root, property._path, property._codec))

  def traverse[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):ValidResult[Option[T]] =
    PathLensOps.traverse(value, path).fold(empty[T])(apply(_, contract, path, codec))

  def traverseMatcher[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Option[Option[T]] =
    PathLensOps.traverse(value, property._path).fold(none[T])(matcher(_, property._root, property._path, property._codec))

  def traverseMatcher[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]] =
    PathLensOps.traverse(value, path).fold(none[T])(matcher(_, contract, path, codec))

  def verify[D <: DObject, T](value:RawObject, property:PropertyLens[D, T], expected:Boolean):List[Failure] =
    verify(value, property._root, property._path, property._codec, expected)

  def verify[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T], expected:Boolean):List[Failure]
}

/*
  An incorrect type will return a match with None
 */
object EmptyOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):ValidResult[Option[T]] =
    codec.unapply(value).fold(empty[T])(t => ValidResult.success(Some(t)))

  def matcher[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]] =
    Some(codec.unapply(value).fold(Option.empty[T])(t => Some(t)))

  def verify[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T], expected: Boolean): List[Failure] =
    if (expected && PathLensOps.traverse(value, path).isEmpty)
      List(ExpectedFailure(contract, path))
    else
      Nil
}

/*
  An incorrect type will fail to return a match and copy or modify operations will fail
 */
object FailOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):ValidResult[Option[T]] =
    codec.unapply(value).fold(ValidResult.failure[Option[T]](IncorrectTypeFailure(contract, path, codec, value.getClass)))(t => ValidResult.success(Some(t)))

  def matcher[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]] =
    codec.unapply(value).fold(Option.empty[Option[T]])(t => Some(Some(t)))

  def verify[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T], expected: Boolean): List[Failure] =
    PathLensOps.traverse(value, path) match {
      case None if expected =>
        List(ExpectedFailure(contract, path))
      case Some(codec(_)) | None =>
        Nil
      case Some(v) =>
        List(IncorrectTypeFailure(contract, path, codec, v.getClass))
    }
}