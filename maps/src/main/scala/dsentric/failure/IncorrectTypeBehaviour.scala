package dsentric.failure

import cats.data.NonEmptyList
import dsentric._
import dsentric.contracts.{ContractFor, PropertyLens}

object IncorrectTypeBehaviour {
  private val empty = Right(None)
  private val none = Some(None)
}

sealed trait IncorrectTypeBehaviour {

  @inline
  protected def empty[T]:Either[NonEmptyList[StructuralFailure], Option[T]] =
    IncorrectTypeBehaviour.empty.asInstanceOf[Either[NonEmptyList[StructuralFailure], Option[T]]]

  protected def none[T]:Option[Option[T]] =
    IncorrectTypeBehaviour.none.asInstanceOf[Option[Option[T]]]

  def apply[D <: DObject, T](value:Raw, property:PropertyLens[D, T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    apply(value, property._root, property._path, property._codec)

  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]]

  def applyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]]

  def verify[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):List[StructuralFailure]

  def verifyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):List[StructuralFailure]


  def matcher[D <: DObject, T](value:Raw, property:PropertyLens[D, T]):Option[Option[T]] =
    matcher(value, property._root, property._path, property._codec)

  def matcher[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]]

  def traverse[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    traverse(value, property._root, property._path, property._codec)

  def traverse[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]]

  def traverseMatcher[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Option[Option[T]] =
    PathLensOps.traverse(value, property._path).fold(none[T])(matcher(_, property._root, property._path, property._codec))

  def traverseMatcher[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]] =
    PathLensOps.traverse(value, path).fold(none[T])(matcher(_, contract, path, codec))

  def traverseVerify[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):List[StructuralFailure] =
    traverseVerify(value, property._root, property._path, property._codec)

  def traverseVerify[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):List[StructuralFailure]



}

/*
  An incorrect type will return a match with None
 */
object EmptyOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    codec.unapply(value).fold(empty[T])(t => Right(Some(t)))

  def applyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    keyCodec.unapply(value).fold(empty[T])(t => Right(Some(t)))

  def verify[D <: DObject, T](value: Raw, contract: ContractFor[D], path: Path, codec: DCodec[T]): List[StructuralFailure] =
    Nil

  def matcher[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]] =
    Some(codec.unapply(value).fold(Option.empty[T])(t => Some(t)))

  def traverse[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T]): Either[NonEmptyList[StructuralFailure], Option[T]] =
    PathLensOps.traverse(value, path)
      .flatMap(codec.unapply) match {
      case None if path.isExpected =>
        Left(NonEmptyList(ExpectedFailure(contract, path), Nil))
      case value =>
        Right(value)
    }

  def traverseVerify[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T]): List[StructuralFailure] =
    if (path.isExpected && PathLensOps.traverse(value, path).flatMap(codec.unapply).isEmpty)
      List(ExpectedFailure(contract, path))
    else
      Nil

  def verifyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):List[StructuralFailure] =
    Nil
}

/*
  An incorrect type will fail to return a match and copy or modify operations will fail
 */
object FailOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    codec.unapply(value).fold[Either[NonEmptyList[StructuralFailure], Option[T]]] {
      Left(NonEmptyList(IncorrectTypeFailure(contract, path, codec, value), Nil))
    }{t:T => Right(Some(t))}

  def applyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    keyCodec.unapply(value).fold[Either[NonEmptyList[StructuralFailure], Option[T]]] {
      Left(NonEmptyList(IncorrectKeyTypeFailure(contract, path \ value, keyCodec), Nil))
    }{t:T => Right(Some(t))}

  def traverse[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T]): Either[NonEmptyList[StructuralFailure], Option[T]] =
    PathLensOps.traverse(value, path) match {
      case None if path.isExpected =>
        Left(NonEmptyList(ExpectedFailure(contract, path), Nil))
      case None =>
        empty
      case Some(v) =>
        apply(v, contract, path, codec)
    }

  def verify[D <: DObject, T](value: Raw, contract: ContractFor[D], path: Path, codec: DCodec[T]): List[StructuralFailure] =
    if (codec.unapply(value).isEmpty)
      List(IncorrectTypeFailure(contract, path, codec, value))
    else
      Nil

  def matcher[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]] =
    codec.unapply(value).fold(Option.empty[Option[T]])(t => Some(Some(t)))

  def traverseVerify[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T]): List[StructuralFailure] =
    PathLensOps.traverse(value, path) match {
      case None if path.isExpected =>
        List(ExpectedFailure(contract, path))
      case Some(codec(_)) | None =>
        Nil
      case Some(v) =>
        List(IncorrectTypeFailure(contract, path, codec, v))
    }

  def verifyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):List[StructuralFailure] =
    if (keyCodec.unapply(value).isEmpty)
      List(IncorrectKeyTypeFailure(contract, path \ value, keyCodec))
    else
      Nil
}