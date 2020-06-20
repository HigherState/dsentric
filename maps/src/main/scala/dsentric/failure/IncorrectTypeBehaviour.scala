package dsentric.failure

import cats.data.NonEmptyList
import dsentric._
import dsentric.contracts.{ContractFor, Expected, PropertyLens}

object IncorrectTypeBehaviour {
  private val empty = Right(None)
  private val someNone = Some(None)
}

sealed trait IncorrectTypeBehaviour {

  @inline
  protected def empty[T]:Either[NonEmptyList[StructuralFailure], Option[T]] =
    IncorrectTypeBehaviour.empty.asInstanceOf[Either[NonEmptyList[StructuralFailure], Option[T]]]

  protected def someNone[T]:Option[Option[T]] =
    IncorrectTypeBehaviour.someNone.asInstanceOf[Option[Option[T]]]

  /**
   * Try to convert value to type specified by the property
   * Returns IncorrectTypeFailure if codec cannot convert
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def apply[D <: DObject, T](value:Raw, property:PropertyLens[D, T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    apply(value, property._root, property._path, property._codec)

  /**
   * Try to convert value to type specified by the codec
   * Returns IncorrectTypeFailure or None if codec cannot convert
   * @param value
   * @param contract
   * @param path
   * @param codec
   * @tparam D
   * @tparam T
   * @return
   */
  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]]

  /**
   * Attempts to convert the raw object key string value to the key type.
   * Returns IncorrectTypeFailure or None if codec fails
   * @param value
   * @param contract
   * @param path
   * @param keyCodec
   * @tparam D
   * @tparam T
   * @return
   */
  def applyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]]

  /**
   * Checks the type is correct for the codec,
   * Always returns no failures if EmptyOnIncorrectType
   * @param value
   * @param contract
   * @param path
   * @param codec
   * @tparam D
   * @tparam T
   * @return
   */
  def verify[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):List[StructuralFailure]

  /**
   * Checks that the key type is correct for the codec
   * Always returns no failures if EmptyOnIncorrectType
   * @param value
   * @param contract
   * @param path
   * @param keyCodec
   * @tparam D
   * @tparam T
   * @return
   */
  def verifyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):List[StructuralFailure]

  /**
   * Attempts to match the type of the value with the Property Codec
   * On EmptyOnIncorrectTypeBehaviour type failure will return Some(None)
   * On FailOnIncorrectTypeBehaviour type failure will return None
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def matcher[D <: DObject, T](value:Raw, property:PropertyLens[D, T]):Option[Option[T]] =
    matcher(value, property._codec)

  /**
   * Attempts to match the type of the value with the Codec
   * On EmptyOnIncorrectTypeBehaviour type failure will return Some(None)
   * On FailOnIncorrectTypeBehaviour type failure will return None
   * @param value
   * @param codec
   * @tparam D
   * @tparam T
   * @return
   */
  def matcher[D <: DObject, T](value:Raw, codec:DCodec[T]):Option[Option[T]]

  /**
   * Extracts value from object following the Property Path.
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * Otherwise only fails if Path is expected with ExpectedFailure
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def traverse[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    traverse(value, property._root, property._path, property._codec)

  /**
   * Extracts value from object using the key value.
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * Otherwise only fails if Property is Expected with ExpectedFailure
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def property[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Either[NonEmptyList[StructuralFailure], Option[T]]

  /**
   * Extracts value from object following the path.
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * Otherwise only fails if Path is expected with ExpectedFailure
   * @param value
   * @param contract
   * @param path
   * @param codec
   * @tparam D
   * @tparam T
   * @return
   */
  def traverse[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):Either[NonEmptyList[StructuralFailure], Option[T]]

  /**
   * Extracts value from object following the Property Path.
   * Returns Some(None) if value not found in path
   * On EmptyOnIncorrectTypeBehaviour type failure will return Some(None)
   * On FailOnIncorrectTypeBehaviour type failure will return None
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def traverseMatcher[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Option[Option[T]] =
    PathLensOps.traverse(value, property._path).fold(someNone[T])(matcher(_, property._codec))

  /**
   * Extracts value from object following the Path.
   * Returns Some(None) if value not found in path
   * On EmptyOnIncorrectTypeBehaviour type failure will return Some(None)
   * On FailOnIncorrectTypeBehaviour type failure will return None
   * @param value
   * @param contract
   * @param path
   * @param codec
   * @tparam D
   * @tparam T
   * @return
   */
  def traverseMatcher[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[Option[T]] =
    PathLensOps.traverse(value, path).fold(someNone[T])(matcher(_, codec))

  /**
   * Verifies value for the property.
   * Returns Empty if no failure
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * Otherwise only fails if Path is expected with ExpectedFailure
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def traverseVerify[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):List[StructuralFailure] =
    traverseVerify(value, property._root, property._path, property._codec)

  /**
   * Verifies value for the path.
   * Returns Empty if no failure
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * Otherwise only fails if Path is expected with ExpectedFailure
   * @param value
   * @param contract
   * @param path
   * @param codec
   * @tparam D
   * @tparam T
   * @return
   */
  def traverseVerify[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):List[StructuralFailure]

  /**
   * Verifies value for the property using the key.
   * Returns Empty if no failure
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * Otherwise only fails if Path is expected with ExpectedFailure
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def propertyVerify[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):List[StructuralFailure]
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

  def matcher[D <: DObject, T](value:Raw, codec:DCodec[T]):Option[Option[T]] =
    Some(codec.unapply(value).fold(Option.empty[T])(t => Some(t)))

  def traverse[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T]): Either[NonEmptyList[StructuralFailure], Option[T]] =
    PathLensOps.traverse(value, path)
      .flatMap(codec.unapply) match {
      case None if path.isExpected =>
        Left(NonEmptyList(ExpectedFailure(contract, path), Nil))
      case value =>
        Right(value)
    }

  def property[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    value.get(property._key).flatMap(property._codec.unapply) match {
      case None if property.isInstanceOf[Expected] =>
        Left(NonEmptyList(ExpectedFailure(property._root, property._path), Nil))
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

  def propertyVerify[D <: DObject, T](value: RawObject, property:PropertyLens[D, T]): List[StructuralFailure] =
    if (property.isInstanceOf[Expected] && !value.contains(property._key))
      List(ExpectedFailure(property._root, property._path))
    else
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

  def property[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Either[NonEmptyList[StructuralFailure], Option[T]] =
    value.get(property._key) match {
      case None if property.isInstanceOf[Expected] =>
        Left(NonEmptyList(ExpectedFailure(property._root, property._path), Nil))
      case None =>
        empty
      case Some(v) =>
        apply(v, property._root, property._path, property._codec)
    }

  def verify[D <: DObject, T](value: Raw, contract: ContractFor[D], path: Path, codec: DCodec[T]): List[StructuralFailure] =
    if (codec.unapply(value).isEmpty)
      List(IncorrectTypeFailure(contract, path, codec, value))
    else
      Nil

  def matcher[D <: DObject, T](value:Raw, codec:DCodec[T]):Option[Option[T]] =
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

  def propertyVerify[D <: DObject, T](value: RawObject, property:PropertyLens[D, T]): List[StructuralFailure] = {
    val CODEC = property._codec
    value.get(property._key) match {
      case None if property.isInstanceOf[Expected] =>
        List(ExpectedFailure(property._root, property._path))
      case Some(CODEC(_)) | None =>
        Nil
      case Some(v) =>
        List(IncorrectTypeFailure(property._root, property._path, property._codec, v))
    }
  }
}