package dsentric.failure

import dsentric._
import dsentric.contracts.{ContractFor, PropertyLens}
import scala.annotation.tailrec

sealed trait Traversed[+T]
sealed trait Available[+T] extends Traversed[T]

case object PathEmptyMaybe extends Traversed[Nothing]

case object NotFound extends Available[Nothing]

final case class Found[+T](value:T) extends Available[T]

final case class TypeFailed(typeFailure: TypeFailure) extends Available[Nothing]



/**
 * Handles resolving Incorrect type behaviours in traversing paths and resolving codec values.
 * Expected path values that are not found will be treated as empty
 */
sealed trait IncorrectTypeBehaviour {
  /**
   * Try to convert value to type specified by the property
   * Returns IncorrectTypeFailure if codec cannot convert
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def apply[D <: DObject, T](value:Raw, property:PropertyLens[D, T]):Available[T] =
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
  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Available[T]

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
  def applyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Available[T]

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
  def verify[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Option[TypeFailure]

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
  def verifyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Option[TypeFailure]

  /**
   * Extracts value from object using the key value.
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def property[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Available[T]

  /**
   * Extracts value from object following the Property Path.
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return First Option is for Path to object found, the second option is for property value found
   */
  def traverse[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Traversed[T] =
    traverse(value, property._root, property._path, property._codec)

  /**
   * Extracts value from object following the path.
   * Returns IncorrectTypeFailure if incorrect type and FailOnIncorrectTypeBehaviour
   * @param value
   * @param contract
   * @param path
   * @param codec
   * @tparam D
   * @tparam T
   * @return First Option is for Path to object found, the second option is for property value found
   */
  def traverse[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):Traversed[T]
}

/*
  An incorrect type will return a match with None
 */
object EmptyOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Available[T] =
    codec.unapply(value).fold[Available[T]](NotFound)(Found.apply)

  def applyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Available[T] =
    keyCodec.unapply(value).fold[Available[T]](NotFound)(Found.apply)

  def verify[D <: DObject, T](value: Raw, contract: ContractFor[D], path: Path, codec: DCodec[T]): Option[TypeFailure] =
    None

  def traverse[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T]): Traversed[T] =
    traverse(value, path) match {
      case None =>
        PathEmptyMaybe
      case Some((rawObject, key)) =>
        rawObject.get(key)
          .flatMap(codec.unapply)
          .fold[Available[T]](NotFound)(Found.apply)
    }

  def property[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Available[T] =
    value.get(property._key)
      .flatMap(property._codec.unapply)
      .fold[Available[T]](NotFound)(Found.apply)

  def verifyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Option[TypeFailure] =
    None

  /**
   * Returns None only if maybe path element not found.
   * Expected path object will default to empty object if not found.
   * Maybe path object will return None if wrong type
   * @param map
   * @param path
   * @return
   */
  @tailrec
  private def traverse(map:RawObject, path:Path):Option[(RawObject, String)] =
    path match {
      case PathKey(head, PathEnd) =>
        Some(map -> head)
      case ExpectedPathKey(head, tail) =>
        map
          .get(head) match {
          case Some(m:RawObject@unchecked) =>
            traverse(m, tail)
          case _ =>
            traverse(Map.empty, tail)
        }
      case PathKey(head, tail) =>
        map
          .get(head) match {
            case Some(m:RawObject@unchecked) =>
              traverse(m, tail)
            case _ =>
              None
          }
      case PathEnd =>
        None
    }
}

/*
  An incorrect type will fail to return a match and copy or modify operations will fail
 */
object FailOnIncorrectTypeBehaviour extends IncorrectTypeBehaviour {
  def apply[D <: DObject, T](value:Raw, contract:ContractFor[D], path:Path, codec:DCodec[T]):Available[T] =
    codec.unapply(value).fold[Available[T]] {
      TypeFailed(IncorrectTypeFailure(contract, path, codec, value))
    }{t:T => Found(t)}

  def applyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Available[T] =
    keyCodec.unapply(value).fold[Available[T]] {
      TypeFailed(IncorrectKeyTypeFailure(contract, path \ value, keyCodec))
    }{t:T => Found(t)}

  def traverse[D <: DObject, T](value: RawObject, contract: ContractFor[D], path: Path, codec: DCodec[T]): Traversed[T] =
    traverse(contract, path, 0, value, path) match {
      case Left(failure) =>
        TypeFailed(failure)
      case Right(None) =>
        PathEmptyMaybe
      case Right(Some((rawObject, key))) =>
        rawObject.get(key) match {
          case None  =>
            NotFound
          case Some(v) =>
            codec.unapply(v).fold[Available[T]] {
              TypeFailed(IncorrectTypeFailure(contract, path, codec, v))
            }{t:T => Found(t)}
        }
    }

  def property[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Available[T] =
    value.get(property._key) match {
      case None =>
        NotFound
      case Some(v) =>
        apply(v, property._root, property._path, property._codec)
    }

  def verify[D <: DObject, T](value: Raw, contract: ContractFor[D], path: Path, codec: DCodec[T]): Option[TypeFailure] =
    if (codec.unapply(value).isEmpty)
      Some(IncorrectTypeFailure(contract, path, codec, value))
    else
      None

  def verifyKey[D <: DObject, T](value:String, contract:ContractFor[D], path:Path, keyCodec:StringCodec[T]):Option[TypeFailure] =
    if (keyCodec.unapply(value).isEmpty)
      Option(IncorrectKeyTypeFailure(contract, path \ value, keyCodec))
    else
      None


  private val empty = Right(None)
  @tailrec
  private def traverse[D <: DObject](contract:ContractFor[D], path:Path, index:Int, map:RawObject, remainder:Path):Either[TypeFailure, Option[(RawObject, String)]] =
    remainder match {
      case PathKey(head, PathEnd) =>
        Right(Some(map -> head))
      case ExpectedPathKey(head, tail) =>
        map
          .get(head) match {
          case Some(m:RawObject@unchecked) =>
            traverse(contract, path, index + 1, m, tail)
          case Some(v) =>
            Left(IncorrectTypeFailure(contract, path.take(index + 1), PessimisticCodecs.dObjectCodec, v))
          case None =>
            traverse(contract, path, index + 1, Map.empty, tail)
        }
      case PathKey(head, tail) =>
        map
          .get(head) match {
          case Some(m:RawObject@unchecked) =>
            traverse(contract, path, index + 1, m, tail)
          case Some(v) =>
            Left(IncorrectTypeFailure(contract, path.take(index + 1), PessimisticCodecs.dObjectCodec, v))
          case None =>
            empty
        }
      case PathEnd =>
        empty
    }
}