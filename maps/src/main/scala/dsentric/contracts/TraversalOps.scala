package dsentric.contracts

import dsentric._
import scala.annotation.tailrec


trait TraversalOps{
  /**
   * Extracts value from object using the key value.
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def propertyValue[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Available[T] =
    value.get(property._key).fold[Available[T]](NotFound){raw =>
      property._codec.get(raw).rebase(property)
    }

  /**
   * Extracts value from object following the Property Pathway.
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def traverse[D <: DObject, T](value:RawObject, property:PropertyLens[D, T]):Traversed[T] =
    traverseObject(value, property._parent)
      .flatMap(propertyValue(_, property))

  /**
   * Traverses the Property parents list.  Parents should always return a RawObject.  In the case where a property is
   * expected, but no value is found for the property, an empty object is returned.
   * @param value
   * @param parent
   * @tparam D
   * @tparam T
   * @return
   */
  private def traverseObject[D <: DObject, T](value:RawObject, parent:BaseContract[D]):Traversed[RawObject] = {
    parent match {
      case expected:ExpectedObjectProperty[D] =>
        traverseObject(value, expected._parent).flatMap { traversedObject =>
          val raw = traversedObject.getOrElse(expected._key, RawObject.empty)
          expected._codec.getForTraversal(raw).rebase(expected._root, expected._path)
        }
      case maybe:MaybeObjectLens[D] =>
        traverseObject(value, maybe._parent).flatMap { traversedObject =>
          traversedObject
            .get(maybe._key)
            .fold[Traversed[RawObject]](PathEmptyMaybe){
              raw => maybe._codec.getForTraversal(raw).rebase(maybe._root, maybe._path)
            }
        }
      case _:ContractFor[D] =>
        Found(value)
    }
  }
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
  def traverse[D <: DObject, T](value:RawObject, contract:ContractFor[D], path:Path, codec:DCodec[T]):Traversed[T] =
    traverse(value, path) match {
      case None =>
        PathEmptyMaybe
      case Some((rawObject, key)) =>
        rawObject.get(key)
          .flatMap(codec.unapply)
          .fold[Available[T]](NotFound)(Found.apply)
    }

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
      case PathKey(head, tail) =>
        map
          .get(head) match {
          case Some(m:RawObject@unchecked) =>
            traverse(m, tail)
          case _ =>
            None
        }

      case _ =>
        None
    }
}

object TraversalOps extends TraversalOps