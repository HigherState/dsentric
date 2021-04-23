package dsentric.contracts

import dsentric._

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
    traverseRaw(value, property._parent)
      .flatMap(propertyValue(_, property))

  def traverse[Key, Value](value:RawObject, additionalProperties:AdditionalProperties[Key, Value], keyString:String):Traversed[Value] =
    traverseRaw(value, additionalProperties)
      .flatMap{rawObject =>
        rawObject.get(keyString).fold[Available[Value]](NotFound) { raw =>
          additionalProperties._additionalValueCodec.get(raw).rebase(additionalProperties._root, additionalProperties._path \ keyString)
        }
      }

  /**
   * Traverses the Property parents list.  Parents should always return a RawObject.  In the case where a property is
   * expected, but no value is found for the property, an empty object is returned.
   * @param value
   * @param parent
   * @tparam D
   * @tparam T
   * @return
   */
  def traverseRaw[T](value:RawObject, base:BaseContractAux):Traversed[RawObject] = {
    base match {
      case expected:ExpectedObjectProperty[base.AuxD]@unchecked =>
        traverseRaw(value, expected._parent).flatMap { traversedObject =>
          val raw = traversedObject.getOrElse(expected._key, RawObject.empty)
          expected._codec.getForTraversal(raw).rebase(expected._root, expected._path)
        }
      case maybe:MaybeObjectLens[base.AuxD]@unchecked =>
        traverseRaw(value, maybe._parent).flatMap { traversedObject =>
          traversedObject
            .get(maybe._key)
            .fold[Traversed[RawObject]](PathEmptyMaybe){
              raw => maybe._codec.getForTraversal(raw).rebase(maybe._root, maybe._path)
            }
        }
      case _:ContractFor[base.AuxD]@unchecked =>
        Found(value)
    }
  }
}

object TraversalOps extends TraversalOps