package dsentric.contracts

import dsentric._
import dsentric.failure.IncorrectTypeFailure

trait TraversalOps{

  /**
   * Extracts value from object following the Property Pathway.
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def traverse[D <: DObject, T](value:RawObject, property:PropertyLens[D, T], ignoreBadTypes:Boolean):Traversed[Raw] =
    traverseRaw(value, property._parent, ignoreBadTypes).flatMap{ rawObject =>
      rawObject.get(property._key) match {
        case None =>
          NotFound
        case Some(raw) =>
          Found(raw)
      }
    }

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
  private def traverseRaw[T](value:RawObject, base:BaseContractAux, ignoreBadTypes:Boolean):Traversed[RawObject] = {
    base match {
      case expected:ExpectedObjectProperty[base.AuxD]@unchecked =>
        traverseRaw(value, expected._parent).flatMap { traversedObject =>
          traversedObject.get(expected._key) match {
            case Some(rawObject: RawObject@unchecked) =>
              Found(rawObject)
            case Some(raw) if !ignoreBadTypes =>
              Failed(IncorrectTypeFailure(expected, raw))
            case _ =>
              Found(RawObject.empty)
          }
        }
      case maybe:MaybeObjectLens[base.AuxD]@unchecked =>
        traverseRaw(value, maybe._parent).flatMap { traversedObject =>
          traversedObject.get(maybe._key) match {
            case Some(rawObject: RawObject@unchecked) =>
              Found(rawObject)
            case Some(raw) if !ignoreBadTypes =>
              Failed(IncorrectTypeFailure(maybe, raw))
            case _ =>
              PathEmptyMaybe
          }
        }
      case _:ContractFor[base.AuxD]@unchecked =>
        Found(value)
    }
  }
}

object TraversalOps extends TraversalOps