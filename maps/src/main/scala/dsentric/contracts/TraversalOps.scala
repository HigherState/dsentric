package dsentric.contracts

import dsentric._
import dsentric.failure.IncorrectTypeFailure

trait TraversalOps{

  /**
   * Extracts value from object following the Property Pathway.
   * Its known that there are MaybeObjectProperties in the Path
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def maybeTraverse[D <: DObject, T](value:RawObject, property:PropertyLens[D, T], ignoreBadTypes:Boolean):MaybeAvailable[Raw] =
    maybeTraverseRaw(value, property._parent, ignoreBadTypes).flatMap{ rawObject =>
      rawObject.get(property._key) match {
        case None =>
          NotFound
        case Some(raw) =>
          Found(raw)
      }
    }

  /**
   * Extracts value from object following the Property Pathway.
   * Its known that there are no MaybeObjectProperties in the Path
   * @param value
   * @param property
   * @tparam D
   * @tparam T
   * @return
   */
  def traverse[D <: DObject, T](value:RawObject, property:PropertyLens[D, T], ignoreBadTypes:Boolean):Available[Raw] =
    traverseRaw(value, property._parent, ignoreBadTypes) match {
      case Found(rawObject) =>
        rawObject.get(property._key) match {
          case None =>
            NotFound
          case Some(raw) =>
            Found(raw)
        }
      case f:Failed =>
        f
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
  private def maybeTraverseRaw[T](value:RawObject, base:BaseContractAux, ignoreBadTypes:Boolean):MaybeAvailable[RawObject] = {
    base match {
      case expected:ExpectedObjectPropertyLensLike[base.AuxD]@unchecked =>
        maybeTraverseRaw(value, expected._parent, ignoreBadTypes).flatMap { traversedObject =>
          traversedObject.get(expected._key) match {
            case Some(rawObject: RawObject@unchecked) =>
              Found(rawObject)
            case Some(raw) if !ignoreBadTypes =>
              Failed(IncorrectTypeFailure(expected, raw))
            case _ =>
              Found(RawObject.empty)
          }
        }
      case maybe:MaybeObjectPropertyLens[base.AuxD]@unchecked =>
        maybeTraverseRaw(value, maybe._parent, ignoreBadTypes).flatMap { traversedObject =>
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

  /**
   * Traverses the Property parents list.  Parents should always return a RawObject.  In the case where a property is
   * expected, but no value is found for the property, an empty object is returned.
   * @param value
   * @param parent
   * @tparam D
   * @tparam T
   * @return
   */
  private def traverseRaw[T](value:RawObject, base:BaseContractAux, ignoreBadTypes:Boolean):Valid[RawObject] = {
    base match {
      case prop:ObjectPropertyLens[base.AuxD]@unchecked =>
        traverseRaw(value, prop._parent, ignoreBadTypes) match {
          case Found(traversedObject) =>
            traversedObject.get(prop._key) match {
              case Some(rawObject: RawObject@unchecked) =>
                Found(rawObject)
              case Some(raw) if !ignoreBadTypes =>
                Failed(IncorrectTypeFailure(prop, raw))
              case _ =>
                Found(RawObject.empty)
            }
          case f: Failed =>
            f
        }
      case _:ContractFor[base.AuxD]@unchecked =>
        Found(value)
    }
  }
}

object TraversalOps extends TraversalOps