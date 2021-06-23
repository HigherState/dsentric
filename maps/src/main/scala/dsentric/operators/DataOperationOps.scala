package dsentric.operators

import dsentric.codecs.{DCodec, DCollectionCodec, DContractCodec, DCoproductCodec, DMapCodec, DTypeContractCodec}
import dsentric.contracts.{BaseContract, CustomPathSetter, DefaultProperty, ExpectedObjectProperty, MaybeExpectedObjectProperty, MaybeObjectProperty, PathSetter, Property}
import dsentric.{DObject, DObjectInst, Raw, RawArray, RawObject}

trait DataOperationOps{

  def sanitize[D <: DObject](contract:BaseContract[D]):PathSetter[D] =
    CustomPathSetter(
      transform(contract){
        case (s:Sanitizer[Any], _, maybeRaw) =>
          s.sanitize(maybeRaw)
      },
      d => d
    )

  def transform[D0 <: DObject](contract:BaseContract[D0], isDelta:Boolean)(pf:PartialFunction[(DataOperator[_], Property[_, _], Option[Raw]), Option[Raw]]):Function[RawObject, RawObject] = { d0 =>

    def objectTransform[D <: DObject](contract:BaseContract[D], rawObject:RawObject):Option[RawObject] = {
      def transformObjectValue[T](obj:RawObject, key:String, property:Property[D, T]):Option[RawObject] =  {
        val current = obj.get(key)
        property._dataOperators
          .map(d => (d, property, current))
          .collectFirst(pf)
          .filter(_ != current)
          .map{
            case None =>
              obj - key
            case Some(newValue) =>
              obj + (key -> newValue)
          }
      }

      def transformObjectValueWithDefault[T](obj:RawObject, key:String, property:Property[D, T], default:Raw):Option[RawObject] =  {
        val current = obj.getOrElse(key, default)
        property._dataOperators
          .map(d => (d, property, Some(current)))
          .collectFirst(pf)
          .filter(_ != current)
          .map{
            case None =>
              obj - key
            case Some(newValue) =>
              obj + (key -> newValue)
          }
      }

      def maybeObjectPropertyTransform(propertyContract:BaseContract[D], propertyObject:RawObject, key:String):Option[RawObject] =
        propertyObject.get(key) match {
          case Some(nested:RawObject@unchecked) =>
            objectTransform(propertyContract, nested)
              .map{newNested => propertyObject + (key -> newNested)}
          case _ =>
            None
        }

      def expectedObjectPropertyTransform(propertyContract:BaseContract[D], propertyObject:RawObject, key:String):Option[RawObject] =
        propertyObject.get(key) match {
          case Some(nested:RawObject@unchecked) =>
            objectTransform(propertyContract, nested)
              .map{newNested => propertyObject + (key -> newNested)}
          case _ =>
            objectTransform(propertyContract, RawObject.empty)
              .map{newNested => propertyObject + (key -> newNested)}
        }

      def checkCodecs[T](rawObject:RawObject, key:String, codec:DCodec[T]):Option[RawObject] =
        rawObject.get(key) -> codec match {
          case (Some(rawObject:RawObject@unchecked), DContractCodec(codecContract)) =>
            objectTransform(codecContract, rawObject)
              .map{newObject => rawObject + (key -> newObject)}
          case (Some(rawObject:RawObject@unchecked), d:DTypeContractCodec) =>
            d.contracts.lift(new DObjectInst(rawObject)).flatMap{typeContract =>
              objectTransform(typeContract, rawObject)
                .map{newObject => rawObject + (key -> newObject)}
            }
          case (Some(rawObject:RawObject@unchecked), d:DMapCodec[T, _, _]) if d.containsContractCodec =>
            rawObject.keys.foldLeft(Option.empty[RawObject]){ (maybeChangedObject, mapKey) =>
              checkCodecs(maybeChangedObject.getOrElse(rawObject), mapKey, d.valueCodec)
                .orElse(maybeChangedObject)
            }
          case (Some(rawArray: RawArray@unchecked), d:DCollectionCodec[T, _]) if d.containsContractCodec =>
            ???

          case (Some(raw), d:DCoproductCodec[T, _]) if d.containsContractCodec =>
            ???
          case _ =>
            None
        }

      contract._fields.foldLeft[Option[RawObject]](None){
        case (maybeChange, (key, p:MaybeObjectProperty[D])) =>
          val transformedObject =
            transformObjectValue(maybeChange.getOrElse(rawObject), key, p)
              .orElse(maybeChange)

          maybeObjectPropertyTransform(p, transformedObject.getOrElse(rawObject), key)
            .orElse(transformedObject)

        case (maybeChange, (key, p:ExpectedObjectProperty[D])) =>
          val transformedObject =
            transformObjectValueWithDefault(maybeChange.getOrElse(rawObject), key, p, RawObject.empty)
              .orElse(maybeChange)
          expectedObjectPropertyTransform(p, transformedObject.getOrElse(rawObject), key)
            .orElse(transformedObject)

        case (maybeChange, (key, p:MaybeExpectedObjectProperty[D])) =>
          val transformedObject =
            transformObjectValueWithDefault(maybeChange.getOrElse(rawObject), key, p, RawObject.empty)
              .orElse(maybeChange)
          expectedObjectPropertyTransform(p, transformedObject.getOrElse(rawObject), key)
            .orElse(transformedObject)

        case (maybeChange, (key, p:DefaultProperty[D, _])) =>
          val maybeTransformed =
            transformObjectValueWithDefault(maybeChange.getOrElse(rawObject), key, p, p.__rawDefault)
              .orElse(maybeChange)
          checkCodecs(maybeTransformed.getOrElse(rawObject), key, p._codec)
            .orElse(maybeTransformed)

        case (maybeChange, (key, p:Property[D, _])) =>
          val maybeTransformed =
            transformObjectValue(maybeChange.getOrElse(rawObject), key, p)
              .orElse(maybeChange)
          checkCodecs(maybeTransformed.getOrElse(rawObject), key, p._codec)
            .orElse(maybeTransformed)
      }
    }

    objectTransform(contract, d0)
      .getOrElse(d0)
  }
}

object DataOperationOps extends DataOperationOps
