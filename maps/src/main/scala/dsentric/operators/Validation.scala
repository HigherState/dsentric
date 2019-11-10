package dsentric.operators

import dsentric.contracts.{Property, _}
import dsentric._


trait ClosedFields

object Validation {
  def validateContract[D <: DObject](contract:BaseContract[D], value:RawObject, maybeCurrentState:Option[RawObject]):PathFailures = {

    @inline
    def validateObjectsProperty[D2 <: DObject, D3 <: DObject](field:String, property:ObjectsProperty[D2, D3], value:RawObject):PathFailures =
      value.get(field).collect {
        case rvs:Vector[_] =>
          rvs.collect{case rv:RawObject@unchecked => rv }.zipWithIndex.flatMap { rvi =>
            validateContract(property._contract, rvi._1, None)
              .map(p => property._path \ rvi._2 ++ p._1 -> p._2)
          }
      }.getOrElse(PathFailures.empty)

    @inline
    def validateMapObjectsProperty[K, D2 <: DObject, D3 <: DObject](field:String, property:MapObjectsProperty[D2, K, D3], value:RawObject, maybeCurrentState:Option[RawObject]):PathFailures =
      value.get(field).collect {
        case rvs:RawObject@unchecked =>
          rvs.toIterator.flatMap{ case (k, v) =>
            val keyFailures =
              if (property._codec.keyCodec.unapply(k).isEmpty)
                PathFailures(property._path ->  "Invalid key value '$k'.")
              else
                PathFailures.empty

            val valueFailures =
              v match {
                case r:RawObject@unchecked =>
                  validateContract(property._contract, r, maybeCurrentState.flatMap(_.get(k).collect{ case r:RawObject@unchecked => r}))
                    .map(p => property._path \ k ++ p._1 -> p._2)
                case _ =>
                  PathFailures(property._path \ k ->  "Value is not of the expected type.")
              }

            keyFailures ++ valueFailures
          }.toVector
      }.getOrElse(PathFailures.empty)


    @inline
    def validateContractProperty[D2 <: DObject](field:String, property:BaseContract[DObject] with Property[D2, _], value:RawObject, maybeCurrentState:Option[RawObject]):PathFailures =
      value.get(field).collect {
        case rv:RawObject@unchecked =>
          validateContract(property, rv, maybeCurrentState.flatMap(_.get(field).collect{case rs:RawObject@unchecked => rs}))
      }.getOrElse(PathFailures.empty)

    @inline
    def validateProperty[D2 <: DObject, T](field:String, property:Property[D2, T], value:RawObject, maybeCurrentState:Option[RawObject]):PathFailures =
      value.get(field) match {
        case Some(DNull) =>
          validatePropertyOperators(field, property, Some(DNull), None, maybeCurrentState)
        case Some(v) =>
          property._codec.unapply(v)
            .fold(PathFailures(property._path -> "Value is not of the expected type.")){ maybeT =>
              validatePropertyOperators(field, property, Some(v), Some(maybeT), maybeCurrentState)
            }
        case None =>
          validatePropertyOperators(field, property, None, None, maybeCurrentState)
      }

    @inline
    def validatePropertyOperators[D2 <: DObject, T](field:String, property:Property[D2, T], maybeValue:Option[Raw], maybeT:Option[T], maybeCurrentState:Option[RawObject]):PathFailures =
      property._dataOperators.collect{
        case validator:ValueValidator[T]@unchecked =>
          validator(property._path, maybeT, maybeCurrentState.flatMap(_.get(field)).flatMap(property._codec.unapply))
        case validator:RawValidator[T]@unchecked =>
          validator(property._path, maybeValue, maybeCurrentState.flatMap(_.get(field)))
      }.toVector.flatten

    @inline
    def validateClosed[D2 <: DObject](baseContract:BaseContract[D], value:RawObject): PathFailures =
      if (baseContract.isInstanceOf[ClosedFields]) {
        val path = baseContract match {
          case p: Property[D2, _]@unchecked => p._path
          case _ => Path.empty
        }
        value.keySet.filterNot(baseContract._fields.keySet).map { k =>
          path -> s"Additional key '$k' not allowed."
        }.toVector
      }
      else
        PathFailures.empty

    validateClosed(contract, value) ++
    contract._fields.foldLeft(PathFailures.empty){
      case (failures, (field, property:BaseContract[DObject]@unchecked with Property[D, _])) =>
        failures ++
        validateProperty(field, property, value, maybeCurrentState) ++
        validateContractProperty(field, property, value, maybeCurrentState)
      case (failures, (field, property:ObjectsProperty[D, _]@unchecked)) =>
        failures ++
        validateProperty(field, property, value, maybeCurrentState) ++
        validateObjectsProperty(field, property, value)
      case (failures, (field, property:MapObjectsProperty[_, D, _]@unchecked)) =>
        failures ++
        validateProperty(field, property, value, maybeCurrentState) ++
        validateMapObjectsProperty(field, property, value, maybeCurrentState)
      case (failures, (field, property)) =>
        failures ++
        validateProperty(field, property, value, maybeCurrentState)
    }
  }

}