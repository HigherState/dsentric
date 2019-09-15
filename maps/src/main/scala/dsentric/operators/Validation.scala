package dsentric.operators

import dsentric.contracts.{Property, _}
import dsentric._

trait ValidationFor[D <: DObject] {
  this: BaseContract[D] =>

  def $validate(value:D):PathFailures =
    Validation.validateFields(this._fields, value.value, None)

  def $validate(value:DObject, currentState:D):PathFailures =
    Validation.validateFields(this._fields, value.value, Some(currentState.value))

}


trait Validation extends ValidationFor[DObject] {
  this: BaseContract[DObject] =>
}

object Validation {
  def validateFields[D2 <: DObject](fields:Map[String, Property[D2, _]], value:RawObject, maybeCurrentState:Option[RawObject]):PathFailures = {

    @inline
    def validatePropertyObject[D2 <: DObject](field:String, property:ObjectsProperty[D2, DObject], value:RawObject):PathFailures =
      value.get(field).collect {
        case rvs:Vector[_] =>
          rvs.collect{case rv:RawObject@unchecked => rv }.flatMap { rv =>
            validateFields(property._contract._fields, rv, None)
          }.map(p => property._path ++ p._1 -> p._2)
      }.getOrElse(PathFailures.empty)

    @inline
    def validateContractProperty[D2 <: DObject](field:String, property:BaseContract[DObject]@unchecked with Property[D2, _], value:RawObject, maybeCurrentState:Option[RawObject]):PathFailures =
      value.get(field).collect {
        case rv:RawObject@unchecked =>
          validateFields(property._fields, rv, maybeCurrentState.flatMap(_.get(field).collect{case rs:RawObject@unchecked => rs}))
      }.getOrElse(PathFailures.empty)

    @inline
    def validatePropertyOperators[D2 <: DObject, T](field:String, property:Property[D2, T], value:RawObject, maybeCurrentState:Option[RawObject]):PathFailures = {
      val v = value.get(field)
      val vt = v.flatMap(property._codec.unapply)
      if (v.nonEmpty && vt.isEmpty)
        PathFailures(property._path -> "Value is not of the expected type.")
      else
        property._dataOperators.collect{
          case validator:ValueValidator[T]@unchecked =>
            validator(property._path, vt, maybeCurrentState.flatMap(property._codec.unapply))
          case validator:RawValidator[T]@unchecked =>
            validator(property._path, v, maybeCurrentState.flatMap(_.get(field)))
        }.toVector.flatten
    }

    fields.foldLeft(PathFailures.empty){
      case (failures, (field, property:BaseContract[DObject]@unchecked with Property[D2, _])) =>
        failures ++
          validatePropertyOperators(field, property, value, maybeCurrentState) ++
          validateContractProperty(field, property, value, maybeCurrentState)
      case (failures, (field, property:ObjectsProperty[D2, DObject]@unchecked)) =>
        failures ++
          validatePropertyOperators(field, property, value, maybeCurrentState) ++
          validatePropertyObject(field, property, value)
      case (failures, (field, property)) =>
        failures ++
          validatePropertyOperators(field, property, value, maybeCurrentState)
    }
  }

}