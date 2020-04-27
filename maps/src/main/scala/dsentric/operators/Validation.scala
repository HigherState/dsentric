package dsentric.operators

import dsentric.contracts.{Property, _}
import dsentric._
import dsentric.failure.{ClosedContractFailure, IncorrectKeyTypeFailure, IncorrectTypeFailure, ValidationFailures}


object Validation {
  def validateContract[D <: DObject](contract:BaseContract[D], value:RawObject, maybeCurrentState:Option[RawObject]):ValidationFailures = {

    @inline
    def validateObjectsProperty[D2 <: DObject, D3 <: DObject](field:String, property:ObjectsProperty[D2, D3], value:RawObject):ValidationFailures =
      value.getOrElse(field, RawArray.empty) match {
        case array:RawArray@unchecked =>
          val (failures, maybeCollection) = array.zipWithIndex.foldRight((ValidationFailures.empty, Option(List.empty[D3]))){
            case ((obj:RawObject@unchecked, index), (failures, maybeElements)) =>
              property._valueCodec.unapply(obj).fold[(ValidationFailures, Option[List[D3]])]{
                val f = IncorrectTypeFailure(property._root, property._path \ index, property._valueCodec, obj)
                (f :: failures) -> None
              } { t  =>
                val fs = validateContract(property._contract, obj, None).map(_.rebase(property._root, property._path \ index))
                (fs ++ failures) -> maybeElements.map(t :: _)
              }

            case ((raw, index), (failures, _)) =>
              val f = IncorrectTypeFailure(property._root, property._path \ index, property._valueCodec, raw)
              (f :: failures) -> None
          }
          //if we can transform the vector we can validate the property
          maybeCollection.foldLeft(failures)((f, c) => f ++ validatePropertyOperators(field, property, Some(array), Some(c.toVector), maybeCurrentState))
        case DNull =>
          validatePropertyOperators(field, property, Some(RawArray.empty), Some(Vector.empty[D3]), maybeCurrentState)
        case raw =>
          ValidationFailures(IncorrectTypeFailure(property._root, property._path, property._codec, raw))
      }

    @inline
    def validateMapObjectsProperty[K, D2 <: DObject, D3 <: DObject](field:String, property:MapObjectsProperty[D2, K, D3], value:RawObject, maybeCurrentState:Option[RawObject]):ValidationFailures =
      value.getOrElse(field, RawObject.empty) match {
        case map: RawObject@unchecked =>
          val (failures, maybeMap) =
            map.foldRight((ValidationFailures.empty, Option(Map.empty[K, D3]))){
              case ((key, obj:RawObject@unchecked), (failures, maybeMap)) =>
                val (failures2, maybeKey) =
                  property._codec.keyCodec.unapply(key).fold[(ValidationFailures, Option[K])]{
                    (IncorrectKeyTypeFailure(property._root, property._path, property._codec.keyCodec, key) :: failures) -> None
                  }{k => failures -> Some(k)}

                property._valueCodec.unapply(obj).fold[(ValidationFailures, Option[Map[K, D3]])]{
                  val f = IncorrectTypeFailure(property._root, property._path \ key, property._valueCodec, obj)
                  (f :: failures2) -> None
                } { t  =>
                  val cs = maybeCurrentState.flatMap(_.get(field).collect {
                    case r: RawObject@unchecked =>
                      r.get(key).collect {case r2: RawObject@unchecked => r2 }
                  }.flatten)
                  val fs = validateContract(property._contract, obj, cs).map(_.rebase(property._root, property._path \ key))
                  val maybeMap2 =
                    for {
                      m <- maybeMap
                      k <- maybeKey
                    } yield m + (k -> t)
                  (fs ++ failures2) -> maybeMap2
                }

              case ((key, raw), (failures, _)) =>
                val failures2 =
                  if (property._codec.keyCodec.unapply(key).isEmpty)
                    IncorrectKeyTypeFailure(property._root, property._path, property._codec.keyCodec, key) :: failures
                  else failures
                val failures3 =
                  IncorrectTypeFailure(property._root, property._path \ key, property._codec.valueCodec, raw) :: failures2
                failures3 -> None
            }
          //if we can transform the vector we can validate the property
          maybeMap.foldLeft(failures)((f, c) => f ++ validatePropertyOperators(field, property, Some(map), Some(c), maybeCurrentState))

        case DNull =>
          validatePropertyOperators(field, property, Some(RawObject.empty), Some(Map.empty[K, D3]), maybeCurrentState)
        case raw =>
          ValidationFailures(IncorrectTypeFailure(property._root, property._path, property._codec, raw))
      }

    @inline
    def validateContractProperty[D2 <: DObject](field:String, property:BaseContract[DObject] with Property[D2, _], value:RawObject, maybeCurrentState:Option[RawObject]):ValidationFailures =
      value.get(field).collect {
        case rv:RawObject@unchecked =>
          validateContract(property, rv, maybeCurrentState.flatMap(_.get(field).collect{case rs:RawObject@unchecked => rs}))
      }.getOrElse(ValidationFailures.empty)

    @inline
    def validateProperty[D2 <: DObject, T](field:String, property:Property[D2, T], value:RawObject, maybeCurrentState:Option[RawObject]):ValidationFailures =
      value.get(field) match {
        case Some(DNull) =>
          validatePropertyOperators(field, property, Some(DNull), None, maybeCurrentState)
        case Some(v) =>
          property._codec.unapply(v)
            .fold(ValidationFailures(IncorrectTypeFailure(property, v))){ maybeT =>
              validatePropertyOperators(field, property, Some(v), Some(maybeT), maybeCurrentState)
            }
        case None =>
          validatePropertyOperators(field, property, None, None, maybeCurrentState)

      }

    @inline
    def validatePropertyOperators[D2 <: DObject, T](field:String, property:Property[D2, T], maybeValue:Option[Raw], maybeT:Option[T], maybeCurrentState:Option[RawObject]):ValidationFailures =
      property._dataOperators.collect{
        case validator:ValueValidator[T]@unchecked =>
          maybeT.fold(ValidationFailures.empty) { t =>
            validator(property._root, property._path, t, maybeCurrentState.flatMap(_.get(field)).flatMap(property._codec.unapply))
          }
        case validator:RawValidator[T]@unchecked =>
          validator(property._root, property._path, maybeValue, maybeCurrentState.flatMap(_.get(field)))
      }.flatten

//    @inline
//    def validateAdditionalProperties[D2 <: DObject](baseContract:BaseContract[D], value:RawObject, maybeCurrentState:Option[RawObject]): ValidationFailures = {
//      lazy val additionalProperties:RawObject = value -- baseContract._fields.keys
//      lazy val additionalPropertiesState:Option[RawObject] = maybeCurrentState.map(_ -- baseContract._fields.keys)
//      baseContract.$additionalProperties match {
//        case OpenProperties =>
//          ValidationFailures.empty
//        case ClosedProperties =>
//          additionalProperties.filterNot(kv => baseContract._fields.keySet(kv._1) || (kv._2 == DNull && maybeCurrentState.nonEmpty))
//            .map { kv =>
//              baseContract match {
//                case p: Property[D2, _]@unchecked =>
//                  ClosedContractFailure(p._root, p._path, kv._1)
//                case b: ContractFor[D] =>
//                  ClosedContractFailure(b, Path.empty, kv._1)
//              }
//            }.toList
//        case p:PatternProperties[_]@unchecked => ???
//        case _ => ???
////          lazy val t = additionalProperties.map{ case (p.keyCodec(k), p) =>
////            p.data
////          }
////          value.
////          ._codec.keyCodec.unapply(key).fold[(ValidationFailures, Option[K])]{
////            (IncorrectKeyTypeFailure(property._root, property._path, property._codec.keyCodec, key) :: failures) -> None
////          }{k => failures -> Some(k)}
//
//      }
//    }

    def convertExpectedObjectBehaviour(field:String, value:RawObject, maybeCurrentState:Option[RawObject]):RawObject =
      value.get(field) -> maybeCurrentState.flatMap(_.get(field).collect{case rs:RawObject@unchecked => rs}) match {
        case (None | Some(DNull), None) =>
          value + (field -> RawObject.empty)
        case (Some(DNull), Some(state)) =>
          value + (field -> state.view.mapValues(_ => DNull).toMap)
        case _ =>
          value
    }

//    validateAdditionalProperties(contract, value, maybeCurrentState) ++
    contract._fields.foldLeft(ValidationFailures.empty){
      case (failures, (field, property:BaseContract[DObject]@unchecked with ExpectedObjectProperty[D])) =>
        val newValue = convertExpectedObjectBehaviour(field, value, maybeCurrentState)
        failures ++
        validateProperty(field, property, newValue, maybeCurrentState) ++
        validateContractProperty(field, property, newValue, maybeCurrentState)
      case (failures, (field, property:BaseContract[DObject]@unchecked with Property[D, _])) =>
        failures ++
        validateProperty(field, property, value, maybeCurrentState) ++
        validateContractProperty(field, property, value, maybeCurrentState)
      case (failures, (field, property:ObjectsProperty[D, _]@unchecked)) =>
        failures ++
        validateObjectsProperty(field, property, value)
      case (failures, (field, property:MapObjectsProperty[D, _, _]@unchecked)) =>
        failures ++
        validateMapObjectsProperty(field, property, value, maybeCurrentState)
      case (failures, (field, property)) =>
        failures ++
        validateProperty(field, property, value, maybeCurrentState)
    }
  }

}