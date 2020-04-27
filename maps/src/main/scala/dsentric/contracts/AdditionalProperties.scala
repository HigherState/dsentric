package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric._
import dsentric.failure.{IncorrectTypeBehaviour, StructuralFailure, ValidResult}

sealed trait AdditionalProperties[D <: DObject] {

  def verify(
              rootContract:ContractFor[D],
              path:Path,
              additionalProperties:RawObject
            ):List[StructuralFailure] = ???

  def applicator(rootContract:ContractFor[D],
                               path:Path,
                               additionalProperties:RawObject):ValidResult[RawObject] = ???

}

trait AdditionalPropertiesOps[D <: DObject] {__internal: BaseContract[D] =>

  def openProperties = OpenProperties()
}
//mixin?
class OpenProperties[D <: DObject](val _parent:BaseContract[D]) extends AdditionalProperties[D] {
  def apply(key:String):OpenPropertiesLens[D] =
    new OpenPropertiesLens[D](_parent._path \ key, _parent)
}
class OpenPropertiesLens[D <: DObject](val _path:Path, val _parent:BaseContract[D]) extends MaybeLens[D, Data] {
  private[dsentric] def __incorrectTypeBehaviour: IncorrectTypeBehaviour = ???

  def _codec: DCodec[Data] = ???

  def _root: ContractFor[D] = _parent._root
}


//Default to Closed as there is no point have $additionalProperties in contract if closed.
//case object ClosedProperties extends AdditionalProperties

case class PatternProperties[K](dataOperators:List[DataOperator[Option[Map[K, Data]]]] = Nil)(implicit val keyCodec:StringCodec[K])
  extends AdditionalProperties {
}

case class TypedProperties[K, V](dataOperators:List[DataOperator[Option[Map[K, V]]]] = Nil)(implicit val keyCodec:StringCodec[K], val valueCodec:DCodec[V])
  extends AdditionalProperties

case class ContractProperties[K, D <: DObject](
  contract:ContractFor[D],
  dataOperators:List[DataOperator[Option[Map[K, D]]]] = Nil)(implicit val keyCodec:StringCodec[K])
  extends AdditionalProperties

//
//abstract class NestedProperties[K:StringCodec](dataOperators:List[DataOperator[Option[Map[K, DObject]]]] = Nil)
//  extends BaseContract[DObject]

trait AddProps[K, D <: DObject] extends AdditionalProperties {

}