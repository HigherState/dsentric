package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric._
import dsentric.failure.{StructuralFailure, ValidResult}

sealed trait AdditionalProperties {

  def verify[D <: DObject](
              rootContract:ContractFor[D],
              path:Path,
              additionalProperties:RawObject
            ):List[StructuralFailure] = ???

  def applicator[D <: DObject](rootContract:ContractFor[D],
                               path:Path,
                               additionalProperties:RawObject):ValidResult[RawObject] = ???

}

case object OpenProperties extends AdditionalProperties

case object ClosedProperties extends AdditionalProperties

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