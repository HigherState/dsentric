package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric.{DCodec, DObject, Data, StringCodec}

sealed trait AdditionalProperties

case object OpenProperties extends AdditionalProperties

case object CloseProperties extends AdditionalProperties

case class PatternProperties[K:StringCodec](dataOperators:List[DataOperator[Option[Map[K, Data]]]] = Nil)
  extends AdditionalProperties

case class TypedProperties[K:StringCodec, V:DCodec](dataOperators:List[DataOperator[Option[Map[K, V]]]] = Nil)
  extends AdditionalProperties

case class ContractProperties[K:StringCodec, D <: DObject](
  contract:ContractFor[D],
  dataOperators:List[DataOperator[Option[Map[K, D]]]] = Nil)
  extends AdditionalProperties

abstract class NestedProperties[K:StringCodec](dataOperators:List[DataOperator[Option[Map[K, DObject]]]] = Nil)
  extends BaseContract[DObject]