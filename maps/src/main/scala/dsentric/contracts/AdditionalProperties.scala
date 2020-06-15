package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric._
import dsentric.failure.{IncorrectTypeBehaviour, StructuralFailure, ValidResult}

/**
 * Marker trait to identify that the Contract is closed for any additionalProperties
 * By default contracts ignore any additional properties
 */
trait Closed

/**
 * Trait to identify that the contract has defined additional properties
 */


sealed trait AdditionalProperties[D <: DObject]

final case class OpenForAdditionalProperties[D <: DObject]()
  extends AdditionalProperties[D]

final case class ClosedForAdditionalProperties[D <: DObject](_root: ContractFor[D], _path:Path)
  extends AdditionalProperties[D]


sealed trait DefinedForAdditionalProperties

final case class ValuesForAdditionalProperties[D <: DObject, K, V](
  _root: ContractFor[D],
  _path:Path,
  _keyCodec:StringCodec[K],
  _valueCode:DCodec[V],
  __incorrectTypeBehaviour:IncorrectTypeBehaviour,
  dataOperators:List[DataOperator[Option[Map[K, V]]]])
  extends DefinedForAdditionalProperties with AdditionalProperties[D]

final case class ObjectsForAdditionalProperties[D <: DObject, K, D2 <: DObject](
  _root: ContractFor[D],
  _path:Path,
  _keyCodec:StringCodec[K],
  _valueCode:DCodec[D2],
  contract:ContractFor[D2],
  __incorrectTypeBehaviour:IncorrectTypeBehaviour,
  dataOperators:List[DataOperator[Option[Map[K, D2]]]])
  extends DefinedForAdditionalProperties with AdditionalProperties[D]

