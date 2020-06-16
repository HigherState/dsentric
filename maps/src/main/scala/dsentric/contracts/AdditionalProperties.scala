package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric._

/**
 * Marker trait to identify that the Contract is closed for any additionalProperties
 * By default contracts ignore any additional properties
 */
abstract class AdditionalPropertyClosed

/**
 * Defines additional properties as conforming to the specified Key [K] Value [V] types.
 * @param _additionalDataOperators
 * @param _additionalKeyCodec
 * @param _additionalValueCodec
 * @tparam K
 * @tparam V
 */
abstract class AdditionalPropertyValues[K, V]
  (val _additionalDataOperators:DataOperator[Option[Map[K, V]]]*)
  (implicit val _additionalKeyCodec:StringCodec[K], val _additionalValueCodec:DCodec[V])

/**
 * Defines additional properties as conforming to the specified Key [K] Value Contract [D2] types.
 * @param _additionalContract
 * @param _additionalDataOperators
 * @param _additionalKeyCodec
 * @param _additionalValueCodec
 * @tparam K
 * @tparam D
 */
abstract class AdditionalPropertyObjects[K, D <: DObject]
  (val _additionalContract:ContractFor[D], val _additionalDataOperators:DataOperator[Option[Map[K, D]]]*)
  (implicit val _additionalKeyCodec:StringCodec[K], val _additionalValueCodec:DCodec[D])