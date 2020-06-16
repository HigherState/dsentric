package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric._
import dsentric.failure.ValidResult

/**
 * Marker trait to identify that the Contract is closed for any additionalProperties
 * By default contracts ignore any additional properties
 */
trait AdditionalProperties extends BaseContractAux {

  /**
   * Returns failure if the key is in the contract definition.
   * @param key
   * @param d
   * @return
   */
  final def $get(key:String)(d:AuxD):ValidResult[Option[Data]] = ???

  /**
   * Returns failure if the key is in the contract definition.
   * @param d
   * @return
   */
  final def $add(d:(String, Data)):ValidPathSetter[AuxD] = ???

  final def $addMany(d:Iterable[(String, Data)]):ValidPathSetter[AuxD] = ???

  final def $drop(key:String):ValidPathSetter[AuxD] = ???

  final def $setOrDrop(key:String, value:Option[Data]):PathSetter[AuxD] = ???

  final def $modify(key:String, f:Option[Data] => Data):ValidPathSetter[AuxD] = ???

  final def $modifyOrDrop(key:String, f:Option[Data] => Option[Data]):ValidPathSetter[AuxD] = ???

  final def $transform(f:Map[String, Data] => Map[String, Data]):ValidPathSetter[AuxD] = ???

  final def $clear:PathSetter[AuxD] = ???

  final def $dynamic[T](field:String)(implicit codec:DCodec[T]):MaybeProperty[AuxD, T] =
    new MaybeProperty[AuxD, T](Some(field), this.asInstanceOf[BaseContract[AuxD]], codec, List.empty)
}

/**
 * Defines additional properties as conforming to the specified Key [K] Value [V] types.
 * @tparam K
 * @tparam V
 */
trait AdditionalPropertyValues[K, V] extends BaseContractAux {
  def _additionalDataOperators:List[DataOperator[Option[Map[K, V]]]]
  def _additionalKeyCodec:StringCodec[K]
  def _additionalValueCodec:DCodec[V]

  final def $get(key:K)(d:AuxD):ValidResult[Option[V]] = ???

  final def $add(d:(K, V)):ValidPathSetter[AuxD] = ???

  final def $addMany(d:Iterable[(K, V)]):ValidPathSetter[AuxD] = ???

  final def $drop(key:K):ValidPathSetter[AuxD] = ???

  final def $setOrDrop(key:K, value:Option[V]):PathSetter[AuxD] = ???

  final def $modify(key:K, f:V => V):ValidPathSetter[AuxD] = ???

  final def $modifyOrDrop(key:K, f:Option[V] => Option[V]):ValidPathSetter[AuxD] = ???

  final def $transform(f:Map[K, V] => Map[K, V]):ValidPathSetter[AuxD] = ???

  final def $clear:PathSetter[AuxD] = ???
}

/**
 * Defines additional properties as conforming to the specified Key [K] Value Contract [D2] types.
 * @tparam K
 * @tparam D
 */
trait AdditionalPropertyObjects[K, D <: DObject] extends BaseContractAux {
  def _additionalDataOperators:List[DataOperator[Option[Map[K, D]]]]
  def _additionalKeyCodec:StringCodec[K]
  def _additionalValueCodec:DCodec[D]
  def _additionalContract:ContractFor[D]

  final def $get(key:K)(d:AuxD):ValidResult[Option[D]] = ???

  final def $add(d:(K, D)):ValidPathSetter[AuxD] = ???

  final def $addMany(d:Iterable[(K, D)]):ValidPathSetter[AuxD] = ???

  final def $drop(key:K):ValidPathSetter[AuxD] = ???

  final def $setOrDrop(key:K, value:Option[D]):PathSetter[AuxD] = ???

  final def $modify(key:K, f: D => D):ValidPathSetter[AuxD] = ???

  final def $modifyOrDrop(key:K, f:Option[D] => Option[D]):ValidPathSetter[AuxD] = ???

  final def $transform(f:Map[K, D] => Map[K, D]):ValidPathSetter[AuxD] = ???

  final def $clear:PathSetter[AuxD] = ???
}


/**
 * Abstract class for ease of use with Contract, ContractFor, SubContract and SubContractFor
 */

abstract class DefinedAdditionalPropertyValues[K, V](
  val _additionalDataOperators:List[DataOperator[Option[Map[K, V]]]],
  val _additionalKeyCodec:StringCodec[K],
  val _additionalValueCodec:DCodec[V]) extends AdditionalPropertyValues[K, V]{

  def this(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
          (implicit keyCodec:StringCodec[K], valueCodec:DCodec[V]) =
    this(additionalPropertyDataOperators.toList, keyCodec, valueCodec)
}

abstract class DefinedAdditionalPropertyObjects[K, D <: DObject](
  val _additionalDataOperators:List[DataOperator[Option[Map[K, D]]]],
  val _additionalKeyCodec:StringCodec[K],
  val _additionalValueCodec:DCodec[D],
  val _additionalContract:ContractFor[D]) extends AdditionalPropertyObjects[K, D]{

  def this(contract:ContractFor[D], additionalPropertyDataOperators:DataOperator[Option[Map[K, D]]]*)
          (implicit keyCodec:StringCodec[K], valueCodec:DCodec[D]) =
    this(additionalPropertyDataOperators.toList, keyCodec, valueCodec, contract)
}
