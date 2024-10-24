package dsentric.contracts

import dsentric.operators.{DataOperator, Optional}
import dsentric._
import dsentric.codecs.std.DValueCodecs
import dsentric.codecs.{DCodec, DContractCodec, DStringCodec}
import dsentric.failure.{ContractFieldFailure, IncorrectTypeFailure, ValidResult}

/**
 * Marker trait to identify that the Contract is closed for any additionalProperties
 * By default contracts ignore any additional properties
 *
 * When Adding values, it will assume that the value being added is valid, IE in the case
 * of a Contract entity, it will not validate the Contract values
 */
trait AdditionalProperties[Key, Value] extends BaseContractAux {
  import cats.implicits._

  def _additionalDataOperators: List[DataOperator[Map[Key, Value]] & Optional]
  def _additionalKeyCodec: DStringCodec[Key]
  def _additionalValueCodec: DCodec[Value]

  private[contracts] def __get(data: RawObject, key: String, dropBadTypes: Boolean): MaybeAvailable[Value] =
    TraversalOps
      .maybeTraverseRaw(data, this, dropBadTypes)
      .flatMap { rawObject =>
        rawObject.get(key) match {
          case Some(value) =>
            _additionalValueCodec.unapply(value) match {
              case None if dropBadTypes =>
                NotFound
              case None                 =>
                Failed(IncorrectTypeFailure(_root, _path \ key, _additionalValueCodec, value))
              case Some(t)              =>
                Found(t)
            }
          case None        =>
            NotFound
        }
      }

  /**
   * Returns failure if the key is in the contract definition.
   * @param key
   * @param d
   * @return
   */
  final def $get(key: Key, dropBadTypes: Boolean)(d: AuxD): ValidResult[Option[Value]] = {
    val keyString = _additionalKeyCodec.apply(key)
    _checkKeyClash(keyString) >>
    __get(d.value, keyString, dropBadTypes).toValidOption
  }

  final def $get(key: Key)(d: AuxD): ValidResult[Option[Value]] =
    $get(key, false)(d)

  /**
   * Returns failure if the key is in the contract definition.
   * @param d
   * @return
   */
  final def $put(d: (Key, Value)): ValidPathSetter[AuxD] = {
    val keyString = _additionalKeyCodec.apply(d._1)
    ValidValueSetter(_path \ keyString, _checkKeyClash(keyString).map(_ => _additionalValueCodec(d._2)))
  }

  /**
   * Returns failure if any key is in the contract definition.
   * @param d
   * @return
   */
  final def $putMany(d: Iterable[(Key, Value)]): ValidPathSetter[AuxD] = {
    def map(rawObject: RawObject): ValidResult[RawObject] = {
      val mapped = d.map(p => _additionalKeyCodec(p._1) -> _additionalValueCodec(p._2))
      mapped
        .filter(p => _fields.contains(p._1))
        .toList
        .map(p => ContractFieldFailure(this._root, this._path, p._1)) match {
        case head :: tail =>
          ValidResult.failure(head, tail)
        case Nil          =>
          ValidResult.success(rawObject ++ mapped)
      }
    }
    RawTraversedModifyValidSetter[AuxD](d => TraversalOps.traverseRaw(d, this, true), map, _path)
  }

  /**
   * Returns failure if the key is in the contract definition.
   * @param d
   * @return
   */
  final def $drop(key: Key): ValidPathSetter[AuxD] = {
    val keyString = _additionalKeyCodec.apply(key)
    ValidDrop(_checkKeyClash(keyString).map(_ => _path \ keyString))
  }

  final def $dropAll: PathSetter[AuxD] =
    RawTraversedIgnoredModifySetter[AuxD](r => r.view.filterKeys(_fields.contains).toMap, _path)

  final def $setOrDrop(key: Key, value: Option[Value]): ValidPathSetter[AuxD] = {
    val keyString = _additionalKeyCodec.apply(key)
    value match {
      case None    =>
        ValidDrop(_checkKeyClash(keyString).map(_ => _path \ keyString))
      case Some(v) =>
        ValidValueSetter(_path \ keyString, _checkKeyClash(keyString).map(_ => _additionalValueCodec(v)))
    }
  }
  final def $modify(key: Key, f: Value => Value, dropBadTypes: Boolean): ValidPathSetter[AuxD] = {
    val keyString = _additionalKeyCodec.apply(key)
    ModifySetter(
      d => _checkKeyClash(keyString) >> __get(d, keyString, dropBadTypes).toValidOption,
      f,
      _additionalValueCodec,
      _path \ keyString
    )
  }

  final def $modify(key: Key, f: Value => Value): ValidPathSetter[AuxD] =
    $modify(key, f, false)

  /**
   * IMPORTANT Does not set if maybe path is empty
   * @param key
   * @param f
   * @return
   */
  final def $modifyOrSet(key: Key, f: Option[Value] => Value): ValidPathSetter[AuxD] = ???

  final def $modifyOrDrop(key: String, f: Option[Data] => Option[Data]): ValidPathSetter[AuxD] = ???

  final def $transform(f: Map[String, Data] => Map[String, Data]): ValidPathSetter[AuxD] = ???

  final def $dynamic[T](field: String)(implicit codec: DCodec[T]): MaybeProperty[AuxD, T] =
    new MaybeProperty[AuxD, T](Some(field), this.asInstanceOf[BaseContract[AuxD]], codec, List.empty)

  private def _checkKeyClash(key: String): ValidResult[Unit] =
    if (_fields.contains(key))
      ValidResult.failure(ContractFieldFailure(this._root, this._path, key), Nil)
    else
      ValidResult.unit
}

trait Open extends AdditionalProperties[String, Data] {
  def _additionalDataOperators: List[DataOperator[Map[String, Data]] & Optional] = Nil

  def _additionalKeyCodec: DStringCodec[String] =
    DValueCodecs.stringCodec

  def _additionalValueCodec: DCodec[Data] =
    DValueCodecs.dataCodec
}

/**
 * Abstract class for ease of use with Contract, ContractFor, SubContract and SubContractFor
 */

abstract class DefinedAdditionalProperties[K, V](
  val _additionalDataOperators: List[DataOperator[Map[K, V]] & Optional],
  val _additionalKeyCodec: DStringCodec[K],
  val _additionalValueCodec: DCodec[V]
) extends AdditionalProperties[K, V] {

  def this(
    additionalPropertyDataOperators: DataOperator[Map[K, V]] & Optional*
  )(implicit keyCodec: DStringCodec[K], valueCodec: DCodec[V]) =
    this(additionalPropertyDataOperators.toList, keyCodec, valueCodec)

  def this(contract: Contract, additionalPropertyDataOperators: DataOperator[Map[K, V]] & Optional*)(implicit
    keyCodec: DStringCodec[K],
    evidence: V =:= DObject
  ) =
    this(additionalPropertyDataOperators.toList, keyCodec, DContractCodec(contract).asInstanceOf[DCodec[V]])

  def this(valueCodec: DCodec[V], additionalPropertyDataOperators: DataOperator[Map[K, V]] & Optional*)(implicit
    keyCodec: DStringCodec[K]
  ) =
    this(additionalPropertyDataOperators.toList, keyCodec, valueCodec)
}

/**
 * By default Contracts and nested objects are closed, this trait can be set on abstract properties to prevent
 * Them being inherited open
 */
trait Closed {
  def _additionalDataOperators: Nil.type = Nil
}
