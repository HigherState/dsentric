package dsentric.contracts

import dsentric.operators.{DataOperator, Expected, Optional}
import dsentric._
import dsentric.codecs.{DCodec, DStringCodec}

trait PropertyOps[D <: DObject] {

  protected def __self: BaseContract[D] = null

  def \?[T](implicit codec: DCodec[T]): MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, __self, codec, Nil)

  def \?[T](dataOperators: DataOperator[T] with Optional*)(implicit codec: DCodec[T]): MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, __self, codec, dataOperators.toList)

  def \?[T](name: String, dataOperators: DataOperator[T] with Optional*)(implicit codec: DCodec[T]): MaybeProperty[D, T] =
    new MaybeProperty[D, T](Some(name), __self, codec, dataOperators.toList)

}

trait ExpectedPropertyOps[D <: DObject] extends PropertyOps[D] {

  def \[T](implicit codec: DCodec[T]): ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, Nil)

  def \[T](dataOperators: DataOperator[T] with Expected*)(implicit codec: DCodec[T]): ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, dataOperators.toList)

  def \[T](name: String, dataOperators: DataOperator[T] with Expected*)(implicit
    codec: DCodec[T]
  ): ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](Some(name), __self, codec, dataOperators.toList)

  def \![T](default: T, dataOperators: DataOperator[T] with Optional*)(implicit codec: DCodec[T]): DefaultProperty[D, T] =
    new DefaultProperty[D, T](None, default, __self, codec, dataOperators.toList)

  def \![T](name: String, default: T, dataOperators: DataOperator[T] with Optional*)(implicit
    codec: DCodec[T]
  ): DefaultProperty[D, T] =
    new DefaultProperty[D, T](Some(name), default: T, __self, codec, dataOperators.toList)

}

trait MaybeExpectedPropertyOps[D <: DObject] extends PropertyOps[D] {
  def \[T](implicit codec: DCodec[T]): MaybeExpectedProperty[D, T] =
    new MaybeExpectedProperty[D, T](None, __self, codec, Nil)

  def \[T](dataOperators: DataOperator[T] with Expected*)(implicit codec: DCodec[T]): MaybeExpectedProperty[D, T] =
    new MaybeExpectedProperty[D, T](None, __self, codec, dataOperators.toList)

  def \[T](name: String, dataOperators: DataOperator[T] with Expected*)(implicit
    codec: DCodec[T]
  ): MaybeExpectedProperty[D, T] =
    new MaybeExpectedProperty[D, T](Some(name), __self, codec, dataOperators.toList)

  def \![T](default: T, dataOperators: DataOperator[T] with Optional*)(implicit
    codec: DCodec[T]
  ): MaybeDefaultProperty[D, T] =
    new MaybeDefaultProperty[D, T](None, default, __self, codec, dataOperators.toList)

  def \![T](name: String, default: T, dataOperators: DataOperator[T] with Optional*)(implicit
    codec: DCodec[T]
  ): MaybeDefaultProperty[D, T] =
    new MaybeDefaultProperty[D, T](Some(name), default: T, __self, codec, dataOperators.toList)

}

trait PropertyObjectOps[D <: DObject] { __internal: BaseContract[D] =>

  /**
   * Maybe nested object
   * @param __nameOverride
   * @param _codec
   * @param _dataOperators
   */
  abstract class \\? private (
    override private[contracts] val __nameOverride: Option[String],
    override val _codec: DCodec[DObject],
    override val _dataOperators: List[DataOperator[DObject] with Optional]
  ) extends MaybeObjectProperty[D]
      with FieldResolver[D]
      with PropertyResolver[D, DObject] {

    def this(dataOperators: DataOperator[DObject] with Optional*)(implicit codec: DCodec[DObject]) =
      this(None, codec, dataOperators.toList)

    def this(name: String, dataOperators: DataOperator[DObject] with Optional*)(implicit codec: DCodec[DObject]) =
      this(Some(name), codec, dataOperators.toList)

    def _parent: BaseContract[D] = __internal
  }

  /**
   * Maybe nested object with Constrained additional properties
   * @param __nameOverride
   * @param _codec
   * @param _dataOperators
   * @param _additionalDataOperators
   * @param _additionalKeyCodec
   * @param _additionalValueCodec
   * @tparam K
   * @tparam V
   */
  abstract class \\\?[K, V] private (
    override private[contracts] val __nameOverride: Option[String],
    override val _codec: DCodec[DObject],
    override val _dataOperators: List[DataOperator[DObject] with Optional],
    val _additionalDataOperators: List[DataOperator[Map[K, V]] with Optional],
    val _additionalKeyCodec: DStringCodec[K],
    val _additionalValueCodec: DCodec[V]
  ) extends AdditionalProperties[K, V]
      with MaybeObjectProperty[D]
      with FieldResolver[D]
      with PropertyResolver[D, DObject] {

    def this(dataOperators: DataOperator[DObject] with Optional*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K], valueCodec: DCodec[V]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(name: String, dataOperators: DataOperator[DObject] with Optional*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K], valueCodec: DCodec[V]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec: DCodec[V], dataOperators: DataOperator[DObject] with Optional*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec: DCodec[V], name: String, dataOperators: DataOperator[DObject] with Optional*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def _parent: BaseContract[D] = __internal
  }

}

trait ExpectedPropertyObjectOps[D <: DObject] extends PropertyObjectOps[D] { __internal: BaseContract[D] =>

  /**
   * Expected nested object
   * @param __nameOverride
   * @param _codec
   * @param _dataOperators
   */
  abstract class \\ private (
    override private[contracts] val __nameOverride: Option[String],
    override val _codec: DCodec[DObject],
    override val _dataOperators: List[DataOperator[DObject] with Expected]
  ) extends ExpectedObjectProperty[D]
      with FieldResolver[D]
      with PropertyResolver[D, DObject] {

    def this(dataOperators: DataOperator[DObject] with Expected*)(implicit codec: DCodec[DObject]) =
      this(None, codec, dataOperators.toList)

    def this(name: String, dataOperators: DataOperator[DObject] with Expected*)(implicit codec: DCodec[DObject]) =
      this(Some(name), codec, dataOperators.toList)

    def _parent: BaseContract[D] = __internal
  }

  /**
   * Expected nested object with Constrained additional properties
   * @param __nameOverride
   * @param _codec
   * @param _dataOperators
   * @param _additionalDataOperators
   * @param _additionalKeyCodec
   * @param _additionalValueCodec
   * @tparam K
   * @tparam V
   */
  abstract class \\\[K, V] private (
    override private[contracts] val __nameOverride: Option[String],
    override val _codec: DCodec[DObject],
    override val _dataOperators: List[DataOperator[DObject] with Expected],
    val _additionalDataOperators: List[DataOperator[Map[K, V]] with Optional],
    val _additionalKeyCodec: DStringCodec[K],
    val _additionalValueCodec: DCodec[V]
  ) extends AdditionalProperties[K, V]
      with ExpectedObjectProperty[D]
      with FieldResolver[D]
      with PropertyResolver[D, DObject] {

    def this(dataOperators: DataOperator[DObject] with Expected*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K], valueCodec: DCodec[V]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(name: String, dataOperators: DataOperator[DObject] with Expected*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K], valueCodec: DCodec[V]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec: DCodec[V], dataOperators: DataOperator[DObject] with Expected*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec: DCodec[V], name: String, dataOperators: DataOperator[DObject] with Expected*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def _parent: BaseContract[D] = __internal
  }
}

trait MaybeExpectedPropertyObjectOps[D <: DObject] extends PropertyObjectOps[D] { __internal: BaseContract[D] =>

  /**
   * Expected nested object
   * @param __nameOverride
   * @param _codec
   * @param _dataOperators
   */
  abstract class \\ private (
    override private[contracts] val __nameOverride: Option[String],
    override val _codec: DCodec[DObject],
    override val _dataOperators: List[DataOperator[DObject] with Expected]
  ) extends MaybeExpectedObjectProperty[D]
      with FieldResolver[D]
      with PropertyResolver[D, DObject] {

    def this(dataOperators: DataOperator[DObject] with Expected*)(implicit codec: DCodec[DObject]) =
      this(None, codec, dataOperators.toList)

    def this(name: String, dataOperators: DataOperator[DObject] with Expected*)(implicit codec: DCodec[DObject]) =
      this(Some(name), codec, dataOperators.toList)

    def _parent: BaseContract[D] = __internal
  }

  /**
   * Expected nested object with Constrained additional properties
   * @param __nameOverride
   * @param _codec
   * @param _dataOperators
   * @param _additionalDataOperators
   * @param _additionalKeyCodec
   * @param _additionalValueCodec
   * @tparam K
   * @tparam V
   */
  abstract class \\\[K, V] private (
    override private[contracts] val __nameOverride: Option[String],
    override val _codec: DCodec[DObject],
    override val _dataOperators: List[DataOperator[DObject] with Expected],
    val _additionalDataOperators: List[DataOperator[Map[K, V]] with Optional],
    val _additionalKeyCodec: DStringCodec[K],
    val _additionalValueCodec: DCodec[V]
  ) extends AdditionalProperties[K, V]
      with MaybeExpectedObjectProperty[D]
      with FieldResolver[D]
      with PropertyResolver[D, DObject] {

    def this(dataOperators: DataOperator[DObject] with Expected*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K], valueCodec: DCodec[V]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(name: String, dataOperators: DataOperator[DObject] with Expected*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K], valueCodec: DCodec[V]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec: DCodec[V], dataOperators: DataOperator[DObject] with Expected*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec: DCodec[V], name: String, dataOperators: DataOperator[DObject] with Expected*)(
      additionalPropertyDataOperators: DataOperator[Map[K, V]] with Optional*
    )(implicit codec: DCodec[DObject], keyCodec: DStringCodec[K]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def _parent: BaseContract[D] = __internal
  }
}
