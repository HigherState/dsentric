package dsentric.contracts

import dsentric._
import dsentric.codecs.{DCodec, DStringCodec}
import dsentric.operators.{DataOperator, Expected, Optional}

sealed trait Property[D <: DObject, T <: Any] extends PropertyLens[D, T] {

  def _dataOperators: List[DataOperator[T]]

  def _codec: DCodec[T]

  def $ : DProjection =
    new DProjection(PathLensOps.pathToMap(_path, 1))
}

sealed trait ValueProperty[D <: DObject, T] extends Property[D, T]
sealed trait ObjectProperty[D <: DObject]   extends Property[D, DObject] with BaseContract[D] {
  def _dataOperators: List[DataOperator[DObject]]
}

private[contracts] trait PropertyResolver[D <: DObject, T] extends Property[D, T] {
  private var __path: Path      = _
  private var __key: String     = _
  @volatile
  private var _bitmap1: Boolean = false

  private[contracts] def __nameOverride: Option[String]

  def _key: String         =
    if (_bitmap1) __key
    else {
      sync()
      __key
    }
  def _path: Path          =
    if (_bitmap1) __path
    else {
      sync()
      __path
    }
  private def sync(): Unit =
    this.synchronized {
      __key = __nameOverride.getOrElse {
        _parent._fields
          .find(p => p._2 == this)
          .getOrElse {
            throw UninitializedFieldError(
              s"Unable to initialize property field from fields: ${_parent._fields.keys.mkString(",")}"
            )
          }
          ._1
      }
      __path = _parent._path \ __key
      _bitmap1 = true
    }
}

final class ExpectedProperty[D <: DObject, T] private[contracts] (
  private[contracts] val __nameOverride: Option[String],
  val _parent: BaseContract[D],
  val _codec: DCodec[T],
  val _dataOperators: List[DataOperator[T] with Expected]
) extends ValueProperty[D, T]
    with PropertyResolver[D, T]
    with ExpectedLens[D, T]

final class MaybeExpectedProperty[D <: DObject, T] private[contracts] (
  private[contracts] val __nameOverride: Option[String],
  val _parent: BaseContract[D],
  val _codec: DCodec[T],
  val _dataOperators: List[DataOperator[T] with Expected]
) extends ValueProperty[D, T]
    with PropertyResolver[D, T]
    with MaybeExpectedLens[D, T]

final class MaybeProperty[D <: DObject, T] private[contracts] (
  private[contracts] val __nameOverride: Option[String],
  val _parent: BaseContract[D],
  val _codec: DCodec[T],
  val _dataOperators: List[DataOperator[T] with Optional]
) extends ValueProperty[D, T]
    with PropertyResolver[D, T]
    with MaybeLens[D, T]

final class DefaultProperty[D <: DObject, T] private[contracts] (
  private[contracts] val __nameOverride: Option[String],
  val _default: T,
  val _parent: BaseContract[D],
  val _codec: DCodec[T],
  val _dataOperators: List[DataOperator[T] with Optional]
) extends ValueProperty[D, T]
    with PropertyResolver[D, T]
    with DefaultLens[D, T]

final class MaybeDefaultProperty[D <: DObject, T] private[contracts] (
  private[contracts] val __nameOverride: Option[String],
  val _default: T,
  val _parent: BaseContract[D],
  val _codec: DCodec[T],
  val _dataOperators: List[DataOperator[T] with Optional]
) extends ValueProperty[D, T]
    with PropertyResolver[D, T]
    with MaybeDefaultLens[D, T]

trait ExpectedObjectProperty[D <: DObject]
    extends ObjectProperty[D]
    with ExpectedObjectPropertyLens[D]
    with ExpectedPropertyObjectOps[D]
    with ExpectedPropertyOps[D]
    with SubContractFor[D] {

  def _dataOperators: List[DataOperator[DObject] with Expected]

}

trait MaybeExpectedObjectProperty[D <: DObject]
    extends ObjectProperty[D]
    with MaybeExpectedObjectPropertyLens[D]
    with MaybeExpectedPropertyObjectOps[D]
    with MaybeExpectedPropertyOps[D]
    with BaseContract[D] {
  override protected def __self: BaseContract[D] = this
  def _dataOperators: List[DataOperator[DObject] with Expected]
}

trait MaybeObjectProperty[D <: DObject]
    extends ObjectProperty[D]
    with MaybeObjectPropertyLens[D]
    with MaybeExpectedPropertyObjectOps[D]
    with MaybeExpectedPropertyOps[D]
    with BaseContract[D] {

  override protected def __self: BaseContract[D] = this
  def _dataOperators: List[DataOperator[DObject] with Optional]
}

case class DynamicProperty[D <: DObject, T](
  _codec: DCodec[T],
  override val _key: String,
  override val _path: Path,
  override val _root: ContractFor[D]
) extends Property[D, T]
    with ApplicativeLens[D, Option[T]]
    with MaybeLens[D, T] {
  private[contracts] def __nameOverride: Option[String] = None

  def _dataOperators: List[DataOperator[T]] = Nil

  def _parent: BaseContract[D] = new ContractFor[D] {}
}

sealed trait AspectProperty[D <: DObject, T <: Any] extends Property[D, T]                                    {
  private var __parent: BaseContract[D] = _

  def _parent: BaseContract[D] = __parent

  lazy val _path: Path =
    _parent._path \ _key

  private[contracts] def __setParent(parent: BaseContract[D]): Unit =
    __parent = parent
}
sealed trait ValueAspectProperty[D <: DObject, T]   extends AspectProperty[D, T] with ValueProperty[D, T]
sealed trait ObjectAspectProperty[D <: DObject]     extends AspectProperty[D, DObject] with ObjectProperty[D] {
  private var __fields: Map[String, AspectProperty[D, _]] = _
  private var __key: String                               = _
  private var _bitmap1: Boolean                           = false

  def _fields: Map[String, AspectProperty[D, _]] = __fields
  def _key: String                               =
    if (_bitmap1) __key
    else {
      _parent._fields.isEmpty
      __key
    }

  private[contracts] def __setFields(fields: Map[String, AspectProperty[D, _]]): Unit =
    __fields = fields
  private[contracts] def __setKey(key: String): Unit = {
    _bitmap1 = true
    __key = key
  }

}

final case class ExpectedAspectProperty[D <: DObject, T] private[contracts] (
  _key: String,
  _codec: DCodec[T],
  _dataOperators: List[DataOperator[T]]
) extends ValueAspectProperty[D, T]
    with ExpectedLens[D, T]

final case class MaybeAspectProperty[D <: DObject, T] private[contracts] (
  _key: String,
  _codec: DCodec[T],
  _dataOperators: List[DataOperator[T] with Optional]
) extends ValueAspectProperty[D, T]
    with MaybeLens[D, T]

final case class DefaultAspectProperty[D <: DObject, T] private[contracts] (
  _key: String,
  _default: T,
  _codec: DCodec[T],
  _dataOperators: List[DataOperator[T] with Optional]
) extends ValueAspectProperty[D, T]
    with DefaultLens[D, T]

sealed class ExpectedObjectAspectProperty[D <: DObject] private[contracts] (
  val _codec: DCodec[DObject],
  val _dataOperators: List[DataOperator[DObject] with Expected]
) extends ObjectAspectProperty[D]
    with ExpectedObjectProperty[D] {}

final class ExpectedObjectAspectPropertyWithAdditional[D <: DObject, Key, Value] private[contracts] (
  override val _codec: DCodec[DObject],
  override val _dataOperators: List[DataOperator[DObject] with Expected],
  val _additionalDataOperators: List[DataOperator[Map[Key, Value]] with Optional],
  val _additionalKeyCodec: DStringCodec[Key],
  val _additionalValueCodec: DCodec[Value]
) extends ExpectedObjectAspectProperty[D](_codec, _dataOperators)
    with AdditionalProperties[Key, Value]

object ExpectedObjectAspectProperty {
//  def apply[D <: DObject](
//    _key: String,
//    _fields: Map[String, AspectProperty[D, _]],
//    _codec: DCodec[DObject],
//    _dataOperators: List[DataOperator[DObject] with Expected]
//  ): ExpectedObjectAspectProperty[D] = {
//    val p = ExpectedObjectAspectProperty[D](_key, _codec, _dataOperators)
//    p.__setFields(_fields)
//    p
//  }

  def apply[D <: DObject](
    objectProperty: ObjectProperty[_],
    _dataOperators: List[DataOperator[DObject] with Expected]
  ): ExpectedObjectAspectProperty[D] = {
    val property =
      objectProperty match {
        case a: AdditionalProperties[_, _] =>
          new ExpectedObjectAspectPropertyWithAdditional[D, Any, Any](
            objectProperty._codec,
            _dataOperators,
            a._additionalDataOperators.asInstanceOf[List[DataOperator[Map[Any, Any]] with Optional]],
            a._additionalKeyCodec.asInstanceOf[DStringCodec[Any]],
            a._additionalValueCodec.asInstanceOf[DCodec[Any]]
          )
        case _                             =>
          new ExpectedObjectAspectProperty[D](objectProperty._codec, _dataOperators)
      }
    property.__setKey(objectProperty._key)
    property
  }

  def apply[D <: DObject](
    objectProperty: ObjectProperty[_],
    _fields: Map[String, AspectProperty[D, _]],
    _dataOperators: List[DataOperator[DObject] with Expected]
  ): ExpectedObjectAspectProperty[D] = {
    val p = apply[D](objectProperty, _dataOperators)
    p.__setFields(_fields)
    p
  }
}

sealed class MaybeObjectAspectProperty[D <: DObject] private[contracts] (
  val _codec: DCodec[DObject],
  val _dataOperators: List[DataOperator[DObject] with Optional]
) extends ObjectAspectProperty[D]
    with MaybeObjectProperty[D]

final class MaybeObjectAspectPropertyWithAdditional[D <: DObject, Key, Value] private[contracts] (
  override val _codec: DCodec[DObject],
  override val _dataOperators: List[DataOperator[DObject] with Optional],
  val _additionalDataOperators: List[DataOperator[Map[Key, Value]] with Optional],
  val _additionalKeyCodec: DStringCodec[Key],
  val _additionalValueCodec: DCodec[Value]
) extends MaybeObjectAspectProperty[D](_codec, _dataOperators)
    with AdditionalProperties[Key, Value]

object MaybeObjectAspectProperty {

  def apply[D <: DObject](
    objectProperty: ObjectProperty[_],
    _dataOperators: List[DataOperator[DObject] with Optional]
  ): MaybeObjectAspectProperty[D] = {
    val property = objectProperty match {
      case a: AdditionalProperties[_, _] =>
        new MaybeObjectAspectPropertyWithAdditional[D, Any, Any](
          objectProperty._codec,
          _dataOperators,
          a._additionalDataOperators.asInstanceOf[List[DataOperator[Map[Any, Any]] with Optional]],
          a._additionalKeyCodec.asInstanceOf[DStringCodec[Any]],
          a._additionalValueCodec.asInstanceOf[DCodec[Any]]
        )
      case _                             =>
        new MaybeObjectAspectProperty[D](objectProperty._codec, _dataOperators)
    }
    property.__setKey(objectProperty._key)
    property
  }

  def apply[D <: DObject](
    objectProperty: ObjectProperty[_],
    _fields: Map[String, AspectProperty[D, _]],
    _dataOperators: List[DataOperator[DObject] with Optional]
  ): MaybeObjectAspectProperty[D] = {
    val p = apply[D](objectProperty, _dataOperators)
    p.__setFields(_fields)
    p
  }
}

object Property {
  def unapply[D <: DObject, T <: Any](p: Property[D, T]): Some[(Path, DCodec[T], List[DataOperator[T]])] =
    Some((p._path, p._codec, p._dataOperators))
}

object ValueProperty {
  def unapply[D <: DObject, T <: Any](p: ValueProperty[D, T]): Some[(Path, DCodec[T], List[DataOperator[T]])] =
    Some((p._path, p._codec, p._dataOperators))
}

object ObjectProperty {
  def unapply[D <: DObject](p: ObjectProperty[D]): Some[(Path, List[DataOperator[DObject]])] =
    Some((p._path, p._dataOperators))
}

object ExpectedProperty {
  def unapply[D <: DObject, T <: Any](
    p: ExpectedProperty[D, T]
  ): Some[(Path, DCodec[T], List[DataOperator[T] with Expected])] =
    Some((p._path, p._codec, p._dataOperators))

  def unapply[D <: DObject, T <: Any](
    p: MaybeExpectedProperty[D, T]
  ): Some[(Path, DCodec[T], List[DataOperator[T] with Expected])] =
    Some((p._path, p._codec, p._dataOperators))
}

object MaybeProperty {
  def unapply[D <: DObject, T <: Any](
    p: MaybeProperty[D, T]
  ): Some[(Path, DCodec[T], List[DataOperator[T] with Optional])] =
    Some((p._path, p._codec, p._dataOperators))
}

object DefaultProperty {
  def unapply[D <: DObject, T <: Any](
    p: DefaultProperty[D, T]
  ): Some[(Path, DCodec[T], T, List[DataOperator[T] with Optional])] =
    Some((p._path, p._codec, p._default, p._dataOperators))

  def unapply[D <: DObject, T <: Any](
    p: MaybeDefaultProperty[D, T]
  ): Some[(Path, DCodec[T], T, List[DataOperator[T] with Optional])] =
    Some((p._path, p._codec, p._default, p._dataOperators))
}

object ExpectedObjectProperty {
  def unapply[D <: DObject](p: ExpectedObjectProperty[D]): Some[(Path, List[DataOperator[DObject] with Expected])] =
    Some((p._path, p._dataOperators))

  def unapply[D <: DObject](
    p: MaybeExpectedObjectProperty[D]
  ): Some[(Path, List[DataOperator[DObject] with Expected])] =
    Some((p._path, p._dataOperators))
}

object MaybeObjectProperty {
  def unapply[D <: DObject](p: MaybeObjectProperty[D]): Some[(Path, List[DataOperator[DObject] with Optional])] =
    Some((p._path, p._dataOperators))
}
