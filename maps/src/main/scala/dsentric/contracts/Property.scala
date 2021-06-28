package dsentric.contracts

import dsentric._
import dsentric.codecs.DCodec
import dsentric.codecs.std.DValueCodecs
import dsentric.failure.{EmptyPropertyFailure, ValidResult}
import dsentric.operators.DataOperator

sealed trait Property[D <: DObject, T <: Any] extends PropertyLens[D, T] {

  private var __path: Path = _
  private var __key: String = _
  @volatile
  private var _bitmap1:Boolean = false

  private[contracts] def __nameOverride:Option[String]
  def _dataOperators:List[DataOperator[_]]

  def _codec: DCodec[T]

  def _key:String =
    if (_bitmap1) __key
    else {
      sync()
      __key
    }
  def _path:Path =
    if (_bitmap1) __path
    else {
      sync()
      __path
    }

  def $:DProjection =
    new DProjection(PathLensOps.pathToMap(_path, 1))


  private def sync(): Unit =
    this.synchronized{
      __key =
        __nameOverride.getOrElse {
          _parent._fields.find(p => p._2 == this).getOrElse{
            throw UninitializedFieldError(s"Unable to initialize property field from fields: ${_parent._fields.keys.mkString(",")}")
          }._1
        }
      __path = _parent._path \ __key
      _bitmap1 = true
    }
}

class ExpectedProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
   val _parent:BaseContract[D],
   val _codec:DCodec[T],
   val _dataOperators:List[DataOperator[T]])
  extends Property[D, T] with ExpectedLens[D, T]

class MaybeExpectedProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
   val _parent:BaseContract[D],
   val _codec:DCodec[T],
   val _dataOperators:List[DataOperator[T]])
    extends Property[D, T] with MaybeExpectedLens[D, T]

class MaybeProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
  val _parent:BaseContract[D],
  val _codec:DCodec[T],
  val _dataOperators:List[DataOperator[Option[T]]])
  extends Property[D, T] with MaybeLens[D, T]

class DefaultProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
   val _default:T,
   val _parent:BaseContract[D],
   val _codec:DCodec[T],
   val _dataOperators:List[DataOperator[Option[T]]])
  extends Property[D, T] with DefaultLens[D, T]

class MaybeDefaultProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
   val _default:T,
   val _parent:BaseContract[D],
   val _codec:DCodec[T],
   val _dataOperators:List[DataOperator[Option[T]]])
    extends Property[D, T] with MaybeDefaultLens[D, T]


trait ExpectedObjectProperty[D <: DObject]
  extends Property[D, DObject]
    with ExpectedObjectPropertyLens[D]
    with ExpectedPropertyObjectOps[D]
    with ExpectedPropertyOps[D]
    with SubContractFor[D] {

  def _dataOperators:List[DataOperator[DObject]]

}

trait MaybeExpectedObjectProperty[D <: DObject]
  extends Property[D, DObject]
    with MaybeExpectedObjectPropertyLens[D]
    with MaybeExpectedPropertyObjectOps[D]
    with MaybeExpectedPropertyOps[D]
    with BaseContract[D] {
  override protected def __self: BaseContract[D] = this
  def _dataOperators:List[DataOperator[DObject]]
}

trait MaybeObjectProperty[D <: DObject]
  extends Property[D, DObject]
    with MaybeObjectPropertyLens[D]
    with MaybeExpectedPropertyObjectOps[D]
    with MaybeExpectedPropertyOps[D]
    with BaseContract[D] {

  override protected def __self: BaseContract[D] = this
  def _dataOperators:List[DataOperator[Option[DObject]]]
}

private[dsentric] case class EmptyProperty[D <: DObject, T](_codec:DCodec[T], override val _key: String, override val _path:Path, override val _root:ContractFor[D]) extends Property[D, T] {
  private[contracts] def __nameOverride: Option[String] = None

  def _dataOperators: List[DataOperator[_]] = Nil

  def _parent: BaseContract[D] = new ContractFor[D]  {}
  private[contracts] def __get(base: RawObject, dropBadTypes: Boolean): MaybeAvailable[T] =
    Failed(EmptyPropertyFailure)
  private[contracts] def __apply(rawObject: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] =
    ValidResult.failure(EmptyPropertyFailure)
  private[contracts] def __reduce(obj: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] =
    ValidResult.failure(EmptyPropertyFailure)
  private[contracts] def __reduceDelta(deltaObject: RawObject, currentObject: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] =
    ValidResult.failure(EmptyPropertyFailure)
}
