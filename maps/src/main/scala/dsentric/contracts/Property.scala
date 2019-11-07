package dsentric.contracts

import dsentric._
import dsentric.failure.{IncorrectTypeBehaviour, ValidResult}
import dsentric.operators.{DataOperator, Validators}

sealed trait Property[D <: DObject, T <: Any] extends PropertyLens[D, T] {

  private var __path: Path = _
  private var __localPath: Path = _
  @volatile
  private var _bitmap1:Boolean = false

  private[contracts] def __nameOverride:Option[String]
  def _dataOperators:Seq[DataOperator[_]]

  @inline
  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour =
    _parent.__incorrectTypeBehaviour

  def _codec: DCodec[T]
  def _parent: BaseContract[D]
  def _root: ContractFor[D] = _parent._root
  def _path:Path =
    if (_bitmap1) __path
    else {
      sync()
      __path
    }


  def _localPath:Path =
    if (_bitmap1) __localPath
    else {
      sync()
      __localPath
    }


  def $:DProjection =
    new DProjection(PathLensOps.pathToMap(_path, 1))


  private def sync(): Unit =
    this.synchronized{
      __localPath =
        Path(__nameOverride.getOrElse {
          _parent._fields.find(p => p._2 == this).getOrElse{
            throw UninitializedFieldError(s"Unable to initialize property field from fields: ${_parent._fields.keys.mkString(",")}")
          }._1
        })
      __path = _parent._path ++ __localPath
      _bitmap1 = true
    }
}

class ExpectedProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
   val _parent:BaseContract[D],
   val _codec:DCodec[T],
   val _dataOperators:Seq[DataOperator[T]])
  extends Property[D, T] with ExpectedLens[D, T]

class MaybeProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
  val _parent:BaseContract[D],
  val _codec:DCodec[T],
  val _dataOperators:Seq[DataOperator[Option[T]]])
  extends Property[D, T] with MaybeLens[D, T]

class DefaultProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
   val _default:T,
   val _parent:BaseContract[D],
   val _codec:DCodec[T],
   val _dataOperators:Seq[DataOperator[Option[T]]])
  extends Property[D, T] with DefaultLens[D, T]


class ObjectsProperty[D <: DObject, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                          val _contract:ContractFor[T],
                                                          val _parent:BaseContract[D],
                                                          val _codec:DCodec[Vector[T]],
                                                          val _dataOperators:Seq[DataOperator[Option[Vector[T]]]]
                                                         ) extends Property[D, Vector[T]] with ObjectsLens[D, T] {
}

class MapObjectsProperty[D <: DObject, K, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                       val _contract:ContractFor[T],
                                                       val _parent:BaseContract[D],
                                                       val _codec:DCodec[Map[K, T]],
                                                       val _keyCodec:StringCodec[K],
                                                       val _dataOperators:Seq[DataOperator[Option[Map[K, T]]]]
                                                      ) extends Property[D, Map[K, T]] with MapObjectsLens[D, K, T]

trait PropertyObjectOps[D <: DObject]  { __internal:BaseContract[D] =>

  class \\ private(override private[contracts] val __nameOverride:Option[String],
                   override val _codec:DCodec[DObject],
                   override val _dataOperators:Seq[DataOperator[DObject]]
                  ) extends Property[D, DObject] with ExpectedObjectLens[D] with SubContractFor[D] {

    def this(dataOperators: DataOperator[DObject]*)(implicit codec:DCodec[DObject]) =
      this(None, codec, Validators.required +: dataOperators)
    def this(name:String, dataOperators: DataOperator[DObject]*)(implicit codec:DCodec[DObject]) =
      this(Some(name), codec, Validators.required +: dataOperators)

    def _parent: BaseContract[D] = __internal
  }

  class \\? private(override private[contracts] val __nameOverride:Option[String],
                    override val _codec:DCodec[DObject],
                    override val _dataOperators:Seq[DataOperator[Option[DObject]]]
                   ) extends Property[D, DObject] with MaybeObjectLens[D] with SubContractFor[D] {

    def this(dataOperators: DataOperator[Option[DObject]]*)(implicit codec:DCodec[DObject]) =
      this(None, codec, dataOperators)
    def this(name:String, dataOperators: DataOperator[Option[DObject]]*)(implicit codec:DCodec[DObject]) =
      this(Some(name), codec, dataOperators)

    def _parent: BaseContract[D] = __internal
  }

}