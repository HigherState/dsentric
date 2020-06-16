package dsentric.contracts

import dsentric._
import dsentric.failure.IncorrectTypeBehaviour
import dsentric.operators.DataOperator

sealed trait Property[D <: DObject, T <: Any] extends PropertyLens[D, T] {

  private var __path: Path = _
  private var __key: String = _
  @volatile
  private var _bitmap1:Boolean = false

  private[contracts] def __nameOverride:Option[String]
  def _dataOperators:List[DataOperator[_]]

  @inline
  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour =
    _parent.__incorrectTypeBehaviour

  def _codec: DCodec[T]
  def _parent: BaseContract[D]
  def _root: ContractFor[D] = _parent._root
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
      //Do we need to mark the path as expected
      if (this.isInstanceOf[Expected])
        __path = _parent._path ++ ExpectedPathKey(__key, PathEnd)
      else
        __path = _parent._path \ __key
      _bitmap1 = true
    }
}
sealed trait Expected

class ExpectedProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
   val _parent:BaseContract[D],
   val _codec:DCodec[T],
   val _dataOperators:List[DataOperator[T]])
  extends Property[D, T] with Expected with ExpectedLens[D, T]

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


class ObjectsProperty[D <: DObject, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                          val _contract:ContractFor[T],
                                                          val _parent:BaseContract[D],
                                                          val _valueCodec:DObjectCodec[T],
                                                          val _dataOperators:List[DataOperator[Option[Vector[T]]]]
                                                         ) extends Property[D, Vector[T]] with ObjectsLens[D, T]

class MapObjectsProperty[D <: DObject, K, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                       val _contract:ContractFor[T],
                                                       val _parent:BaseContract[D],
                                                       val _keyCodec:StringCodec[K],
                                                       val _valueCodec:DObjectCodec[T],
                                                       val _dataOperators:List[DataOperator[Option[Map[K, T]]]]
                                                      ) extends Property[D, Map[K, T]] with MapObjectsLens[D, K, T]

sealed trait ExpectedObjectProperty[D <: DObject] extends Property[D, DObject] with Expected with ExpectedObjectLens[D] with SubContractFor[D]
sealed trait MaybeObjectProperty[D <: DObject] extends Property[D, DObject] with MaybeObjectLens[D] with SubContractFor[D]

trait PropertyObjectOps[D <: DObject] { __internal:BaseContract[D] =>

  class \\ private(override private[contracts] val __nameOverride:Option[String],
                   override val _codec:DCodec[DObject],
                   override val _dataOperators:List[DataOperator[DObject]]
                  ) extends ExpectedObjectProperty[D] {

    def this(dataOperators: DataOperator[DObject]*)(implicit codec:DCodec[DObject]) =
      this(None, codec, dataOperators.toList)

    def this(name:String, dataOperators: DataOperator[DObject]*)(implicit codec:DCodec[DObject]) =
      this(Some(name), codec, dataOperators.toList)

    def _parent: BaseContract[D] = __internal
  }

  class \\? private(override private[contracts] val __nameOverride:Option[String],
                    override val _codec:DCodec[DObject],
                    override val _dataOperators:List[DataOperator[Option[DObject]]]
                   ) extends MaybeObjectProperty[D] {

    def this(dataOperators: DataOperator[Option[DObject]]*)(implicit codec:DCodec[DObject]) =
      this(None, codec, dataOperators.toList)

    def this(name:String, dataOperators: DataOperator[Option[DObject]]*)(implicit codec:DCodec[DObject]) =
      this(Some(name), codec, dataOperators.toList)

    def _parent: BaseContract[D] = __internal
  }

  class \\\[K, V] private(override private[contracts] val __nameOverride:Option[String],
                          override val _codec:DCodec[DObject],
                          override val _dataOperators:List[DataOperator[DObject]],
                          val _additionalDataOperators:List[DataOperator[Option[Map[K, V]]]],
                          val _additionalKeyCodec:StringCodec[K],
                          val _additionalValueCodec:DCodec[V]
                         ) extends AdditionalPropertyValues[K, V] with ExpectedObjectProperty[D] {

    def this(dataOperators: DataOperator[DObject]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DCodec[DObject], keyCodec:StringCodec[K], valueCodec:DCodec[V]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(name:String, dataOperators: DataOperator[DObject]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DCodec[DObject], keyCodec:StringCodec[K], valueCodec:DCodec[V]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def _parent: BaseContract[D] = __internal
  }

  class \\\?[K, V] private(override private[contracts] val __nameOverride:Option[String],
                           override val _codec:DCodec[DObject],
                           override val _dataOperators:List[DataOperator[DObject]],
                           val _additionalDataOperators:List[DataOperator[Option[Map[K, V]]]],
                           val _additionalKeyCodec:StringCodec[K],
                           val _additionalValueCodec:DCodec[V]
                          ) extends AdditionalPropertyValues[K, V] with MaybeObjectProperty[D] {

    def this(dataOperators: DataOperator[DObject]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DCodec[DObject], keyCodec:StringCodec[K], valueCodec:DCodec[V]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(name:String, dataOperators: DataOperator[DObject]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DCodec[DObject], keyCodec:StringCodec[K], valueCodec:DCodec[V]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def _parent: BaseContract[D] = __internal
  }

  class \\\\[K, D2 <: DObject] private(override private[contracts] val __nameOverride:Option[String],
                                       val _codec:DCodec[DObject],
                                       val _dataOperators:List[DataOperator[DObject]],
                                       val _additionalDataOperators:List[DataOperator[Option[Map[K, D2]]]],
                                       val _additionalKeyCodec:StringCodec[K],
                                       val _additionalValueCodec:DCodec[D2],
                                       val _additionalContract:ContractFor[D2]
                                      ) extends AdditionalPropertyObjects[K, D2] with ExpectedObjectProperty[D] {

    def this(dataOperators: DataOperator[DObject]*)(contract:ContractFor[D2], additionalPropertyDataOperators:DataOperator[Option[Map[K, D2]]]*)
            (implicit codec:DCodec[DObject], keyCodec:StringCodec[K], valueCodec:DCodec[D2]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec, contract)

    def this(name:String, dataOperators: DataOperator[DObject]*)(contract:ContractFor[D2], additionalPropertyDataOperators:DataOperator[Option[Map[K, D2]]]*)
            (implicit codec:DCodec[DObject], keyCodec:StringCodec[K], valueCodec:DCodec[D2]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec, contract)

    def _parent: BaseContract[D] = __internal
  }

  class \\\\?[K, D2 <: DObject] private(override private[contracts] val __nameOverride:Option[String],
                                        val _codec:DCodec[DObject],
                                        val _dataOperators:List[DataOperator[DObject]],
                                        val _additionalDataOperators:List[DataOperator[Option[Map[K, D2]]]],
                                        val _additionalKeyCodec:StringCodec[K],
                                        val _additionalValueCodec:DCodec[D2],
                                        val _additionalContract:ContractFor[D2]
                                       ) extends AdditionalPropertyObjects[K, D2] with MaybeObjectProperty[D] {

    def this(dataOperators: DataOperator[DObject]*)(contract:ContractFor[D2], additionalPropertyDataOperators:DataOperator[Option[Map[K, D2]]]*)
            (implicit codec:DCodec[DObject], keyCodec:StringCodec[K], valueCodec:DCodec[D2]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec, contract)

    def this(name:String, dataOperators: DataOperator[DObject]*)(contract:ContractFor[D2], additionalPropertyDataOperators:DataOperator[Option[Map[K, D2]]]*)
            (implicit codec:DCodec[DObject], keyCodec:StringCodec[K], valueCodec:DCodec[D2]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec, contract)

    def _parent: BaseContract[D] = __internal
  }
}