package dsentric.contracts

import dsentric._
import dsentric.codecs.{DCodec, DObjectCodec, DStringCodec}
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
      //Do we need to mark the path as expected
      if (this.isInstanceOf[Expected])
        __path = _parent._path ++ PathKey(__key, PathEnd)
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


sealed trait ExpectedObjectProperty[D <: DObject] extends Property[D, DObject] with Expected with ExpectedObjectPropertyLens[D] with SubContractFor[D] {

  def _dataOperators:List[DataOperator[DObject]]

//  override private[contracts] def __map[D2 >: D](f: (Property[D, _], RawObject) => RawObject)(obj: RawObject): RawObject = {
//    val local = super.__map(f)(obj)
//    val nested = __incorrectTypeBehaviour.
//    _fields.foldLeft(nested){(a, p) =>
//      f(p._2.asInstanceOf[Property[D, Any]], a)
//    }
//  }

}
sealed trait MaybeObjectProperty[D <: DObject] extends Property[D, DObject] with MaybeObjectPropertyLens[D] with SubContractFor[D] {
  def _dataOperators:List[DataOperator[Option[DObject]]]
}

trait PropertyObjectOps[D <: DObject] { __internal:BaseContract[D] =>

  /**
   * Expected nested object
   * @param __nameOverride
   * @param _codec
   * @param _dataOperators
   */
  class \\ private(override private[contracts] val __nameOverride:Option[String],
                   override val _codec:DObjectCodec[DObject],
                   override val _dataOperators:List[DataOperator[DObject]]
                  ) extends ExpectedObjectProperty[D] {

    def this(dataOperators: DataOperator[DObject]*)(implicit codec:DObjectCodec[DObject]) =
      this(None, codec, dataOperators.toList)

    def this(name:String, dataOperators: DataOperator[DObject]*)(implicit codec:DObjectCodec[DObject]) =
      this(Some(name), codec, dataOperators.toList)

    def _parent: BaseContract[D] = __internal
  }

  /**
   * Maybe nested object
   * @param __nameOverride
   * @param _codec
   * @param _dataOperators
   */
  class \\? private(override private[contracts] val __nameOverride:Option[String],
                    override val _codec:DObjectCodec[DObject],
                    override val _dataOperators:List[DataOperator[Option[DObject]]]
                   ) extends MaybeObjectProperty[D] {

    def this(dataOperators: DataOperator[Option[DObject]]*)(implicit codec:DObjectCodec[DObject]) =
      this(None, codec, dataOperators.toList)

    def this(name:String, dataOperators: DataOperator[Option[DObject]]*)(implicit codec:DObjectCodec[DObject]) =
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
  class \\\[K, V] private(override private[contracts] val __nameOverride:Option[String],
                          override val _codec:DObjectCodec[DObject],
                          override val _dataOperators:List[DataOperator[DObject]],
                          val _additionalDataOperators:List[DataOperator[Option[Map[K, V]]]],
                          val _additionalKeyCodec:DStringCodec[K],
                          val _additionalValueCodec:DCodec[V]
                         ) extends AdditionalProperties[K, V] with ExpectedObjectProperty[D] {

    def this(dataOperators: DataOperator[DObject]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DObjectCodec[DObject], keyCodec:DStringCodec[K], valueCodec:DCodec[V]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(name:String, dataOperators: DataOperator[DObject]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DObjectCodec[DObject], keyCodec:DStringCodec[K], valueCodec:DCodec[V]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec:DCodec[V], dataOperators: DataOperator[DObject]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DObjectCodec[DObject], keyCodec:DStringCodec[K]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec:DCodec[V], name:String, dataOperators: DataOperator[DObject]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DObjectCodec[DObject], keyCodec:DStringCodec[K]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)


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
  class \\\?[K, V] private(override private[contracts] val __nameOverride:Option[String],
                           override val _codec:DObjectCodec[DObject],
                           override val _dataOperators:List[DataOperator[Option[DObject]]],
                           val _additionalDataOperators:List[DataOperator[Option[Map[K, V]]]],
                           val _additionalKeyCodec:DStringCodec[K],
                           val _additionalValueCodec:DCodec[V]
                          ) extends AdditionalProperties[K, V] with MaybeObjectProperty[D] {

    def this(dataOperators: DataOperator[Option[DObject]]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DObjectCodec[DObject], keyCodec:DStringCodec[K], valueCodec:DCodec[V]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(name:String, dataOperators: DataOperator[Option[DObject]]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DObjectCodec[DObject], keyCodec:DStringCodec[K], valueCodec:DCodec[V]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec:DCodec[V], dataOperators: DataOperator[Option[DObject]]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DObjectCodec[DObject], keyCodec:DStringCodec[K]) =
      this(None, codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def this(valueCodec:DCodec[V], name:String, dataOperators: DataOperator[Option[DObject]]*)(additionalPropertyDataOperators:DataOperator[Option[Map[K, V]]]*)
            (implicit codec:DObjectCodec[DObject], keyCodec:DStringCodec[K]) =
      this(Some(name), codec, dataOperators.toList, additionalPropertyDataOperators.toList, keyCodec, valueCodec)

    def _parent: BaseContract[D] = __internal
  }

}