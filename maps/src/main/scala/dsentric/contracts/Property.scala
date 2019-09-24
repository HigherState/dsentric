package dsentric.contracts

import dsentric._
import dsentric.operators.DataOperator

sealed trait Property[D <: DObject, T <: Any] {

  private var __path: Path = _
  private var __localPath: Path = _
  @volatile
  private var _bitmap1:Boolean = false

  private[contracts] def __nameOverride:Option[String]
  def _dataOperators:Seq[DataOperator[_]]

  private[dsentric] def _strictness:Strictness
  private[dsentric] def _strictGet(data:D):PathResult[T]

  def _codec: DCodec[T]
  def _parent: BaseContract[D]
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
  extends Property[D, T] with ExpectedLens[D, T] {

  val $delta:PatternMatcher[D, Option[T]] =
    new PatternMatcher(_strictDeltaGet(_).toMatcher)

  def unapply(j: D): Option[T] =
    _strictGet(j).toOption
}

class MaybeProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
  val _parent:BaseContract[D],
  val _codec:DCodec[T],
  val _strictness:Strictness,
  val _dataOperators:Seq[DataOperator[Option[T]]])
  extends Property[D, T] with MaybeLens[D, T] {

  def unapply(j:D):Option[Option[T]] =
    _strictGet(j).toMatcher

  val $delta:PatternMatcher[D, Option[Option[T]]] =
    new PatternMatcher[D, Option[Option[T]]](d => _strictDeltaGet(d).map(_.toOption).toMatcher)
}

class DefaultProperty[D <: DObject, T] private[contracts]
  (private[contracts] val __nameOverride:Option[String],
   val _default:T,
   val _parent:BaseContract[D],
   val _codec:DCodec[T],
   val _strictness:Strictness,
   val _dataOperators:Seq[DataOperator[Option[T]]])
  extends Property[D, T] with DefaultLens[D, T] {

  def unapply(j:D):Option[T] =
    _strictGet(j).toOption

  val $delta:PatternMatcher[D, Option[Option[T]]] =
    new PatternMatcher(_strictDeltaGet(_).map(_.toOption).toMatcher)

  val $deltaDefault:PatternMatcher[D, Option[T]] =
    new PatternMatcher(_strictDeltaGet(_).map(_.getValue).toMatcher)

}

sealed trait ObjectsProperty[D <: DObject, T <: DObject] extends Property[D, Vector[T]] {
  def _contract:ContractFor[T]
}


class ExpectedObjectsProperty[D <: DObject, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                             val _contract:ContractFor[T],
                                                             val _parent:BaseContract[D],
                                                             val _codec:DCodec[Vector[T]],
                                                             val _dataOperators:Seq[DataOperator[Vector[T]]]
                                                            ) extends ObjectsProperty[D, T] with ExpectedLens[D, Vector[T]] {

  def unapply(j: D): Option[Vector[T]] =
    _strictGet(j).toOption
}

class MaybeObjectsProperty[D <: DObject, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                          val _contract:ContractFor[T],
                                                          val _parent:BaseContract[D],
                                                          val _codec:DCodec[Vector[T]],
                                                          val _strictness:Strictness,
                                                          val _dataOperators:Seq[DataOperator[Option[Vector[T]]]]
                                                         ) extends ObjectsProperty[D, T] with MaybeLens[D, Vector[T]] {

  def unapply(j:D):Option[Option[Vector[T]]] =
    _strictGet(j).toMatcher
}

class DefaultObjectsProperty[D <: DObject, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                                val _contract:ContractFor[T],
                                                                val _default:Vector[T],
                                                                val _parent:BaseContract[D],
                                                                val _codec:DCodec[Vector[T]],
                                                                val _strictness:Strictness,
                                                                val _dataOperators:Seq[DataOperator[Option[Vector[T]]]]
                                                           ) extends ObjectsProperty[D, T] with DefaultLens[D, Vector[T]] {

  def unapply(j:D):Option[Vector[T]] =
    _strictGet(j).toOption

}


sealed trait MapObjectsProperty[K, D <: DObject, T <: DObject] extends Property[D, Map[K, T]] {
  def _contract:ContractFor[T]
  def _keyCodec:DCodec[K]
}

class ExpectedMapObjectsProperty[K, D <: DObject, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                          val _contract:ContractFor[T],
                                                          val _parent:BaseContract[D],
                                                          val _codec:DCodec[Map[K, T]],
                                                          val _keyCodec:DCodec[K],
                                                          val _dataOperators:Seq[DataOperator[Map[K, T]]]
                                                         ) extends MapObjectsProperty[K, D, T] with ExpectedLens[D, Map[K, T]] {

  def unapply(j: D): Option[Map[K, T]] =
    _strictGet(j).toOption
}

class MaybeMapObjectsProperty[K, D <: DObject, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                       val _contract:ContractFor[T],
                                                       val _parent:BaseContract[D],
                                                       val _codec:DCodec[Map[K, T]],
                                                       val _keyCodec:DCodec[K],
                                                       val _strictness:Strictness,
                                                       val _dataOperators:Seq[DataOperator[Option[Map[K, T]]]]
                                                      ) extends MapObjectsProperty[K, D, T] with MaybeLens[D, Map[K, T]] {

  def unapply(j:D):Option[Option[Map[K, T]]] =
    _strictGet(j).toMatcher
}

class DefaultMapObjectsProperty[K, D <: DObject, T <: DObject](private[contracts] val __nameOverride:Option[String],
                                                         val _contract:ContractFor[T],
                                                         val _default:Map[K, T],
                                                         val _parent:BaseContract[D],
                                                         val _codec:DCodec[Map[K, T]],
                                                         val _keyCodec:DCodec[K],
                                                         val _strictness:Strictness,
                                                         val _dataOperators:Seq[DataOperator[Option[Map[K, T]]]]
                                                        ) extends MapObjectsProperty[K, D, T] with DefaultLens[D, Map[K, T]] {

  def unapply(j:D):Option[Map[K, T]] =
    _strictGet(j).toOption

}
//
//private[dsentric] class NothingProperty[T](implicit val _codec:DCodec[T]) extends Property[Nothing, T] {
//
//  private[dsentric] def _strictness:Strictness =
//    IgnoreOnWrongType
//
//  private[dsentric] def _strictGet(data: Nothing): PathResult[T] = Empty
//
//  private[contracts] def __nameOverride: Option[String] =
//    None
//
//  def _dataOperators: Seq[DataOperator[_]] =
//    Seq.empty
//
//  def _parent: BaseContract[Nothing] =
//    NothingBaseContract
//}
