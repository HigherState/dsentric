package dsentric

import cats.data._

import scala.collection.{IterableLike, mutable}

trait Data extends Any {

  def value:Any

  def render(implicit R:Renderer):String =
    R.print(value)

  def asObject:Option[DObject] =
    value match {
      case m:Map[String, Any]@unchecked =>
        Some(new DObjectInst(m))
      case _ =>
        None
    }

  def asArray:Option[DArray] =
    value match {
      case m:Vector[Any]@unchecked =>
        Some(new DArray(m))
      case _ =>
        None
    }

  def nestedContains[T](t:T)(implicit D:DCodec[T]):Boolean =
    DataOps.valueContains(value, t, D)

  def nestedValueMap[T, U](pf:PartialFunction[T, U])(implicit D1:DCodec[T], D2:DCodec[U]):Data =
    new DValue(DataOps.nestedValueMap(value, pf))

  def nestedKeyValueMap[T, U](pf:PartialFunction[(String, T), Option[(String, U)]])(implicit D1:DCodec[T], D2:DCodec[U]):Data =
    new DValue(DataOps.nestedKeyValueMap(value, pf).asInstanceOf[Map[String, Any]])

  def nestedKeyMap(pf:PartialFunction[String, Option[String]]):Data =
    new DValue(DataOps.nestedKeyMap(value, pf).asInstanceOf[Map[String, Any]])

  def decode[T](implicit D:DCodec[T]):Option[T] =
    D.unapply(value)

  def equals(obj:Data):Boolean =
    this.value == obj.value

  override def toString:String =
    SimpleRenderer.print(value)
}
class DValue private[dsentric](val value:Any) extends AnyVal with Data

trait DObjectLike[+This <: DObjectLike[This] with DObject] extends Any with Data with IterableLike[(String, Data), This] {
  def value:Map[String, Any]

  protected def wrap(value:Map[String, Any]):This

  protected[this] def newBuilder: mutable.Builder[(String, Data), This] =
    new mutable.Builder[(String, Data), This] {
      private var elems:List[(String, Any)] = Nil
      def +=(elem: (String, Data)): this.type = {
        elems = elem._1 -> elem._2.value :: elems
        this
      }

      def clear(): Unit = {
        elems = Nil
      }

      def result(): This =
        wrap(elems.toMap)
    }


  def iterator =
    value.iterator.map(p => p._1 -> ForceWrapper.data(p._2))

  def seq =
    toIterator

  private[dsentric] def lensWrap(value:Map[String, Any]):This =
    wrap(value)

  def +(v:(String, Data)):This =
    wrap(value + (v._1 -> v._2.value))

  def ++(v:TraversableOnce[(String, Data)]):This =
    wrap(value ++ v.map(t => t._1 -> t._2.value))

  def -(key:String):This =
    wrap(value - key)

  def --(keys:TraversableOnce[String]):This =
    wrap(value -- keys)

  def \[T](path:Path)(implicit D:DCodec[T]):Option[T] =
    PathLensOps
      .traverse(value, path)
      .collect{ case D(t) => t}

  def toObject:DObject =
    new DObjectInst(value)

  def get(key:String):Option[Data] =
    value.get(key).map(ForceWrapper.data)

  override def size:Int =
    value.size

  override def isEmpty:Boolean =
    value.isEmpty

  override def nonEmpty:Boolean =
    value.nonEmpty

  def keys:Iterable[String] =
    value.keys

  def keySet:Set[String] =
    value.keySet

  def contains(key:String):Boolean =
    value.contains(key)

  override def toIterable:Iterable[(String, Data)] =
    value.mapValues(ForceWrapper.data)

  def values:Iterable[Data] =
    value.values.map(ForceWrapper.data)

  def applyDelta(delta:DObject):This =
    wrap(DObjectOps.rightReduceConcatMap(this.value, delta.value))

  def reduce:Option[This] =
    DObjectOps.reduceMap(this.value).map(wrap)

  def diff(compare:DObject):DObject =
    DObjectOps.rightDifferenceMap(value -> compare.value).fold(DObject.empty)(new DObjectInst(_))

  def toQuery:NonEmptyList[(String, Path)] Either DQuery =
    DQuery(value)

  def select(projection:DProjection):This =
    wrap(DObjectOps.selectMap(value, projection.value))

  override def nestedValueMap[T, U](pf:PartialFunction[T, U])(implicit D1:DCodec[T], D2:DCodec[U]):This =
    wrap(DataOps.nestedValueMap(value, pf).asInstanceOf[Map[String, Any]])

  override def nestedKeyValueMap[T, U](pf:PartialFunction[(String, T), Option[(String, U)]])(implicit D1:DCodec[T], D2:DCodec[U]):This =
    wrap(DataOps.nestedKeyValueMap(value, pf).asInstanceOf[Map[String, Any]])

  override def nestedKeyMap(pf:PartialFunction[String, Option[String]]):This =
    wrap(DataOps.nestedKeyMap(value, pf).asInstanceOf[Map[String, Any]])
}

trait DObject extends Any with DObjectLike[DObject]

final class DObjectInst private[dsentric](val value:Map[String, Any]) extends AnyVal with DObject {
  @inline
  protected def wrap(value: Map[String, Any]):DObject =
    new DObjectInst(value)
}

final class DQuery private[dsentric](val value:Map[String, Any]) extends AnyVal with DObject with DObjectLike[DQuery]{

  protected def wrap(value: Map[String, Any]) = new DQuery(value)

  def isMatch(j:DObject):Boolean =
    Query(Some(j.value), value)

  def &&(d:DQuery):DQuery =
    (value.get("$and"), d.value.get("$and")) match {
      case (None, None) =>
        if (value.contains("$or") || d.value.contains("$or"))
          new DQuery(Map("$and" -> Vector(value, d.value)))
        else
          new DQuery(DObjectOps.concatMap(value, d.value))
      case (None, Some(vr:Vector[Any]@unchecked)) =>
        new DQuery(Map("$and" -> (value +: vr)))
      case (Some(vl:Vector[Any]@unchecked), None) =>
        new DQuery(Map("$and" -> (vl :+ d.value)))
      case (Some(vl:Vector[Any]@unchecked), Some(vr:Vector[Any]@unchecked)) =>
        new DQuery(Map("$and" -> (vl ++ vr)))
      case _ =>
        new DQuery(Map("$and" -> Vector(value, d.value)))
    }


  def ||(d:DQuery):DQuery =
    (value.get("$or"), d.value.get("$or")) match {
      case (None, Some(vr:Vector[Any]@unchecked)) =>
        new DQuery(Map("$or" -> (value +: vr)))
      case (Some(vl:Vector[Any]@unchecked), None) =>
        new DQuery(Map("$or" -> (vl :+ d.value)))
      case (Some(vl:Vector[Any]@unchecked), Some(vr:Vector[Any]@unchecked)) =>
        new DQuery(Map("$or" -> (vl ++ vr)))
      case _ =>
        new DQuery(Map("$or" -> Vector(value, d.value)))
    }

  def ! :DQuery =
    new DQuery(Map("$not" -> value))

  def not:DQuery = this.!
}

final class DProjection(val value:Map[String, Any]) extends AnyVal with DObject with DObjectLike[DProjection] {

  protected def wrap(value: Map[String, Any]) = new DProjection(value)

  def &(d:DProjection):DProjection =
    new DProjection(DObjectOps.concatMap(value, d.value))

  def select(obj:DObject):DObject =
    new DObjectInst(DObjectOps.selectMap(obj.value, value))

  def toPaths:Option[Set[Path]] =
    getPaths(value, List.empty)

  private def getPaths(projection:Map[String, Any], segments:Path):Option[Set[Path]] = {
    val pairs = projection.flatMap {
      case (key, 1) =>
        Some(Set(segments :+ Right(key)))
      case (key, j:Map[String, Any]@unchecked) =>
        getPaths(j, segments :+ Right(key))
      case _ =>
        None
    }.toList
    if (pairs.length < projection.size) None
    else Some(pairs.reduce(_ ++ _))
  }
}

class DArray(val value:Vector[Any]) extends AnyVal with Data {
  def toObjects:Vector[DObject] =
    value.collect {
      case m:Map[String, Any]@unchecked =>
        new DObjectInst(m)
    }

  override def nestedValueMap[T, U](pf:PartialFunction[T, U])(implicit D1:DCodec[T], D2:DCodec[U]):DArray =
    new DArray(DataOps.nestedValueMap(value, pf).asInstanceOf[Vector[Any]])
}

final class DNull extends Data {
  override def value:DNull = this
  override def toString:String = "null"

  override def equals(obj: scala.Any): Boolean =
    obj.isInstanceOf[DNull]
}

object Data{
  def apply[T](value:T)(implicit codec:DCodec[T]):Data =
    codec.apply(value)
}

final class DTrue(implicit val codec:DCodec[Boolean]) extends Data {
  def value = true
  def unapply(value:Data):Boolean =
    codec.unapply(value.value).getOrElse(false)
}
final class DFalse(implicit val codec:DCodec[Boolean]) extends Data {
  def value = false
  def unapply(value:Data):Boolean =
    codec.unapply(value.value).getOrElse(false)
}


object DObject{

  val empty:DObject =
    new DObjectInst(Map.empty[String, Any])

  def apply(values:(String, Data)*):DObject =
    new DObjectInst(values.toIterator.map(p => p._1 -> p._2.value).toMap)

  def apply(map:Map[String, Data]):DObject =
    new DObjectInst(map.mapValues(_.value))
}

object DArray{

  val empty = new DArray(Vector.empty)

  def apply[T](values:T*)(implicit codec:DCodec[T]) =
    new DArray(values.map(codec.apply(_).value).toVector)
}

object DQuery{

  //TODO confirm is valid query structure
  def apply(values:(String, Data)*):NonEmptyList[(String, Path)] Either DQuery =
    Right(new DQuery(values.toIterator.map(p => p._1 -> p._2.value).toMap))

  private[dsentric] def apply(value:Map[String, Any]):NonEmptyList[(String, Path)] Either DQuery =
    Right(new DQuery(value))

  val empty = new DQuery(Map.empty)
}

object ForceWrapper {

  def data(value:Any):Data =
    value match {
      case m:Map[String,Any]@unchecked =>
        dObject(m)
      case v:Vector[Any]@unchecked =>
        dArray(v)
      case a:Any =>
        dValue(a)
    }

  def dObject(value:Map[String, Any]):DObject =
    new DObjectInst(value)

  def dValue(value:Any) =
    new DValue(value)

  def dArray(value:Vector[Any]) =
    new DArray(value)

  def dQuery(value:Map[String, Any]) =
    new DQuery(value)
}
