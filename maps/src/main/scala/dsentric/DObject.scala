package dsentric

import cats.data._

import scala.collection.{Iterable, IterableLike, mutable}

trait Data extends Any {

  def value:Any

  def isNull: Boolean = value match {
    case null => true
    case x: Data => x.isNull
    case _ => false
  }

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

  override def toString:String =
    SimpleRenderer.print(value)
}

class DValue private[dsentric](val value:Any) extends AnyVal with Data

trait DObjectLike[+This <: DObjectLike[This] with DObject] extends Any with Data with IterableLike[(String, Data), This] {
  def value:Map[String, Any]

  override protected[this] def thisCollection: Iterable[(String, Data)] =
    value.map(p => p._1 -> ForceWrapper.data(p._2))
  override protected[this] def toCollection(repr: This): Iterable[(String, Data)] =
    repr.value.map(p => p._1 -> ForceWrapper.data(p._2))
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

  def iterator: Iterator[(String, Data)] =
    value.iterator.map(p => p._1 -> ForceWrapper.data(p._2))

  def seq: Iterator[(String, Data)] =
    toIterator

  private[dsentric] def internalWrap(value:Map[String, Any]):This =
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

  def +\(v:(Path, Data)):This =
    wrap(PathLensOps.set(value, v._1, v._2.value))

  def ++\(v:TraversableOnce[(Path, Data)]):This =
    wrap(v.foldLeft(value){(v, e) =>
      PathLensOps.set(value, e._1, e._2.value)
    })

  def toObject:DObject =
    new DObjectInst(value)

  def get(key:String):Option[Data] =
    value.get(key).map(ForceWrapper.data)

  def get(path: Path):Option[Data] =
    PathLensOps.traverse(value, path).map(ForceWrapper.data)

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

  def contains(path:Path):Boolean =
    PathLensOps
      .traverse(value, path).nonEmpty
  //All projected paths must be found
  def contains(projection:DProjection):Boolean =
    DObjectOps.contains(value, projection.value, false)

  def contains(obj:DObject):Boolean =
    DObjectOps.contains(value, obj.value, true)

  def intersects(projection:DProjection):Boolean =
    DObjectOps.intersects(value, projection.value, false)

  def intersects(obj:DObject):Boolean =
    DObjectOps.intersects(value, obj.value, false)

  /*
  Value on the left hand side is the selection by the projection, the value on the right has the values exluded
   */
  def partition(projection:DProjection):(DObject, DObject) =
    this.select(projection) -> this.omit(projection)

  override def toIterable:Iterable[(String, Data)] =
    value.mapValues(ForceWrapper.data)

  def values:Iterable[Data] =
    value.values.map(ForceWrapper.data)

  def applyDelta(delta:DObject):This =
    wrap(DObjectOps.rightReduceConcatMap(this.value, delta.value))

  def reduce:Option[This] =
    DObjectOps.reduceMap(this.value).map(wrap)

  //Returns the differences in the compare object
  def diff(compare:DObject):DObject =
    DObjectOps.rightDifferenceMap(value -> compare.value).fold(DObject.empty)(new DObjectInst(_))

  def toQuery:NonEmptyList[(String, Path)] Either DQuery =
    DQuery(value)

  def select(projection:DProjection):This =
    wrap(DObjectOps.selectMap(value, projection.value))

  def omit(projection:DProjection):This =
    wrap(DObjectOps.omitMap(value, projection.value))

  def filterKeys(p:String => Boolean):This =
    wrap(value.filterKeys(p))

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

final class DProjection private[dsentric](val value:Map[String, Any]) extends AnyVal with DObject with DObjectLike[DProjection] {

  protected def wrap(value: Map[String, Any]) = new DProjection(value)

  def &(key:String): DProjection =
    wrap(value + (key -> 1))

  def &(path:Path): DProjection =
    wrap(value ++ PathLensOps.pathToMap(path, 1))

  //Nest projection into a new object under the given key
  def nest(key:String):DProjection =
    wrap(Map(key -> value))

  def nest(path:Path):DProjection =
    wrap(PathLensOps.pathToMap(path, value))

  def &(d:DProjection):DProjection =
    new DProjection(DObjectOps.concatMap(value, d.value))

  def select[D <: DObjectLike[D] with DObject](obj:D):D =
    obj.internalWrap(DObjectOps.selectMap(obj.value, value))

  def toPaths:Set[Path] =
    getPaths(value, Path.empty).getOrElse(Set.empty)

  def toDObject:DObject =
    new DObjectInst(value)

  private def getPaths(projection:Map[String, Any], segments:Path):Option[Set[Path]] = {
    val pairs = projection.flatMap {
      case (key, 1) =>
        Some(Set(segments \ key))
      case (key, j:Map[String, Any]@unchecked) =>
        getPaths(j, segments \ key)
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

  def toValues[T](implicit D:DCodec[T]):Vector[T] =
    value.flatMap(D.unapply)

  def toDataValues:Vector[Data] =
    toValues(DefaultCodecs.dataCodec)

  override def nestedValueMap[T, U](pf:PartialFunction[T, U])(implicit D1:DCodec[T], D2:DCodec[U]):DArray =
    new DArray(DataOps.nestedValueMap(value, pf).asInstanceOf[Vector[Any]])

  def intersect(dArray: DArray): DArray =
    new DArray(value.intersect(dArray.value))

  def nonEmpty: Boolean =
    value.nonEmpty

  def intersects(dArray: DArray): Boolean =
    intersect(dArray).nonEmpty

  def contains(data: Data): Boolean =
    value.contains(data.value)
}

final class DNull extends Data {
  override def value:DNull = this
  override def toString:String = "null"
  override def isNull: Boolean = true

  override def equals(obj: scala.Any): Boolean =
    obj.isInstanceOf[DNull]
}

object Data{
  def apply[T](value:T)(implicit codec:DCodec[T]):Data =
    codec.apply(value)
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

object DProjection {

  val empty = new DProjection(Map.empty)

  def apply(paths:Path*):DProjection =
    new DProjection(paths.map(p => PathLensOps.pathToMap(p, 1))
      .foldLeft(Map.empty[String, Any])(DObjectOps.concatMap))
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

  def dProjection(value:Map[String, Any]) =
    new DProjection(value)
}
