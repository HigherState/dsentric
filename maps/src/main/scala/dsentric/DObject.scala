package dsentric

import cats.data.{Xor, _}

trait Data extends Any {

  def value:Any

  def render(implicit R:Renderer) =
    R.print(value)

  def asObject:Option[DObject] =
    value match {
      case m:Map[String, Any]@unchecked =>
        Some(new DObject(m))
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

  def decode[T](implicit D:DCodec[T]) =
    D.unapply(value)

  override def toString() = {
    SimpleRenderer.print(value)
  }
}

class DValue private[dsentric](val value:Any) extends AnyVal with Data

class DObject private[dsentric](val value:Map[String, Any]) extends AnyVal with Data{
  def +(v:(String, Data)) =
    new DObject(value + (v._1 -> v._2.value))

  def ++(v:TraversableOnce[(String, Data)]) =
    new DObject(value ++ v.map(t => t._1 -> t._2.value))

  def ++(m:DObject) =
    new DObject(value ++ m.value)

  def -(key:String) =
    new DObject(value - key)

  def --(keys:TraversableOnce[String]) =
    new DObject(value -- keys)

  def \[T](path:Path)(implicit D:DCodec[T]):Option[T] =
    PathLensOps
      .traverse(value, path)
      .collect{ case D(t) => t}


  def size = value.size

  def isEmpty = value.isEmpty

  def nonEmpty = value.nonEmpty

  def keys = value.keys

  def keySet = value.keySet

  def contains(key:String) = value.contains(key)

  def applyDelta(delta:DObject):DObject =
    DObjectOps.rightReduceConcat(this, delta)

  def reduce:Option[DObject] =
    DObjectOps.reduce(this)

  def diff(compare:DObject):DObject =
    DObjectOps.rightDifference(this, compare)

  def toQuery:NonEmptyList[(String, Path)] Xor DQuery =
    DQuery(value)

  override def nestedValueMap[T, U](pf:PartialFunction[T, U])(implicit D1:DCodec[T], D2:DCodec[U]):DObject =
    new DObject(DataOps.nestedValueMap(value, pf).asInstanceOf[Map[String, Any]])

}



class DQuery private[dsentric](val value:Map[String, Any]) extends AnyVal with Data{
  def +(v:(String, Data)) =
    new DQuery(value + (v._1 -> v._2.value))

  def ++(v:TraversableOnce[(String, Data)]) =
    new DQuery(value ++ v.map(t => t._1 -> t._2.value))

  def ++(m:DQuery) =
    new DQuery(value ++ m.value)

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

  def toObject:DObject =
    new DObject(value)
}

class DProjection(val value:Map[String, Any]) extends AnyVal with Data{

  def &(d:DProjection):DProjection =
    new DProjection(DObjectOps.concatMap(value, d.value))

  def toObject:DObject =
    new DObject(value)

  def select(obj:DObject):DObject =
    new DObject(DObjectOps.selectMap(obj.value, value))

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
  def toObjects = value.collect {
    case m:Map[String, Any]@unchecked =>
      new DObject(m)
  }

  override def nestedValueMap[T, U](pf:PartialFunction[T, U])(implicit D1:DCodec[T], D2:DCodec[U]):DArray =
    new DArray(DataOps.nestedValueMap(value, pf).asInstanceOf[Vector[Any]])
}

final class DNull extends Data {
  override def value:DNull = this
  override def toString() = "null"

  override def equals(obj: scala.Any): Boolean =
    obj.isInstanceOf[DNull]
}

object Data{
  def apply[T](value:T)(implicit codec:DCodec[T]):Data =
    codec.apply(value)


}

object DObject{

  val empty = new DObject(Map.empty[String, Any])

  def apply(values:(String, Data)*):DObject =
    new DObject(values.toIterator.map(p => p._1 -> p._2.value).toMap)

  def apply(map:Map[String, Data]):DObject =
    new DObject(map.mapValues(_.value))
}

object DArray{

  val empty = new DArray(Vector.empty)

  def apply[T](values:T*)(implicit codec:DCodec[T]) =
    new DArray(values.map(codec.apply(_).value).toVector)
}

object DQuery{

  //TODO confirm is valid query structure
  def apply(values:(String, Data)*):NonEmptyList[(String, Path)] Xor DQuery =
    Xor.right(new DQuery(values.toIterator.map(p => p._1 -> p._2.value).toMap))

  private[dsentric] def apply(value:Map[String, Any]):NonEmptyList[(String, Path)] Xor DQuery =
    Xor.right(new DQuery(value))

  val empty = new DQuery(Map.empty)
}

object ForceWrapper {

  def dObject(value:Map[String, Any]) =
    new DObject(value)

  def dValue(value:Any) =
    new DValue(value)

  def dArray(value:Vector[Any]) =
    new DArray(value)

  def dQuery(value:Map[String, Any]) =
    new DQuery(value)
}
