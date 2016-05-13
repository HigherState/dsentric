package dsentric

import cats.data.{Xor, _}

trait Json extends Any {

  def value:Any

  def toString(implicit R:Renderer) =
    R.print(value)
}

class JValue(val value:Any) extends AnyVal with Json

class JObject(val value:Map[String, Any]) extends AnyVal with Json{
  def +(v:(String, Json)) =
    new JObject(value + (v._1 -> v._2.value))

  def ++(v:TraversableOnce[(String, Json)]) =
    new JObject(value ++ v.map(t => t._1 -> t._2.value))

  def ++(m:JObject) =
    new JObject(value ++ m.value)

  def -(key:String) =
    new JObject(value - key)

  def --(keys:TraversableOnce[String]) =
    new JObject(value -- keys)

  def size = value.size

  def isEmpty = value.isEmpty

  def applyDelta(delta:JObject):JObject =
    JObjectOps.rightReduceConcat(this, delta)

  def select(projection:JObject):JObject =
    JObjectOps.select(this, projection)

  def reduce:Option[JObject] =
    JObjectOps.reduce(this)

}

class JQuery(val value:Map[String, Any]) extends AnyVal{


  def isMatch(j:JObject):Boolean =
    Query(Some(j.value), value)

  def &&(d:JQuery):JQuery =
    (value.get("$and"), d.value.get("$and")) match {
      case (None, None) =>
        if (value.contains("$or") || d.value.contains("$or"))
          new JQuery(Map("$and" -> Vector(value, d.value)))
        else
          new JQuery(JObjectOps.concatMap(value, d.value))
      case (None, Some(vr:Vector[Any]@unchecked)) =>
        new JQuery(Map("$and" -> (value +: vr)))
      case (Some(vl:Vector[Any]@unchecked), None) =>
        new JQuery(Map("$and" -> (vl :+ d.value)))
      case (Some(vl:Vector[Any]@unchecked), Some(vr:Vector[Any]@unchecked)) =>
        new JQuery(Map("$and" -> (vl ++ vr)))
      case _ =>
        new JQuery(Map("$and" -> Vector(value, d.value)))
    }


  def ||(d:JQuery):JQuery =
    (value.get("$or"), d.value.get("$or")) match {
      case (None, Some(vr:Vector[Any]@unchecked)) =>
        new JQuery(Map("$or" -> (value +: vr)))
      case (Some(vl:Vector[Any]@unchecked), None) =>
        new JQuery(Map("$or" -> (vl :+ d.value)))
      case (Some(vl:Vector[Any]@unchecked), Some(vr:Vector[Any]@unchecked)) =>
        new JQuery(Map("$or" -> (vl ++ vr)))
      case _ =>
        new JQuery(Map("$or" -> Vector(value, d.value)))
    }

  def ! :JQuery =
    new JQuery(Map("$not" -> value))

  def not:JQuery = this.!
}

class JArray(val value:Vector[Any]) extends AnyVal with Json

trait JNull

object Json{
  def apply[T](value:T)(implicit codec:JCodec[T]):Json =
    codec.apply(value)
}

object JObject{

  val empty = new JObject(Map.empty[String, Any])

  def apply(map:Map[String, Any]):JObject =
    new JObject(map)
  def apply(values:(String, Json)*):JObject =
    new JObject(values.toIterator.map(p => p._1 -> p._2.value).toMap)
}

object JArray{

  val empty = new JArray(Vector.empty)

  def apply[T](values:T*)(implicit codec:JCodec[T]) =
    new JArray(values.map(codec.apply(_).value).toVector)
}

object JQuery{

  //TODO confirm is valid query structure
  def apply(map:Map[String, Any]):NonEmptyList[(String, Path)] Xor JQuery =
    Xor.right(new JQuery(map))
  def apply(values:(String, Json)*):NonEmptyList[(String, Path)] Xor JQuery =
    Xor.right(new JQuery(values.toIterator.map(p => p._1 -> p._2.value).toMap))
}
