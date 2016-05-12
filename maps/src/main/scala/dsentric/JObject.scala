package dsentric

class Json(val value:Any) extends AnyVal {
  def asObject:Option[JObject] =
    value match {
      case m:Map[String, Any]@unchecked =>
        Some(new JObject(m))
      case _ =>
        None
    }

  def asArray:Option[JArray] =
    value match {
      case v:Vector[Any]@unchecked =>
        Some(new JArray(v))
      case _ =>
        None
    }

  def toString(implicit R:Renderer) =
    R.print(value)
}

class JObject(val value:Map[String, Any]) extends AnyVal{
  def +(v:JPair) =
    new JObject(value + v.toTuple)

  def ++(v:TraversableOnce[JPair]) =
    new JObject(value ++ v.map(_.toTuple))

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

  def toString(implicit R:Renderer) =
    R.print(value)
}

class JArray(val value:Vector[Any]) extends AnyVal {
  def toString(implicit R:Renderer) =
    R.print(value)
}

case class JPair(key:String, value:Any) {
  def toTuple = key -> value
}

trait JNull

object Json{
  def apply[T](value:T)(implicit codec:JCodec[T]):Json =
    new Json(codec.apply(value))
}

object JObject{

  val empty = new JObject(Map.empty[String, Any])

  def apply(map:Map[String, Any]):JObject =
    new JObject(map)
  def apply(values:JPair*):JObject =
    new JObject(values.toIterator.map(_.toTuple).toMap)
}

object JArray{

  val empty = new JArray(Vector.empty)

  def apply[T](values:T*)(implicit codec:JCodec[T]) =
    new JArray(values.flatMap(codec.unapply).toVector)
}
