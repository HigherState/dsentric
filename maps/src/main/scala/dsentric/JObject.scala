package dsentric

class JObject(val value:Map[String, Any]) extends AnyVal {
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
    new JObject(ComparisonOps.applyDelta(value, delta.value))

}

case class JPair(key:String, value:Any) {
  def toTuple = key -> value
}

sealed trait JNull
object JNull extends JNull

object JObject{

  val empty = new JObject(Map.empty[String, Any])

  def apply(map:Map[String, Any]):JObject =
    new JObject(map)
  def apply(values:JPair*):JObject =
    new JObject(values.toIterator.map(_.toTuple).toMap)
}
