package dsentric

case class JObject(value:Map[String, Any]) extends AnyVal {
  def +(v:JPair) =
    JObject(value + v.toTuple)

  def ++(v:TraversableOnce[JPair]) =
    JObject(value ++ v.map(_.toTuple))

  def ++(m:JObject) =
    JObject(value ++ m.value)

  def -(key:String) =
    JObject(value - key)

  def --(keys:TraversableOnce[String]) =
    JObject(value -- keys)

  def size = value.size

  def isEmpty = value.isEmpty

}

case class JPair(key:String, value:Any) {
  def toTuple = key -> value
}

sealed trait JNull
object JNull extends JNull

object JObject{

  val empty = JObject(Map.empty[String, Any])
  def apply(values:JPair*):JObject =
    JObject(values.toIterator.map(_.toTuple).toMap)
}
