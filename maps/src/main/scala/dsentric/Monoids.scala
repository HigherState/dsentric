package dsentric

import cats.Monoid

/*
  Will remove keys with null or empty map values.
 */
object ReduceConcatMonoid extends Monoid[JObject] {
  def empty: JObject = JObject.empty

  def combine(x: JObject, y: JObject): JObject =
    JObject(combine(x.value, y.value))

  private def combine(x: Map[String, Any], y: Map[String, Any]): Map[String, Any] =
    y.foldLeft(x){
      case (acc, (k, _:JNull)) =>
        acc - k
      case (acc, (k, v:Map[String, Any]@unchecked)) if v.isEmpty =>
        acc - k
      case (acc, (k, v:Map[String, Any]@unchecked)) =>
        acc.get(k).fold(acc + (k -> v)) {
          case c: Map[String, Any]@unchecked =>
            val d = combine(c, v)
            if (d.isEmpty)
              acc - k
            else
              acc + (k -> d)
          case c =>
            acc + (k -> v)
        }
      case (acc, (k, v)) =>
        acc + (k -> v)
    }
}

object RightDifferenceMonoid extends Monoid[JObject] {

  def empty: JObject = JObject.empty

  def combine(x: JObject, y: JObject): JObject =
    diff(x.value -> y.value).fold(JObject.empty)(new JObject(_))

  def diff:Function[(Map[String, Any],Map[String, Any]),Option[Map[String, Any]]] = {
    case (s, d) if d == s =>
      None
    case (s, d) =>
      val r = d.flatMap { kvp =>
        s.get(kvp._1).fold(Option(kvp)){
          case m:Map[String, Any]@unchecked =>
            kvp._2 match {
              case m2:Map[String, Any]@unchecked =>
                diff(m -> m2).map(kvp._1 -> _)
              case _ =>
                Some(kvp)
            }
          case v if v == kvp._2 =>
            None
          case _ =>
            Some(kvp)
        }
      }
      if (r.nonEmpty) Some(r)
      else None
    }
}


//  private[dsentric]def select(target:Map[String, Any], projection:Map[String, Any]):Map[String, Any] =
//    target.foldLeft(Map.empty[String, Any]) {
//      case (acc, (k, true)) =>
//        target.get(k).fold(acc){v =>
//          acc + (k -> v)
//        }
//
//      case (acc, (k, j:Map[String, Any]@unchecked)) =>
//        target.get(k).fold(acc){
//          case m:Map[String, Any]@unchecked =>
//            val result = select(m, j)
//            if (result.nonEmpty)
//              acc + (k -> result)
//            else
//              acc
//        }
//      case (acc, _) =>
//        acc
//    }
