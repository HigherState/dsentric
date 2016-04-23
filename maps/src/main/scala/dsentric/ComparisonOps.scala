package dsentric

/**
  * Created by Jamie Pullar on 23/04/2016.
  */
object ComparisonOps {

  private[dsentric] def applyDelta(target:Map[String, Any], delta:Map[String, Any]):Map[String, Any] =
    delta.foldLeft(target){
      case (acc, (k, JNull)) =>
        acc - k
      case (acc, (k, v:Map[String, Any]@unchecked)) if v.isEmpty =>
        acc - k
      case (acc, (k, v:Map[String, Any]@unchecked)) =>
        acc.get(k).fold(acc + (k -> v)) {
          case c: Map[String, Any]@unchecked =>
            val d = applyDelta(c, v)
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

  private[dsentric]def select(target:Map[String, Any], projection:Map[String, Any]):Map[String, Any] =
    target.foldLeft(Map.empty[String, Any]) {
      case (acc, (k, true)) =>
        target.get(k).fold(acc){v =>
          acc + (k -> v)
        }

      case (acc, (k, j:Map[String, Any]@unchecked)) =>
        target.get(k).fold(acc){
          case m:Map[String, Any]@unchecked =>
            val result = select(m, j)
            if (result.nonEmpty)
              acc + (k -> result)
            else
              acc
        }
      case (acc, _) =>
        acc
    }

//  private[dsentric] def difference(delta:Map[String, Any], source:Map[String, Any]):Option[Map[String, Any]] =
//    (delta, source) match {
//      case (d, s) if d == s =>
//        None
//      case (JObject(d), JObject(j)) =>
//        val s = j.toMap
//        val o = d.toList.flatMap { kvp =>
//          s.get(kvp._1).fold(Option(kvp)){ v =>
//            difference(kvp._2, v).map(kvp._1 -> _)
//          }
//        }
//        o.nonEmpty.option(Json(o:_*))
//      case (d, _) =>
//        Some(d)
//    }
}
