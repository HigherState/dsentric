package dsentric

trait DObjectOps {

  def concat(x :DObject, y:DObject):DObject =
    new DObjectInst(concatMap(x.value, y.value))

  private[dsentric] def concatMap(x: Map[String, Any], y: Map[String, Any]): Map[String, Any] =
    y.foldLeft(x){
      case (acc, (k, v:Map[String, Any]@unchecked)) =>
        acc.get(k).fold(acc + (k -> v)) {
          case c: Map[String, Any]@unchecked =>
            acc + (k -> concatMap(c, v))
          case c =>
            acc + (k -> v)
        }
      case (acc, (k, v)) =>
        acc + (k -> v)
    }

  //Nulls and Empty object will reduce out when applied from the right
  def rightReduceConcat(x: DObject, y: DObject): DObject =
    new DObjectInst(rightReduceConcatMap(x.value, y.value))

  private val EMPTY_MAP = Map.empty[String, Any]
  private[dsentric] def rightReduceConcatMap(x: Map[String, Any], y: Map[String, Any]): Map[String, Any] =
    y.foldLeft(x){
      case (acc, (k, _:DNull)) =>
        acc - k
      case (acc, (k, EMPTY_MAP)) =>
        acc - k
      case (acc, (k, v:Map[String, Any]@unchecked)) =>
        acc.get(k).fold(acc + (k -> v)) {
          case c: Map[String, Any]@unchecked =>
            rightReduceConcatMap(c, v) match {
              case EMPTY_MAP =>
                acc - k
              case d =>
                acc + (k -> d)
            }
          case _ =>
            acc + (k -> v)
        }
      case (acc, (k, v)) =>
        acc + (k -> v)
    }

  def rightDifference(x: DObject, y: DObject): DObject =
    rightDifferenceMap(x.value -> y.value).fold(DObject.empty)(new DObjectInst(_))

  private[dsentric] def rightDifferenceMap:Function[(Map[String, Any],Map[String, Any]),Option[Map[String, Any]]] = {
    case (s, d) if d == s =>
      None
    case (s, d) =>
      val r = d.flatMap { kvp =>
        s.get(kvp._1).fold(Option(kvp)){
          case m:Map[String, Any]@unchecked =>
            kvp._2 match {
              case m2:Map[String, Any]@unchecked =>
                rightDifferenceMap(m -> m2).map(kvp._1 -> _)
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

  def select(target:DObject, projection:DProjection):DObject =
    new DObjectInst(selectMap(target.value, projection.value))

  private[dsentric] def selectMap(target:Map[String, Any], projection:Map[String, Any]):Map[String, Any] =
    projection.foldLeft(Map.empty[String, Any]) {
      case (acc, (k, 1)) =>
        target.get(k).fold(acc){v =>
          acc + (k -> v)
        }

      case (acc, (k, j:Map[String, Any]@unchecked)) =>
        target.get(k).fold(acc){
          case m:Map[String, Any]@unchecked =>
            val result = selectMap(m, j)
            if (result.nonEmpty)
              acc + (k -> result)
            else
              acc
        }
      case (acc, _) =>
        acc
    }

  /*
    Removes nulls and empty objects, return None if empty
   */
  def reduce(target:DObject):Option[DObject] =
    reduceMap(target.value).map(new DObjectInst(_))

  private[dsentric] def reduceMap(target:Map[String, Any]):Option[Map[String, Any]] = {
    val reducedMap = target.flatMap {
      case (k, _: DNull) =>
        None
      case (k, m: Map[String, Any]@unchecked) =>
        reduceMap(m).map(k -> _)
      case kvp =>
        Some(kvp)
    }
    if (reducedMap.isEmpty) None
    else Some(reducedMap)
  }
}

object DObjectOps extends DObjectOps



