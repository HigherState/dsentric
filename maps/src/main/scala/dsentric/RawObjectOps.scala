package dsentric


//TODO remove DObject referencing methods, mak pure Raw
trait RawObjectOps {

  def concat(x :DObject, y:DObject):DObject =
    new DObjectInst(concatMap(x.value, y.value))

  def concatMap(x: RawObject, y: RawObject): RawObject =
    y.foldLeft(x){
      case (acc, (k, v:RawObject@unchecked)) =>
        acc.get(k).fold(acc + (k -> v)) {
          case c: RawObject@unchecked =>
            acc + (k -> concatMap(c, v))
          case _ =>
            acc + (k -> v)
        }
      case (acc, (k, v)) =>
        acc + (k -> v)
    }

  //Nulls and Empty object will reduce out when applied from the right
  def rightReduceConcat(x: DObject, y: DObject): DObject =
    new DObjectInst(rightReduceConcatMap(x.value, y.value))

  def rightReduceConcatMap(x: RawObject, y: RawObject): RawObject =
    y.foldLeft(x){
      case (acc, (k, DNull)) =>
        acc - k
      case (acc, (k, RawObject.empty)) =>
        acc - k
      case (acc, (k, v:RawObject@unchecked)) =>
        acc.get(k) match {
          case Some(c:RawObject@unchecked) =>
            rightReduceConcatMap(c, v) match {
              case RawObject.empty =>
                acc - k
              case d =>
                acc + (k -> d)
            }
          case _ =>
            //need to reduce delta values in case there are more nulls or empty objects in the delta
            reduceMap(v).foldLeft(acc)((a,vr) => a + (k -> vr))
        }
      case (acc, (k, v)) =>
        acc + (k -> v)
    }

  def rightDifference(x: DObject, y: DObject): DObject =
    rightDifferenceMap(x.value -> y.value).fold(DObject.empty)(new DObjectInst(_))

  def rightDifferenceMap:Function[(RawObject,RawObject),Option[RawObject]] = {
    case (s, d) if d == s =>
      None
    case (s, d) =>
      val r = d.flatMap { kvp =>
        s.get(kvp._1).fold(Option(kvp)){
          case m:RawObject@unchecked =>
            kvp._2 match {
              case m2:RawObject@unchecked =>
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

  def rightDifferenceReduceMap:Function[(Map[String, Any],Map[String, Any]),Option[Map[String, Any]]] = {
    case (s, d) if d == s =>
      None
    case (s, d) =>
      val r = d.flatMap { kvp =>
        s.get(kvp._1) match {
          case Some(m:Map[String, Any]@unchecked) =>
            kvp._2 match {
              case m2:Map[String, Any]@unchecked =>
                rightDifferenceMap(m -> m2).map(kvp._1 -> _)
              case _ =>
                Some(kvp)
            }
          case Some(v) if v == kvp._2 =>
            None
          // Drop if delta remove doesnt remove anything
          case None if kvp._2 == DNull || kvp._2 == Map.empty =>
            None
          case _ =>
            kvp._2 match {
              case m2:Map[String, Any]@unchecked =>
                reduceMap(m2).map(kvp._1 -> _)
              case _ =>
                Some(kvp)
            }
        }
      }
      if (r.nonEmpty) Some(r)
      else None
  }

  def select(target:DObject, projection:DProjection):DObject =
    new DObjectInst(selectMap(target.value, projection.value))


  def contains(target:RawObject, projectionOrMap:RawObject, leafValuesMustMatch:Boolean):Boolean =
    projectionOrMap.forall {
      case (k, j:RawObject@unchecked) =>
        target.get(k).exists{
          case m:RawObject@unchecked =>
            contains(m, j, leafValuesMustMatch)
          case _ =>
            false
        }
      case (k, value) =>
        target.get(k).exists{v =>
          !leafValuesMustMatch || v == value
        }
    }

  def intersects(target:RawObject, projectionOrMap:RawObject, leafValuesMustMatch:Boolean):Boolean =
    projectionOrMap.exists {
      case (k, j:RawObject@unchecked) =>
        target.get(k).exists{
          case m:RawObject@unchecked =>
            contains(m, j, leafValuesMustMatch)
          case _ =>
            false
        }
      case (k, value) =>
        target.get(k).exists{v =>
          !leafValuesMustMatch || v == value
        }
    }

  def selectMap(target:RawObject, projection:RawObject):RawObject =
    projection.foldLeft(RawObject.empty) {
      case (acc, (k, 1)) =>
        target.get(k).fold(acc){v =>
          acc + (k -> v)
        }

      case (acc, (k, j:RawObject@unchecked)) =>
        target.get(k).fold(acc){
          case m:RawObject@unchecked =>
            val result = selectMap(m, j)
            if (result.nonEmpty)
              acc + (k -> result)
            else
              acc
          case _ =>
            acc
        }
      case (acc, _) =>
        acc
    }

  def omitMap(target:RawObject, projection:RawObject):RawObject =
    projection.foldLeft(target) {
      case (acc, (k, 1)) =>
        acc - k

      case (acc, (k, j:RawObject@unchecked)) =>
        target.get(k).fold(acc){
          case m:RawObject@unchecked =>
            val result = omitMap(m, j)
            if (result.nonEmpty)
              acc + (k -> result)
            else
              acc - k
          case _ =>
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

  def reduceMap(target:RawObject):Option[RawObject] = {
    val reducedMap = target.flatMap {
      case (k, DNull) =>
        None
      case (k, m: RawObject@unchecked) =>
        reduceMap(m).map(k -> _)
      case kvp =>
        Some(kvp)
    }
    if (reducedMap.isEmpty) None
    else Some(reducedMap)
  }
}

object RawObjectOps extends RawObjectOps



