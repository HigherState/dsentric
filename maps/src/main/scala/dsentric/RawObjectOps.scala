package dsentric

//TODO move DObject referencing methods, make pure Raw
trait RawObjectOps {

  def traverseConcat(x: RawObject, y: RawObject): RawObject =
    y.foldLeft(x) {
      case (acc, (k, v: RawObject @unchecked)) =>
        acc.get(k).fold(acc + (k -> v)) {
          case c: RawObject @unchecked =>
            acc + (k -> traverseConcat(c, v))
          case _                       =>
            acc + (k -> v)
        }
      case (acc, (k, v))                       =>
        acc + (k -> v)
    }

  /**
   * Applies changes in y onto x, if y contains a null, it will remove the key if present, or be ignored if not.
   * If an object has its key count reduced to empty and it is nested it will remove the key value pair.
   * @param x
   * @param delta
   * @return
   */
  def removingTraverseConcat(x: RawObject, delta: RawObject, RemoveValue: Raw): RawObject =
    delta.foldLeft(x) {
      case (acc, (k, RemoveValue))                    =>
        acc - k
      case (acc, (k, v: Map[String, Any] @unchecked)) =>
        acc.get(k) match {
          case Some(c: RawObject @unchecked) =>
            removingTraverseConcat(c, v, RemoveValue) match {
              case RawObject.empty =>
                acc - k
              case d               =>
                acc + (k -> d)
            }
          case _                             =>
            //need to reduce delta values in case there are more nulls or empty objects in the delta
            //if empty, we just remove the key
            val m = reduceMap(v, RemoveValue)
            m.fold(acc - k)(vr => acc + (k -> vr))
        }
      case (acc, (k, v))                              =>
        acc + (k -> v)
    }

  /**
   * Applies changes in y onto x, if y contains a null, it will remove the key if present, or be ignored if not.
   * If an object has its key count reduced to empty and it is nested it will remove the key value pair.
   * @param x
   * @param delta
   * @return
   */
  def deltaTraverseConcat(x: RawObject, delta: RawObject): RawObject       =
    removingTraverseConcat(x, delta, DNull)
  def rightDifference: Function[(RawObject, RawObject), Option[RawObject]] = {
    case (s, d) if d == s =>
      None
    case (s, d)           =>
      val r = d.flatMap { kvp =>
        s.get(kvp._1).fold(Option(kvp)) {
          case v if v == kvp._2        =>
            None
          case m: RawObject @unchecked =>
            kvp._2 match {
              case m2: RawObject @unchecked =>
                rightDifference(m -> m2).map(kvp._1 -> _)
              case _                        =>
                Some(kvp)
            }
          case _                       =>
            Some(kvp)
        }
      }
      if (r.nonEmpty) Some(r)
      else None
  }

  /**
   * Left is current, right is new value, result calculates the Delta object
   * @return
   */
  def calculateDelta: Function[(RawObject, RawObject), Option[RawObject]] = {
    case (s, d) if d == s =>
      None
    case (s, d)           =>
      val r      = d.flatMap { kvp =>
        s.get(kvp._1).fold(Option(kvp)) {
          case v if v == kvp._2        =>
            None
          case m: RawObject @unchecked =>
            kvp._2 match {
              case m2: RawObject @unchecked =>
                calculateDelta(m -> m2).map(kvp._1 -> _)
              case _                        =>
                Some(kvp)
            }
          case _                       =>
            Some(kvp)
        }
      }
      val drop   = s.view.filterKeys(k => !d.contains(k)).map(_._1 -> DNull)
      val result = r ++ drop
      if (result.nonEmpty) Some(result)
      else None
  }

  /**
   * For Deltas can strip out values that do not cause any change, possibility delta does nothing at all
   * @return
   */
  def differenceDelta: Function[(RawObject, RawObject), Option[RawObject]] = {
    case (s, d) if d == s =>
      None
    case (s, d)           =>
      val r = d.flatMap { kvp =>
        s.get(kvp._1) -> kvp._2 match {
          case (Some(v), dv) if v == dv                                  =>
            None
          case (Some(m: RawObject @unchecked), dm: RawObject @unchecked) =>
            differenceDelta(m -> dm).map(kvp._1 -> _)
          case (Some(_: RawObject @unchecked), _)                        =>
            Some(kvp)
          //Can end up replacing value with an empty map
          case (Some(_), dm: RawObject @unchecked)                       =>
            Some(kvp._1 -> reduceMap(dm, DNull).getOrElse(RawObject.empty))
          // Drop if delta remove doesnt remove anything
          case (None, DNull)                                             =>
            None
          case (None, dm: RawObject @unchecked)                          =>
            reduceMap(dm, DNull).map(kvp._1 -> _)
          case _                                                         =>
            Some(kvp)
        }
      }
      if (r.nonEmpty) Some(r)
      else None
  }

  def contains(target: RawObject, projectionOrMap: RawObject, leafValuesMustMatch: Boolean): Boolean =
    projectionOrMap.forall {
      case (k, j: RawObject @unchecked) =>
        target.get(k).exists {
          case m: RawObject @unchecked =>
            contains(m, j, leafValuesMustMatch)
          case _                       =>
            false
        }
      case (k, value)                   =>
        target.get(k).exists { v =>
          !leafValuesMustMatch || v == value
        }
    }

  def intersects(target: RawObject, projectionOrMap: RawObject, leafValuesMustMatch: Boolean): Boolean =
    projectionOrMap.exists {
      case (k, j: RawObject @unchecked) =>
        target.get(k).exists {
          case m: RawObject @unchecked =>
            contains(m, j, leafValuesMustMatch)
          case _                       =>
            false
        }
      case (k, value)                   =>
        target.get(k).exists { v =>
          !leafValuesMustMatch || v == value
        }
    }

  def selectMap(target: RawObject, projection: RawObject, wildCard: ProjectionWildcard): RawObject =
    projection.foldLeft(RawObject.empty) {
      case (acc, (k, j: RawObject @unchecked)) if wildCard.matches(k) =>
        target.foldLeft(acc) {
          //ignore explicitly defined properties
          case (a, (k, _)) if projection.contains(k) =>
            a
          case (a, (k, m: RawObject @unchecked))     =>
            val result = selectMap(m, j, wildCard)
            if (result.nonEmpty)
              a + (k -> result)
            else a
          case (a, _)                                =>
            a
        }
      case (acc, (k, 1)) if wildCard.matches(k)                       =>
        //ignore explicitly defined properties
        acc ++ target.view.filterKeys(k => !projection.contains(k))
      case (acc, (k, 1))                                              =>
        target.get(k).fold(acc) { v =>
          acc + (k -> v)
        }
      case (acc, (k, j: RawObject @unchecked))                        =>
        target.get(k).fold(acc) {
          case m: RawObject @unchecked =>
            val result = selectMap(m, j, wildCard)
            if (result.nonEmpty)
              acc + (k -> result)
            else
              acc
          case _                       =>
            acc
        }
      case (acc, _)                                                   =>
        acc
    }

  def omitMap(target: RawObject, projection: RawObject, wildCard: ProjectionWildcard): RawObject =
    projection.foldLeft(target) {

      case (acc, (k, j: RawObject @unchecked)) if wildCard.matches(k) =>
        target.foldLeft(acc) {
          case (a, (k, m: RawObject @unchecked)) =>
            val result = omitMap(m, j, wildCard)
            if (result.nonEmpty)
              a + (k -> result)
            else
              a - k
          case (a, _)                       =>
            a
        }

      case (acc, (k, 1)) if wildCard.matches(k) =>
        //ignore explicitly defined properties
        acc -- target.view.keys.filter(k => !projection.contains(k))

      case (acc, (k, 1)) =>
        acc - k

      case (acc, (k, j: RawObject @unchecked))                        =>
        target.get(k).fold(acc) {
          case m: RawObject @unchecked =>
            val result = omitMap(m, j, wildCard)
            if (result.nonEmpty)
              acc + (k -> result)
            else
              acc - k
          case _                       =>
            acc
        }
      case (acc, _)                                                   =>
        acc
    }

  /*
    Removes nulls and empty objects, return None if empty
   */
  def reduceMap(target: RawObject, Match: Raw): Option[RawObject] = {
    val reducedMap = target.flatMap {
      case (_, Match)                   =>
        None
      case (k, m: RawObject @unchecked) =>
        reduceMap(m, Match).map(k -> _)
      case kvp                          =>
        Some(kvp)
    }
    if (reducedMap.isEmpty) None
    else Some(reducedMap)
  }

  def reducesEmpty(target: RawObject): Boolean =
    target.foldLeft(true) {
      case (false, _)                        =>
        false
      case (_, (_, DNull))                   =>
        true
      case (_, (_, m: RawObject @unchecked)) =>
        reducesEmpty(m)
      case _                                 =>
        false
    }

  def leafMap(target: RawObject)(f: Raw => Raw): RawObject =
    target.map {
      case (k, o: RawObject @unchecked) =>
        k -> leafMap(o)(f)
      case (k, v)                       =>
        k -> f(v)
    }
}

object RawObjectOps extends RawObjectOps
