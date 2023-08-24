package dsentric.filter

import com.github.ghik.silencer.silent
import dsentric.{DObject, DObjectOps, Data, Path, RawArray, RawObject, RawObjectOps}

final class DFilter private[dsentric] (val value: RawObject) extends AnyVal with DObject with DObjectOps[DFilter] {

  protected def wrap(value: RawObject) = new DFilter(value)

  def isMatch(j: DObject, valueNotFoundAsNull: Boolean = false): Boolean =
    DFilterOps(Some(j.value), value, valueNotFoundAsNull)

  def &&(d: DFilter): DFilter =
    (value.get("$and"), d.value.get("$and")) match {
      case (None, None)                                                   =>
        if (value.contains("$or") || d.value.contains("$or"))
          new DFilter(Map("$and" -> Vector(value, d.value)))
        else
          new DFilter(RawObjectOps.traverseConcat(value, d.value))
      case (None, Some(vr: RawArray @unchecked))                          =>
        new DFilter(Map("$and" -> (value +: vr)))
      case (Some(vl: RawArray @unchecked), None)                          =>
        new DFilter(Map("$and" -> (vl :+ d.value)))
      case (Some(vl: RawArray @unchecked), Some(vr: RawArray @unchecked)) =>
        new DFilter(Map("$and" -> (vl ++ vr)))
      case _                                                              =>
        new DFilter(Map("$and" -> Vector(value, d.value)))
    }

  def ||(d: DFilter): DFilter =
    (value.get("$or"), d.value.get("$or")) match {
      case (None, Some(vr: RawArray @unchecked))                          =>
        new DFilter(Map("$or" -> (value +: vr)))
      case (Some(vl: RawArray @unchecked), None)                          =>
        new DFilter(Map("$or" -> (vl :+ d.value)))
      case (Some(vl: RawArray @unchecked), Some(vr: RawArray @unchecked)) =>
        new DFilter(Map("$or" -> (vl ++ vr)))
      case _                                                              =>
        new DFilter(Map("$or" -> Vector(value, d.value)))
    }
  @silent
  def ! : DFilter             =
    new DFilter(Map("$not" -> value))

  def not: DFilter            = this.!

  def toPaths: Set[Path] =
    getPaths(value, Path.empty).getOrElse(Set.empty)

  private def getPaths(filter: RawObject, segments: Path): Option[Set[Path]] = {
    val paths = filter
      .flatMap {
        case (key, j: RawObject @unchecked) if key.startsWith("$") =>
          getPaths(j, segments)
        case (key, j: RawObject @unchecked)                        =>
          getPaths(j, segments \ key)
        case ("$and" | "$or", j: RawArray @unchecked)              =>
          j.iterator.collect { case m: RawObject @unchecked => getPaths(m, segments) }.flatten
        case (key, _) if key.startsWith("$")                       =>
          Some(Set(segments))
        case (key, _)                                              =>
          Some(Set(segments \ key))
        case _                                                     =>
          None
      }
      .reduce(_ ++ _)
    if (paths.isEmpty) None
    else Some(paths)
  }
}

object DFilter {

  //TODO confirm is valid query structure
  def apply(values: (String, Data)*): DFilter            =
    new DFilter(values.iterator.map(p => p._1 -> p._2.value).toMap)

  private[dsentric] def apply(value: RawObject): DFilter =
    new DFilter(value)

  val empty = new DFilter(Map.empty)
}
