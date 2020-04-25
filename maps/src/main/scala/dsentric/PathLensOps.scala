package dsentric

import scala.annotation.tailrec

object PathLensOps {

  @tailrec
  private[dsentric] final def traverse(map:RawObject, path:Path):Option[Any] =
    path match {
      case PathKey(head, PathEnd) =>
        map.get(head)
      case PathKey(head, tail) =>
        map
          .get(head)
          .collect{case m:RawObject@unchecked => m} match {
            case None => None
            case Some(m) => traverse(m, tail)
          }
      case _ =>
        Some(map)
    }

  private[dsentric] def set(map:RawObject, path:Path, value:Raw):RawObject =
    path match {
      case PathKey(head, PathEnd) =>
        map + (head -> value)
      case PathKey(head, tail@PathKey(_, _)) =>
        val child = map
          .get(head)
          .collect{case m:RawObject@unchecked => m}.getOrElse(Map.empty[String, Any])
        map + (head -> set(child, tail, value))
      case _ =>
        map
    }
  //Should handle wrong type - remember for Dsentric1
  private[dsentric] def setIfEmpty(map:Map[String, Any], path:Path, value:Any):Option[Map[String, Any]] =
    path match {
      case PathKey(head, PathEnd) =>
        if (map.contains(head)) None
        else Some(map + (head -> value))

      case PathKey(head, tail@PathKey(_, _)) =>
        map
          .get(head)
          .collect{case m:Map[String, Any]@unchecked => m} match {
            case None =>
              Some(map + (head -> set(Map.empty, tail, value)))
            case Some(m) =>
              setIfEmpty(m, tail, value)
                .map(t => map + (head -> t))
          }
      case _ =>
        None
    }

  private[dsentric] def setIfNonEmpty(map:Map[String, Any], path:Path, value:Any):Option[Map[String, Any]] =
    path match {
      case PathKey(head, PathEnd) =>
        if (!map.contains(head)) None
        else Some(map + (head -> value))

      case PathKey(head, tail@PathKey(_, _)) =>
        map
          .get(head)
          .flatMap {
            case m: Map[String, Any]@unchecked =>
              setIfNonEmpty(m, tail, value)
                .map(t => map + (head -> t))
            case _ =>
              None
          }
      case _ =>
        None
    }

  //returns none if no change
  private[dsentric] def drop(map:RawObject, path:Path):Option[RawObject] =
    path match {
      case PathKey(head, PathEnd) =>
        if (map.contains(head)) Some(map - head)
        else None

      case PathKey(head, tail@PathKey(_, _)) =>
        map
          .get(head)
          .flatMap{
            case child:RawObject@unchecked =>
              drop(child, tail)
                .map{
                  case m if m.isEmpty =>
                    map - head
                  case m =>
                    map + (head -> m)
                }
            case _ =>
              None
          }
      case _ =>
        None
    }

  private[dsentric] def pathToMap(path:Path, value:Raw):RawObject = {
    path match {
      case PathEnd =>
        value match {
          case m:RawObject@unchecked => m
          case _ => Map.empty
        }
      case PathKey(last, PathEnd) =>
        Map(last -> value)
      case PathKey(last, tail) =>
        Map(last -> pathToMap(tail, value))
      case PathIndex(_, _) =>
        pathToMap(PathEnd, value)
    }
  }
}

