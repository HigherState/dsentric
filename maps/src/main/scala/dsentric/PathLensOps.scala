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

  /*
  Returns None if no change
   */
  private[dsentric] def modify[T](map:RawObject, path:Path, f:Raw => CodecResult[Raw]):Option[RawObject] =
    path match {
      case PathKey(head, PathEnd) =>
        for {
          v <- map.get(head)
          a = f(v)
          r <- a match {
            case DCodeFailure =>
              None
            case DCodeSuccess(t) if t == v =>
              None
            case DCodeSuccess(t) =>
              Some(t)
          }
        } yield map + (head -> r)

      case PathKey(head, tail@PathKey(_, _)) =>
         map
           .get(head)
           .collect{case m:RawObject@unchecked => m}
           .flatMap(modify(_, tail, f))
           .map(t => map + (head -> t))

      case _ =>
        None
    }

  //Can create nested objects
  //f result is None if codec failed to match
  private[dsentric] def maybeModify[T](map:RawObject, path:Path, f:Option[Raw] => CodecResult[Raw]):Option[RawObject] =
    path match {
      case PathKey(head, PathEnd) =>
        val v = map.get(head)
        f(v) match {
          case DCodeFailure =>
            None
          case DCodeSuccess(t) if v.contains(t) =>
            None
          case DCodeSuccess(t) =>
            Some(map + (head -> t))
        }

      case PathKey(head, tail@PathKey(_, _)) =>
        val child =
          map
            .get(head)
            .collect{case m:RawObject@unchecked => m}.getOrElse(RawObject.empty)

        maybeModify(child, tail, f)
          .map(v => map + (head -> v))
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

  //f is None if codec failed to match, next None is to drop
  private[dsentric] def maybeModifyOrDrop[T](map:RawObject, path:Path, f:Option[Raw] => PathResult[Raw]):Option[RawObject] =
    path match {
      case PathKey(head, PathEnd) =>
        val v = map.get(head)
        f(v) match {
          case Empty =>
            v.map(_ => map - head) //if v is empty then there is no change
          case DCodeFailure => // failed to decode
            None
          case DCodeSuccess(t) if v.contains(t) => // value is the same
            None
          case DCodeSuccess(t) =>
            Some(map + (head -> t))
        }

      case PathKey(head, tail@PathKey(_, _)) =>
        val child =
          map
            .get(head)
            .collect{case m:RawObject@unchecked => m}.getOrElse(RawObject.empty)

        maybeModifyOrDrop(child, tail, f)
          .map{
            case m if m.isEmpty =>
              map - head
            case m =>
              map + (head -> m)
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

