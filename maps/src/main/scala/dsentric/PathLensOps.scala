package dsentric

import scala.annotation.tailrec

object PathLensOps {

  @tailrec
  private[dsentric] final def traverse(map:Map[String, Any], path:Path):Option[Any] =
    path match {
      case PathKey(head, PathEnd) =>
        map.get(head)
      case PathKey(head, tail) =>
        map
          .get(head)
          .collect{case m:Map[String, Any]@unchecked => m} match {
            case None => None
            case Some(m) => traverse(m, tail)
          }
      case _ =>
        Some(map)
    }

  private[dsentric] def set(map:Map[String, Any], path:Path, value:Any):Map[String, Any] =
    path match {
      case PathKey(head, PathEnd) =>
        map + (head -> value)
      case PathKey(head, tail@PathKey(_, _)) =>
        val child = map
          .get(head)
          .collect{case m:Map[String, Any]@unchecked => m}.getOrElse(Map.empty[String, Any])
        map + (head -> set(child, tail, value))
      case _ =>
        map
    }

  /*
  Returns None if no change
   */
  private[dsentric] def modify[T](map:Map[String, Any], path:Path, codec:DCodec[T], f:T => T):Option[Map[String, Any]] =
    path match {
      case PathKey(head, PathEnd) =>
        for {
          v <- map.get(head)
          t <- codec.unapply(v)
          a = codec(f(t)).value
          r <- if (a == v) None else Some(a) //return None if same value
        } yield map + (head -> r)

      case PathKey(head, tail@PathKey(_, _)) =>
         map
           .get(head)
           .collect{case m:Map[String, Any]@unchecked => m}
           .flatMap(modify(_, tail, codec, f))
           .map(t => map + (head -> t))

      case _ =>
        None
    }

  //Can create nested objects
  private[dsentric] def maybeModify[T](map:Map[String, Any], path:Path, codec:DCodec[T], strictness:Strictness, f:Option[T] => T):Option[Map[String, Any]] =
    path match {
      case PathKey(head, PathEnd) =>
        map.get(head) match {
          case None =>
            Some(map + (head -> codec(f(None)).value))
          case Some(v) =>
            for {
              t <- strictness(v, codec)
              a = codec(f(t)).value
              r <- if (t.contains(a)) None else Some(a)
            } yield map + (head -> r)
        }

      case PathKey(head, tail@PathKey(_, _)) =>
        val child =
          map
            .get(head)
            .collect{case m:Map[String, Any]@unchecked => m}.getOrElse(Map.empty[String, Any])

        maybeModify(child, tail, codec, strictness, f)
            .map(v => map + (head -> v))
      case _ =>
        None
    }

  private[dsentric] def drop(map:Map[String, Any], path:Path):Option[Map[String, Any]] =
    path match {
      case PathKey(head, PathEnd) =>
        if (map.contains(head)) Some(map - head)
        else None

      case PathKey(head, tail@PathKey(_, _)) =>
        map
          .get(head)
          .flatMap{
            case child:Map[String, Any]@unchecked =>
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

  private[dsentric] def maybeModifyOrDrop[T](map:Map[String, Any], path:Path, codec:DCodec[T], strictness:Strictness, f:Option[T] => Option[T]):Option[Map[String, Any]] =
    path match {
      case PathKey(head, PathEnd) =>
        map.get(head) match {
          case None =>
            f(None)
              .map(codec(_).value)
              .map(v => map + (head -> v))

          case Some(v) =>
            strictness(v, codec).map { t =>
              f(t).map(codec(_).value).fold(map - head){r => map + (head -> r)}
            }
        }

      case PathKey(head, tail@PathKey(_, _)) =>
        val child =
          map
            .get(head)
            .collect{case m:Map[String, Any]@unchecked => m}.getOrElse(Map.empty[String, Any])

        maybeModifyOrDrop(child, tail, codec, strictness, f)
          .map(v => map + (head -> v))
      case _ =>
        None
    }

  private[dsentric] def pathToMap(path:Path, value:Any):Map[String, Any] = {
    path match {
      case PathEnd =>
        value match {
          case m:Map[String,Any]@unchecked => m
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

