package dsentric

import scala.annotation.tailrec

object PathOps {

  @tailrec
  private[dsentric] final def traverse(map:Map[String, Any], path:Path):Option[Any] =
    path match {
      case Right(head) :: tail =>
        if (tail.isEmpty) map.get(head)
        else
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
      case Right(head) :: Nil =>
        map + (head -> value)
      case Right(head) :: (tail@(Right(_) :: _)) =>
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
  private[dsentric] def modify[T](map:Map[String, Any], path:Path, codec:JCodec[T], f:T => T):Option[Map[String, Any]] =
    path match {
      case Right(head) :: Nil =>
        for {
          v <- map.get(head)
          t <- codec.unapply(v)
          a = codec(f(t))
          r <- if (a == v) None else Some(a) //return None if same value
        } yield map + (head -> r)

      case Right(head) :: (tail@(Right(_) :: _)) =>
         map
           .get(head)
           .collect{case m:Map[String, Any]@unchecked => m}
           .flatMap(modify(_, tail, codec, f))
           .map(t => map + (head -> t))

      case _ =>
        None
    }

  //Can create nested objects
  private[dsentric] def maybeModify[T](map:Map[String, Any], path:Path, codec:JCodec[T], strictness:Strictness, f:Option[T] => T):Option[Map[String, Any]] =
    path match {
      case Right(head) :: Nil =>
        map.get(head) match {
          case None =>
            Some(map + (head -> codec(f(None))))
          case Some(v) =>
            for {
              t <- strictness(v, codec)
              a = codec(f(t))
              r <- if (t.contains(a)) None else Some(a)
            } yield map + (head -> r)
        }

      case Right(head) :: (tail@(Right(_) :: _)) =>
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
      case Right(head) :: Nil =>
        if (map.contains(head)) Some(map - head)
        else None

      case Right(head) :: (tail@(Right(_) :: _)) =>
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

  private[dsentric] def maybeModifyOrDrop[T](map:Map[String, Any], path:Path, codec:JCodec[T], strictness:Strictness, f:Option[T] => Option[T]):Option[Map[String, Any]] =
    path match {
      case Right(head) :: Nil =>
        map.get(head) match {
          case None =>
            f(None)
              .map(codec.apply)
              .map(v => map + (head -> v))

          case Some(v) =>
            strictness(v, codec).map { t =>
              f(t).map(codec.apply).fold(map - head){r => map + (head -> r)}
            }
        }

      case Right(head) :: (tail@(Right(_) :: _)) =>
        val child =
          map
            .get(head)
            .collect{case m:Map[String, Any]@unchecked => m}.getOrElse(Map.empty[String, Any])

        maybeModifyOrDrop(child, tail, codec, strictness, f)
          .map(v => map + (head -> v))
      case _ =>
        None
    }
}

