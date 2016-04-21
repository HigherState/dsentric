package dsentric

import scala.annotation.tailrec

object PathOps {

  @tailrec
  def traverse(map:Map[String, Any], path:Path):Option[Any] =
    path match {
      case Right(head) :: tail =>
        if (tail.isEmpty) map.get(head)
        else
          map.get(head)
            .collect{case m:Map[String, Any]@unchecked => m} match {
              case None => None
              case Some(m) => traverse(m, tail)
            }
      case _ =>
        Some(map)
    }

  def set(map:Map[String, Any], path:Path, value:Any):Map[String, Any] =
    path match {
      case Right(head) :: Nil =>
        map + (head -> value)
      case Right(head) :: (tail@(Right(_) :: _)) =>
        val child = map.get(head)
          .collect{case m:Map[String, Any]@unchecked => m}.getOrElse(Map.empty[String, Any])
        map + (head -> set(child, tail, value))
      case _ =>
        map
    }

  def modify[T](map:Map[String, Any], path:Path, codec:JCodec[T], f:T => T):Option[Map[String, Any]] =
    path match {
      case Right(head) :: Nil =>
        map.get(head)
          .flatMap(codec.unapply)
          .map(t => map + (head -> codec(f(t))))
      case Right(head) :: (tail@(Right(_) :: _)) =>
         map.get(head)
          .collect{case m:Map[String, Any]@unchecked => m}
          .flatMap(modify(_, tail, codec, f))
          .map(t => map + (head -> t))
      case _ =>
        None
    }

}
