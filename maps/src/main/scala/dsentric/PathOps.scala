package dsentric

import scala.annotation.tailrec

object PathOps {

  @tailrec
  def traverse(map:Map[String, Any], path:Path):Option[Any] =
    path match {
      case head :: tail =>
        head match {
          case Left(_) => None
          case Right(p) =>
            if (tail.isEmpty) map.get(p)
            else
              map.get(p)
                .collect{case m:Map[String, Any]@unchecked => m} match {
                case None => None
                case Some(m) => traverse(m, tail)
              }
        }
      case Nil =>
        Some(map)
    }

}
