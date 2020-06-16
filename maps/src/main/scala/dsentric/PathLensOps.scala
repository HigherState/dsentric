package dsentric

import dsentric.failure.ValidResult
import scala.annotation.tailrec

object PathLensOps {

  /**
   * Return value found at end of traversal if present
   * @param map
   * @param path
   * @return
   */
  @tailrec
  private[dsentric] final def traverse(map:RawObject, path:Path):Option[Raw] =
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

  /**
   * Set value at end of traversal, will create objects as required to
   * instantiate path if path not complete.
   * Will also replace value if they appear in the path.
   * @param map
   * @param path
   * @param value
   * @return
   */
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


  /**
   * Transforms the value in the position, will create path if necessary.
   * If there is no change, will return None
   * Will clear out empty objects along the path if they should occur.
   * @param map
   * @param path
   * @param f
   * @return
   */
  private[dsentric] def transform(map:Map[String, Any], path:Path)(f:Option[Any] => ValidResult[Option[Any]]):ValidResult[Option[Map[String, Any]]] = {
    path match {
      case PathKey(head, PathEnd) =>
        val v = map.get(head)
        f(v).flatMap { t =>
          if (t == v)
            ValidResult.none
          else
            ValidResult.success(Some(t.fold(map - head)(value => map + (head -> value))))
        }
      case PathKey(head, tail@PathKey(_, _)) =>
        val child = map
          .get(head)
          .collect{case m:RawObject@unchecked => m}
          .getOrElse(Map.empty[String, Any])
        transform(child, tail)(f)
          .map(_.map{
            case m if m.isEmpty =>
              map - head
            case m =>
              map + (head -> m)
          })
      case _ =>
        ValidResult.none
    }
  }

  /**
   * Removes the value at target site.
   * Will clear out empty objects along the path if they occur.
   * Will not replace a value result if found in the path.
   * @param map
   * @param path
   * @return
   */
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
          case _ => RawObject.empty
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

