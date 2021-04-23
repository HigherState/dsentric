package dsentric

import scala.util.Try

/**
 * this is a cons style datastructure where the beginning of the path
 * is the first element
 * It is optimised for traversals to a point.
 *
 * For Example
 *
 *  first\2\third
 *  would read
 *  PathKey(first, PathIndex(2, PathKey(third, PathEnd) ) )
 *
 */
sealed trait Path {
  def isEmpty:Boolean

  /**
   * Returns true if all path members are expected
   * @return
   */
  def isExpected: Boolean
  def ::(key:String):Path =
    PathKey(key, this)
  def ::(index:Int):Path =
    PathIndex(index, this)
  def ++ (path:Path):Path =
    this match {
      case PathEnd =>
        path
      case PathKey(key, tail) =>
        PathKey(key, tail ++ path)
      case PathIndex(index, tail) =>
        PathIndex(index, tail ++ path)
    }
  def \(key:String):Path =
    this match {
      case PathEnd =>
        PathKey(key, PathEnd)
      case PathKey(key2, tail) =>
        PathKey(key2, tail \ key)
      case PathIndex(index, tail) =>
        PathIndex(index, tail \ key)
    }
  def \(index:Int):Path =
    this match {
      case PathEnd =>
        PathIndex(index, PathEnd)
      case PathKey(key, tail) =>
        PathKey(key, tail \ index)
      case PathIndex(index2, tail) =>
        PathIndex(index2, tail \ index)
    }

  def hasSubPath(path:Path):Boolean =
    _hasSubPath(this -> path)

  def take(index:Int):Path


  override def toString: String =
    this match {
      case PathEnd =>
        ""
      case PathKey(key, tail) =>
        "\\" + key + tail
      case PathIndex(index, tail) =>
        "\\" + index + tail
    }

  private def _hasSubPath:Function[(Path, Path), Boolean] = {
    case (_, PathEnd) =>
      true
    case (PathKey(key1, tail1), PathKey(key2, tail2)) =>
     key1 == key2 &&  _hasSubPath(tail1 -> tail2)
    case (PathIndex(index1, tail1), PathIndex(index2, tail2)) =>
      index1 == index2 && _hasSubPath(tail1 -> tail2)
    case _ =>
      false
  }

  def toList:List[Either[Int, String]] =
    this match {
      case PathEnd => Nil
      case PathKey(key, tail) =>
        scala.collection.immutable.::(Right(key), tail.toList)
      case PathIndex(index, tail) =>
        scala.collection.immutable.::(Left(index), tail.toList)
    }

}

case object PathEnd extends Path {
  def isEmpty: Boolean = true
  def isExpected: Boolean = true
  def take(index:Int):Path = PathEnd
}


sealed case class PathKey(key:String, next:Path) extends Path {
  def isEmpty:Boolean = false

  def isExpected: Boolean = false

  def take(index:Int):Path =
    if (index <= 0) PathEnd
    else PathKey(key, next.take(index - 1))
}

private[dsentric] final class ExpectedPathKey(override val key:String, override val next:Path) extends PathKey(key, next) {
  override def isExpected: Boolean = next.isExpected

  override def take(index:Int):Path =
    if (index <= 0) PathEnd
    else new ExpectedPathKey(key, next.take(index - 1))
}

final case class PathIndex(index:Int, next:Path) extends Path {
  def isEmpty:Boolean = false

  def isExpected: Boolean = false

  def take(index:Int):Path =
    if (index <= 0) PathEnd
    else PathIndex(index, next.take(index - 1))
}

object Path {
  type Mix = Int with String
  val empty:Path = PathEnd

  def apply[T >: Mix](s:T*):Path =
    s.foldRight(Path.empty) {
      case (i:Int, a) =>
        PathIndex(i, a)
      case (s:String, a) =>
        PathKey(s, a)
      case (_, a) =>
        a
    }

  //TODO handle chars \ " etc
  def fromString(s:String):Path =
    s.split('\\').filterNot(_.isEmpty).foldRight(Path.empty){ (s, a) =>
      Try(s.toInt).map(PathIndex(_, a)).getOrElse(PathKey(s, a))
    }
}


