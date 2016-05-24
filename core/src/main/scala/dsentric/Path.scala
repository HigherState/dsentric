package dsentric

import scala.util.Try


object Path {
  type Mix = Int with String
  val empty = List.empty[Either[Int, String]]

  def apply[T >: Mix](s:T*):Path =
    s.collect {
      case i:Int =>
        Left(i)
      case s:String =>
        Right(s)
    }.toList

  //TODO handle chars \ " etc
  def fromString(s:String):Path =
    s.split('\\').map{s =>
      Try(s.toInt).map(Left(_)).getOrElse(Right(s))
    }.toList
}

object Failures {
  val empty = Vector.empty[(Path, String)]

  def apply(elems:(Path, String)*) =
    Vector.apply(elems:_*)
}

final class PathOps(val self:Path) extends AnyVal {

  def \(part:String):Path = self ++ List[Either[Int, String]](Right(part))
  def \(part:Int):Path = self ++List[Either[Int, String]](Left(part))

  def hasSubPath(path:Path) =
    path.zip(self).foldLeft(true) {
      case (a, (s, p)) =>  a && s == p
    }
}

trait ToPathOps {
  implicit def toPathOps(path:Path):PathOps =
    new PathOps(path)
}

