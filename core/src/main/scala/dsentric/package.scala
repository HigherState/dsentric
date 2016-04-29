package object dsentric {

  type Numeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type Length = String with Iterable[Nothing]
  type Optionable[T] = T with Option[T]

  type Path = List[Either[Int, String]]
  type Failures = Vector[(Path, String)]

  implicit class PathExt(val self:Path) extends AnyVal {

    def \(part:String) = self :+ Left(part)
    def \(part:Int) = self :+ Right(part)

    def hasSubPath(path:Path) =
      path.zip(self).foldLeft(true) {
        case (a, (s, p)) =>  a && s == p
      }
  }

  implicit class StringExt(val self:String) extends AnyVal {
    def \(part:String):Path = List(Right(self), Right(part))
    def \(part:Int):Path = List(Right(self), Left(part))
  }

  implicit class IntExt(val self:Int) extends AnyVal {
    def \(part:String):Path = List(Left(self), Right(part))
    def \(part:Int):Path = List(Left(self), Left(part))
  }
}
