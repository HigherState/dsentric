package dsentric


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
}

object Failures {
  val empty = Vector.empty[(Path, String)]

  def apply(elems:(Path, String)*) =
    Vector.apply(elems:_*)
}
