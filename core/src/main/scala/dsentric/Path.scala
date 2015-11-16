package dsentric

object Path {
  val empty = Vector.empty[Either[Int, String]]

  def apply(s:String*):Path =
    s.map(Right.apply).toVector
}

object Failures {
  val empty = Vector.empty[(Path, String)]

  def apply(elems:(Path, String)*) =
    Vector.apply(elems:_*)
}
