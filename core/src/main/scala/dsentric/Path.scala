package dsentric

object Path {
  val empty = List.empty[Either[Int, String]]

  def apply(s:String*):Path =
    s.map(Right.apply).toList
}

object Failures {
  val empty = Vector.empty[(Path, String)]

  def apply(elems:(Path, String)*) =
    Vector.apply(elems:_*)
}
