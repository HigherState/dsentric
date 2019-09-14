package dsentric

class PatternMatcher[D <: DObject, T](unapplyFunction:Function[D, Option[T]]) {
  def unapply(j:D):Option[T] =
    unapplyFunction(j)
}
