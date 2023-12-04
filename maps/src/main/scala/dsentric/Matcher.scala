package dsentric

sealed trait Matcher {
  def apply(j: Raw): Boolean
  protected def default: Any
}

object ExistenceMatcher extends Matcher {
  def apply(j: Raw): Boolean = true
  protected def default: Any = DNull

}

class MatcherUnapply private[dsentric] (key: String, matcher: Matcher) extends ApplicativeMatcher[DObject] {
  def unapply(j: DObject): Boolean =
    j.value
      .get(key)
      .fold(false)(v => matcher(v))
}
