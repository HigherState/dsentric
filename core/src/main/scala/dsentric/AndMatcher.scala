package dsentric

trait AndMatcher {
  object && {
    def unapply[A](a: A) = Some((a, a))
  }
}
object AndMatcher extends AndMatcher