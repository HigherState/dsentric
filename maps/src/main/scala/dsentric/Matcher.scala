package dsentric

import dsentric.codecs.DCodec

sealed trait Matcher  {
  def apply(j:Raw):Boolean
  protected def default:Any
}

object ExistenceMatcher extends Matcher {
  def apply(j:Raw):Boolean = true
  protected def default:Any = DNull

}

case class ValueMatcher[T](value:T)(implicit _codec:DCodec[T]) extends Matcher {
  val default: Raw = _codec(value)
  def apply(j: Raw): Boolean = j == default
}

trait DataMatchers {
  implicit def valueMatcher[T](value:T)(implicit _codec:DCodec[T]): ValueMatcher[T] =
    ValueMatcher(value)
}

class MatcherUnapply private[dsentric](key: String, matcher:Matcher) extends ApplicativeMatcher[DObject] {
  def unapply(j:DObject):Boolean = {
    j.value
      .get(key)
      .fold(false) { v => matcher(v) }
  }
}
