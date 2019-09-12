package dsentric

sealed trait Matcher  {
  def apply(j:Raw):Boolean
  protected def default:Any
}

object ExistenceMatcher extends Matcher {
  def apply(j:Raw):Boolean = true
  protected def default:Any = Dsentric.dNull
}

case class ValueMatcher[T](value:T)(implicit _codec:DCodec[T]) extends Matcher {
  val default: Raw = _codec(value).value
  def apply(j: Raw): Boolean = j == default
}

trait DataMatchers {
  implicit def valueMatcher[T](value:T)(implicit _codec:DCodec[T]): ValueMatcher[T] =
    ValueMatcher(value)
}
