package dsentric

sealed trait Matcher  {
  def apply(j:Any):Boolean
  protected def default:Any
}

object ExistenceMatcher extends Matcher {
  def apply(j:Any):Boolean = true
  protected def default:Any = DNull
}

case class ValueMatcher[T](value:T)(implicit _codec:DCodec[T]) extends Matcher {
  val default: Any = _codec(value).value
  def apply(j: Any): Boolean = j == default
}

trait DataMatchers {
  implicit def valueMatcher[T](value:T)(implicit _codec:DCodec[T]) =
    ValueMatcher(value)
}
