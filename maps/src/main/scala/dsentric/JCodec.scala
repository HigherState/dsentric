package dsentric

/**
  * Created by Jamie Pullar on 20/04/2016.
  */
trait JCodec[T] {
  def apply(t:T):Any
  def unapply(a:Any):Option[T]
}

private sealed trait DirectCodec[T] extends JCodec[T]{
  def apply(t:T):Any = t

  def unapply(a:Any):Option[T] =
    if (isMatch(a)) Some(a.asInstanceOf[T])
    else None

  protected def isMatch(a:Any):Boolean

}

private sealed trait NumericCodec[T] extends JCodec[T] {
  def apply(t:T):Any = t
}

trait DefaultCodecs {
  implicit val mapCodec:JCodec[Map[String, Any]] =
    new DirectCodec[Map[String, Any]] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[Map[String, Any]@unchecked]
    }
}

trait PessimisticCodecs extends DefaultCodecs {

  implicit val stringCodec:JCodec[String] = new DirectCodec[String] {
    protected def isMatch(a: Any): Boolean =
      a.isInstanceOf[String]
  }
  implicit val booleanCodec:JCodec[Boolean] = new DirectCodec[Boolean] {
    protected def isMatch(a: Any): Boolean =
      a.isInstanceOf[Boolean]
  }
  implicit val intCodec:JCodec[Int] = new NumericCodec[Int] {
    def unapply(a: Any): Option[Int] =
      NumericPartialFunctions.int.lift(a)
  }


}

object DefaultCodecs extends DefaultCodecs

object PessimisticCodecs extends PessimisticCodecs
