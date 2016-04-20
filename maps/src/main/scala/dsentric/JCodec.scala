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

trait PessimisticCodecs {

  implicit val stringCodec:JCodec[String] = new DirectCodec[String] {
    protected def isMatch(a: Any): Boolean =
      a.isInstanceOf[String]
  }
}

object PessimisticCodecs extends PessimisticCodecs
