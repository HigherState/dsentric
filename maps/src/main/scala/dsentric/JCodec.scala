package dsentric


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
  //Breaks JObject so not implicit
  val mapCodec:JCodec[Map[String, Any]] =
    new DirectCodec[Map[String, Any]] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[Map[String, Any]@unchecked]
    }

  implicit val jobjectCodec:JCodec[JObject] =
    new JCodec[JObject] {
      def apply(t: JObject): Any = t.value

      def unapply(a: Any): Option[JObject] =
        a match {
          case m:Map[String, Any]@unchecked =>
            Some(JObject(m))
          case _ =>
            None
        }
    }

  implicit val jNullCodec:JCodec[JNull] =
    new DirectCodec[JNull] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[JNull]
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
