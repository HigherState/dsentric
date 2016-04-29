package dsentric

import scala.collection.mutable.ListBuffer


trait JCodec[T] {
  def apply(t:T):Any
  def unapply(a:Any):Option[T]
}

//Use only if T is stored as is in Map
trait DirectCodec[T] extends JCodec[T]{
  def apply(t:T):Any = t
}

private sealed trait MatchCodec[T] extends DirectCodec[T] {

  def unapply(a:Any):Option[T] =
    if (isMatch(a)) Some(a.asInstanceOf[T])
    else None

  protected def isMatch(a:Any):Boolean
}

trait DefaultCodecs {
  //Breaks JObject so not implicit
  val mapCodec:JCodec[Map[String, Any]] =
    new MatchCodec[Map[String, Any]] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[Map[String, Any]@unchecked]
    }

  implicit val jobjectCodec:JCodec[JObject] =
    new JCodec[JObject] {
      def apply(t: JObject): Any =
        t.value

      def unapply(a: Any): Option[JObject] =
        a match {
          case m:Map[String, Any]@unchecked =>
            Some(new JObject(m))
          case _ =>
            None
        }
    }

  implicit val jNullCodec:JCodec[JNull] =
    new MatchCodec[JNull] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[JNull]
    }

}

trait PessimisticCodecs extends DefaultCodecs {

  implicit val stringCodec:JCodec[String] =
    new MatchCodec[String] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[String]
    }
  implicit val booleanCodec:JCodec[Boolean] =
    new MatchCodec[Boolean] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[Boolean]
    }
  implicit val intCodec:JCodec[Int] =
    new DirectCodec[Int] {
      def unapply(a: Any): Option[Int] =
        NumericPartialFunctions.int.lift(a)
    }

  implicit val doubleCodec:JCodec[Double] =
    new DirectCodec[Double] {
      def unapply(a: Any): Option[Double] =
        NumericPartialFunctions.double.lift(a)
    }

  implicit def listCodec[T](implicit C:JCodec[T]):JCodec[List[T]] =
    new JCodec[List[T]] {
      def apply(t: List[T]): Any =
        if (C.isInstanceOf[DirectCodec[T]])
          t
        else
          t.map(C.apply)

      def unapply(a: Any): Option[List[T]] =
        a match {
          case s:Iterable[Any]@unchecked =>
            s.toIterator.map(C.unapply).foldLeft[Option[ListBuffer[T]]](Some(new ListBuffer[T])){
              case (Some(lb), Some(t)) => Some(lb += t)
              case _ => None
            }.map(_.result())
          case _ =>
            None
        }
    }
}

object DefaultCodecs extends DefaultCodecs

object PessimisticCodecs extends PessimisticCodecs
