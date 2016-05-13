package dsentric

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ListBuffer


trait JCodec[T] {
  def apply(t:T):Json
  def unapply(a:Any):Option[T]
}

trait JValueCodec[T] extends JCodec[T] {
  override def apply(t:T):JValue
}

trait JObjectCodec[T] extends JCodec[T] {
  override def apply(t:T):JObject
}

trait JArrayCodec[T] extends JCodec[T] {
  override def apply(t:T):JArray
}



//Use only if T is stored as is in Map


private sealed trait MatchCodec[T] extends JCodec[T] {

  def unapply(a:Any):Option[T] =
    if (isMatch(a)) Some(a.asInstanceOf[T])
    else None

  protected def isMatch(a:Any):Boolean
}

private sealed trait DirectCodec[T] extends JValueCodec[T] {
  override def apply(t: T): JValue =
    new JValue(t)
}

trait DefaultCodecs {
  //Breaks JObject so not implicit
//  val mapCodec:JObjectCodec[Map[String, Any]] =
//    new MatchCodec[Map[String, Any]] with JObjectCodec[Map[String, Any]] {
//
//      override def apply(t: Map[String, Any]): JObject =
//        new JObject(t)
//
//      protected def isMatch(a: Any): Boolean =
//        a.isInstanceOf[Map[String, Any]@unchecked]
//    }
//
//  val vectorCodec:JArrayCodec[Vector[Any]] =
//    new MatchCodec[Vector[Any]] with JArrayCodec[Vector[Any]] {
//
//      protected def isMatch(a: Any): Boolean =
//        a.isInstanceOf[Vector[Any]@unchecked]
//    }

  implicit val jsonCodec:JCodec[Json] =
    new JCodec[Json] {
      def unapply(a: Any): Option[Json] =
        a match {
          case a:Map[String, Any]@unchecked =>
            Some(new JObject(a))
          case v:Vector[Any]@unchecked =>
            Some(new JArray(v))
          case j =>
            Some(new JValue(j))
        }
      def apply(t: Json): Json =
        t
    }

  implicit val jObjectCodec:JObjectCodec[JObject] =
    new JObjectCodec[JObject] {
      def apply(t: JObject): JObject =
        t

      def unapply(a: Any): Option[JObject] =
        a match {
          case m:Map[String, Any]@unchecked =>
            Some(new JObject(m))
          case _ =>
            None
        }
    }

  implicit val jArrayCodec:JArrayCodec[JArray] =
    new JArrayCodec[JArray] {
      def apply(t: JArray): JArray =
        t

      def unapply(a: Any): Option[JArray] =
        a match {
          case v:Vector[Any]@unchecked =>
            Some(new JArray(v))
          case _ =>
            None
        }
    }

  implicit val jNullCodec:JCodec[JNull] =
    new MatchCodec[JNull] with DirectCodec[JNull] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[JNull]
    }
}

trait PessimisticCodecs extends DefaultCodecs {

  implicit val stringCodec:JValueCodec[String] =
    new DirectCodec[String] with MatchCodec[String] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[String]
    }
  implicit val booleanCodec:JValueCodec[Boolean] =
    new DirectCodec[Boolean] with MatchCodec[Boolean] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[Boolean]
    }
  implicit val longCodec:JValueCodec[Long] =
    new DirectCodec[Long] {
      def unapply(a: Any): Option[Long] =
        NumericPartialFunctions.long.lift(a)
    }
  implicit val doubleCodec:JValueCodec[Double] =
    new DirectCodec[Double] {
      def unapply(a: Any): Option[Double] =
        NumericPartialFunctions.double.lift(a)
    }

  implicit val intCodec:JValueCodec[Int] =
    new JValueCodec[Int] {
      def apply(t: Int): JValue =
        new JValue(t.toLong)
      def unapply(a: Any): Option[Int] =
        NumericPartialFunctions.int.lift(a)
    }
  implicit val shortCodec:JValueCodec[Short] =
    new JValueCodec[Short] {
      def apply(t: Short): JValue =
        new JValue(t.toLong)
      def unapply(a: Any): Option[Short] =
        NumericPartialFunctions.short.lift(a)
    }
  implicit val byteCodec:JValueCodec[Byte] =
    new JValueCodec[Byte] {
      def apply(t: Byte): JValue =
        new JValue(t.toLong)
      def unapply(a: Any): Option[Byte] =
        NumericPartialFunctions.byte.lift(a)
    }
  implicit val floatCodec:JValueCodec[Float] =
    new JValueCodec[Float] {
      def apply(t: Float): JValue =
        new JValue(t.toDouble)
      def unapply(a: Any): Option[Float] =
        NumericPartialFunctions.float.lift(a)
    }

  implicit def listCodec[T](implicit C:JCodec[T]):JArrayCodec[List[T]] =
    new JArrayCodec[List[T]] {
      def apply(t: List[T]): JArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new JArray(t.toVector)
        else
          new JArray(t.map(C.apply(_).value).toVector)

      def unapply(a: Any): Option[List[T]] =
        a match {
          case s:Vector[Any]@unchecked =>
            s.toIterator.map(C.unapply).foldLeft[Option[ListBuffer[T]]](Some(new ListBuffer[T])){
              case (Some(lb), Some(t)) => Some(lb += t)
              case _ => None
            }.map(_.result())
          case _ =>
            None
        }
    }

  implicit def vectorCodec[T](implicit C:JCodec[T]):JArrayCodec[Vector[T]] =
    new JArrayCodec[Vector[T]] {
      def apply(t: Vector[T]): JArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new JArray(t)
        else
          new JArray(t.map(C.apply(_).value))

      def unapply(a: Any): Option[Vector[T]] =
        a match {
          case s:Vector[Any]@unchecked =>
            s.toIterator.map(C.unapply).foldLeft[Option[VectorBuilder[T]]](Some(new VectorBuilder[T])){
              case (Some(vb), Some(t)) => Some(vb += t)
              case _ => None
            }.map(_.result())
          case _ =>
            None
        }
    }
}

object DefaultCodecs extends DefaultCodecs

object PessimisticCodecs extends PessimisticCodecs
