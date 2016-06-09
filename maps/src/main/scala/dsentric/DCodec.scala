package dsentric

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ListBuffer

trait DCodec[T] {
  def apply(t:T):Data
  def unapply(a:Any):Option[T]
}

trait DValueCodec[T] extends DCodec[T] {
  override def apply(t:T):DValue
}

trait DObjectCodec[T] extends DCodec[T] {
  override def apply(t:T):DObject
}

trait DArrayCodec[T] extends DCodec[T] {
  override def apply(t:T):DArray
}


trait MatchCodec[T] extends DCodec[T] {

  def unapply(a:Any):Option[T] =
    if (isMatch(a)) Some(a.asInstanceOf[T])
    else None

  protected def isMatch(a:Any):Boolean
}

trait DirectCodec[T] extends DValueCodec[T] {
  override def apply(t: T): DValue =
    new DValue(t)
}

trait DefaultCodecs {

  implicit val dataCodec:DCodec[Data] =
    new DCodec[Data] {
      def unapply(a: Any): Option[Data] =
        a match {
          case a:Map[String, Any]@unchecked =>
            Some(new DObject(a))
          case v:Vector[Any]@unchecked =>
            Some(new DArray(v))
          case j =>
            Some(new DValue(j))
        }
      def apply(t: Data): Data =
        t
    }

  implicit val dQueryCodec:DCodec[DQuery] =
    new DCodec[DQuery] {
      def unapply(a: Any): Option[DQuery] =
        a match {
          case a:Map[String, Any]@unchecked =>
            DQuery(a).toOption
          case j =>
            None
        }
      def apply(t: DQuery): Data =
        new DObject(t.value)
    }

  implicit val dObjectCodec:DObjectCodec[DObject] =
    new DObjectCodec[DObject] {
      def apply(t: DObject): DObject =
        t

      def unapply(a: Any): Option[DObject] =
        a match {
          case m:Map[String, Any]@unchecked =>
            Some(new DObject(m))
          case _ =>
            None
        }
    }

  implicit val dArrayCodec:DArrayCodec[DArray] =
    new DArrayCodec[DArray] {
      def apply(t: DArray): DArray =
        t

      def unapply(a: Any): Option[DArray] =
        a match {
          case v:Vector[Any]@unchecked =>
            Some(new DArray(v))
          case _ =>
            None
        }
    }

  implicit val dNullCodec:DCodec[DNull] =
    new MatchCodec[DNull] with DirectCodec[DNull] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[DNull]
    }

  implicit def tupleCodec[T1,T2](implicit D1:DCodec[T1], D2:DCodec[T2]):DCodec[(T1, T2)] =
    new DArrayCodec[(T1, T2)] {
      override def apply(t: (T1, T2)): DArray =
        new DArray(Vector(D1(t._1).value, D2(t._2).value))

      def unapply(a: Any): Option[(T1, T2)] =
        a match {
          case Vector(D1(t1), D2(t2)) =>
            Some(t1 -> t2)
          case _ =>
            None
        }
    }
}

trait PessimisticCodecs extends DefaultCodecs {

  implicit val stringCodec:DValueCodec[String] =
    new DirectCodec[String] with MatchCodec[String] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[String]
    }
  implicit val booleanCodec:DValueCodec[Boolean] =
    new DirectCodec[Boolean] with MatchCodec[Boolean] {
      protected def isMatch(a: Any): Boolean =
        a.isInstanceOf[Boolean]
    }
  implicit val longCodec:DValueCodec[Long] =
    new DirectCodec[Long] {
      def unapply(a: Any): Option[Long] =
        NumericPartialFunctions.long.lift(a)
    }
  implicit val doubleCodec:DValueCodec[Double] =
    new DirectCodec[Double] {
      def unapply(a: Any): Option[Double] =
        NumericPartialFunctions.double.lift(a)
    }

  implicit val intCodec:DValueCodec[Int] =
    new DValueCodec[Int] {
      def apply(t: Int): DValue =
        new DValue(t.toLong)
      def unapply(a: Any): Option[Int] =
        NumericPartialFunctions.int.lift(a)
    }
  implicit val shortCodec:DValueCodec[Short] =
    new DValueCodec[Short] {
      def apply(t: Short): DValue =
        new DValue(t.toLong)
      def unapply(a: Any): Option[Short] =
        NumericPartialFunctions.short.lift(a)
    }
  implicit val byteCodec:DValueCodec[Byte] =
    new DValueCodec[Byte] {
      def apply(t: Byte): DValue =
        new DValue(t.toLong)
      def unapply(a: Any): Option[Byte] =
        NumericPartialFunctions.byte.lift(a)
    }
  implicit val floatCodec:DValueCodec[Float] =
    new DValueCodec[Float] {
      def apply(t: Float): DValue =
        new DValue(t.toDouble)
      def unapply(a: Any): Option[Float] =
        NumericPartialFunctions.float.lift(a)
    }

  implicit def optionCodec[T](implicit D:DCodec[T]) =
    new DCodec[Option[T]] {
      def apply(t: Option[T]): Data =
        t.fold[Data](Dsentric.dNull)(t => D(t))

      def unapply(a: Any): Option[Option[T]] =
        a match {
          case Dsentric.dNull => Some(None)
          case D(v) => Some(Some(v))
          case _ => None
        }
    }

  implicit def listCodec[T](implicit C:DCodec[T]):DArrayCodec[List[T]] =
    new DArrayCodec[List[T]] {
      def apply(t: List[T]): DArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new DArray(t.toVector)
        else
          new DArray(t.map(C.apply(_).value).toVector)

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

  implicit def vectorCodec[T](implicit C:DCodec[T]):DArrayCodec[Vector[T]] =
    new DArrayCodec[Vector[T]] {
      def apply(t: Vector[T]): DArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new DArray(t)
        else
          new DArray(t.map(C.apply(_).value))

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

  implicit def fixedMapCodec[T](implicit D:DCodec[T]):DCodec[Map[String, T]] =
    if (D.isInstanceOf[DirectCodec[T]])
      new DirectCodec[Map[String, T]] {
        def unapply(a: Any): Option[Map[String, T]] =
          toMapT(a)
      }
    else
      new DCodec[Map[String, T]] {
        def unapply(a: Any): Option[Map[String, T]] =
          toMapT(a)

        def apply(t: Map[String, T]): Data =
          new DValue(t.mapValues(D(_).value))
      }

  private def toMapT[T](a:Any)(implicit D:DCodec[T]) =
    a match {
      case a:Map[String, Any]@unchecked =>
        a.toIterator.foldLeft(Option(Map.newBuilder[String, T])){
          case (Some(m), (k, D(v))) =>
            Some(m += (k -> v))
          case _ =>
            None
        }.map(_.result())
      case _ =>
        None
    }
}

trait OptimisticCodecs extends DefaultCodecs {

  import dsentric.util.ToStringContextOps._

  implicit val stringCodec:DValueCodec[String] =
    new DirectCodec[String] {
      def unapply(a: Any): Option[String] =
        Some(a.toString)
    }

  implicit val booleanCodec:DValueCodec[Boolean] =
    new DirectCodec[Boolean] {
      def unapply(a: Any): Option[Boolean] =
        a match {
          case true | i"true" | 1 => Some(true)
          case false | i"false" | 0 => Some(false)
          case _ => None
        }
    }
  implicit val longCodec:DValueCodec[Long] =
    new DirectCodec[Long] {
      def unapply(a: Any): Option[Long] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.long.lift(a))(NumericPartialFunctions.long.lift)

    }
  implicit val doubleCodec:DValueCodec[Double] =
    new DirectCodec[Double] {
      def unapply(a: Any): Option[Double] =
        NumericPartialFunctions.stringDouble.lift(a)
          .orElse(NumericPartialFunctions.double.lift(a))
    }

  implicit val intCodec:DValueCodec[Int] =
    new DValueCodec[Int] {
      def apply(t: Int): DValue =
        new DValue(t.toLong)
      def unapply(a: Any): Option[Int] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.int.lift(a))(NumericPartialFunctions.int.lift)
    }
  implicit val shortCodec:DValueCodec[Short] =
    new DValueCodec[Short] {
      def apply(t: Short): DValue =
        new DValue(t.toLong)
      def unapply(a: Any): Option[Short] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.short.lift(a))(NumericPartialFunctions.short.lift)
    }
  implicit val byteCodec:DValueCodec[Byte] =
    new DValueCodec[Byte] {
      def apply(t: Byte): DValue =
        new DValue(t.toLong)
      def unapply(a: Any): Option[Byte] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.byte.lift(a))(NumericPartialFunctions.byte.lift)
    }
  implicit val floatCodec:DValueCodec[Float] =
    new DValueCodec[Float] {
      def apply(t: Float): DValue =
        new DValue(t.toDouble)
      def unapply(a: Any): Option[Float] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.float.lift(a))(NumericPartialFunctions.float.lift)
    }

  implicit def optionCodec[T](implicit D:DCodec[T]) =
    new DCodec[Option[T]] {
      def apply(t: Option[T]): Data =
        t.fold[Data](Dsentric.dNull)(t => D(t))

      def unapply(a: Any): Option[Option[T]] =
        a match {
          case Dsentric.dNull => Some(None)
          case D(v) => Some(Some(v))
          case _ => Some(None)
        }
    }

  implicit def listCodec[T](implicit C:DCodec[T]):DArrayCodec[List[T]] =
    new DArrayCodec[List[T]] {
      def apply(t: List[T]): DArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new DArray(t.toVector)
        else
          new DArray(t.map(C.apply(_).value).toVector)

      def unapply(a: Any): Option[List[T]] =
        a match {
          case s:Vector[Any]@unchecked =>
            Some(s.toIterator.flatMap(C.unapply).toList)
          case _ =>
            None
        }
    }

  implicit def vectorCodec[T](implicit C:DCodec[T]):DArrayCodec[Vector[T]] =
    new DArrayCodec[Vector[T]] {
      def apply(t: Vector[T]): DArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new DArray(t)
        else
          new DArray(t.map(C.apply(_).value))

      def unapply(a: Any): Option[Vector[T]] =
        a match {
          case s:Vector[Any]@unchecked =>
            Some(s.flatMap(C.unapply))
          case _ =>
            None
        }
    }

  implicit def fixedMapCodec[T](implicit D:DCodec[T]):DCodec[Map[String, T]] =
    if (D.isInstanceOf[DirectCodec[T]])
      new DirectCodec[Map[String, T]] {
        def unapply(a: Any): Option[Map[String, T]] =
          toMapT(a)
      }
    else
      new DCodec[Map[String, T]] {
        def unapply(a: Any): Option[Map[String, T]] =
          toMapT(a)

        def apply(t: Map[String, T]): Data =
          new DValue(t.mapValues(D(_).value))
      }

  private def toMapT[T](a:Any)(implicit D:DCodec[T]) =
    a match {
      case a:Map[String, Any]@unchecked =>
        Some(a.flatMap(p => D.unapply(p._2).map(p._1 -> _)))
      case _ =>
        None
    }
}

object DefaultCodecs extends DefaultCodecs

object PessimisticCodecs extends PessimisticCodecs

object OptimisticCodecs extends OptimisticCodecs
