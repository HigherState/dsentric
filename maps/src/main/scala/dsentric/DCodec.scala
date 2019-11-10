package dsentric

import dsentric.schema._

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ListBuffer

trait DCodec[T] {
  def apply(t:T):Data
  def unapply(a:Raw):Option[T]
  def typeDefinition:TypeDefinition
}

trait DValueCodec[T] extends DCodec[T] {
  override def apply(t:T):DValue
}

trait StringCodec[T] extends DCodec[T] {
  override def apply(t:T):DValue =
    new DValue(toString(t))


  def toString(t:T):String
}

trait DObjectCodec[T] extends DCodec[T] {
  override def apply(t:T):DObject
}

trait DArrayCodec[T, S] extends DCodec[S] {
  override def apply(t:S):DArray

  def valueCodec:DCodec[T]
}

trait DMapCodec[K, T] extends DCodec[Map[K, T]] {
  override def apply(t:Map[K,T]):DObject

  def keyCodec:StringCodec[K]

  def valueCodec:DCodec[T]
}


trait MatchCodec[T] extends DCodec[T] {

  def unapply(a:Raw):Option[T] =
    if (isMatch(a)) Some(a.asInstanceOf[T])
    else None

  protected def isMatch(a:Raw):Boolean
}

trait DirectCodec[T] extends DValueCodec[T] {
  override def apply(t: T): DValue =
    new DValue(t)
}

trait DefaultCodecs {

  implicit val anyCodec:DCodec[Any] =
    new DirectCodec[Any] {
      def unapply(a:Raw): Option[Any] = Some(a)

      def typeDefinition:TypeDefinition =
        TypeDefinition.anyDefinition
    }

  implicit val dataCodec:DCodec[Data] =
    new DCodec[Data] {
      def unapply(a: Any): Option[Data] =
        a match {
          case a:RawObject@unchecked =>
            Some(new DObjectInst(a))
          case v:RawArray@unchecked =>
            Some(new DArray(v))
          case DNull =>
            Some(DNull)
          case j =>
            Some(new DValue(j))
        }
      def apply(t: Data): Data =
        t

      def typeDefinition:TypeDefinition =
        TypeDefinition.anyDefinition
    }

  implicit val dQueryCodec:DCodec[DQuery] =
    new DCodec[DQuery] {
      def unapply(a:Raw): Option[DQuery] =
        a match {
          case a:RawObject@unchecked =>
            DQuery(a).toOption
          case j =>
            None
        }
      def apply(t: DQuery): Data =
        new DObjectInst(t.value)

      def typeDefinition:TypeDefinition =
        ObjectDefinition.empty
    }

  implicit val dObjectCodec:DObjectCodec[DObject] =
    new DObjectCodec[DObject] {
      def apply(t: DObject): DObject =
        t

      def unapply(a:Raw): Option[DObject] =
        a match {
          case m:RawObject@unchecked =>
            Some(new DObjectInst(m))
          case _ =>
            None
        }

      def typeDefinition:TypeDefinition =
        ObjectDefinition.empty
    }

  implicit val dArrayCodec:DArrayCodec[Any, DArray] =
    new DArrayCodec[Any, DArray] {
      def apply(t: DArray): DArray =
        t

      def unapply(a:Raw): Option[DArray] =
        a match {
          case v:RawArray@unchecked =>
            Some(new DArray(v))
          case _ =>
            None
        }
      def typeDefinition:TypeDefinition =
        ArrayDefinition.empty

      def valueCodec: DCodec[Any] =
        anyCodec
    }

  implicit def setCodec[T](implicit D:StringCodec[T]):DCodec[Set[T]] =
    new DCodec[Set[T]] {
      def apply(t: Set[T]): Data =
        ForceWrapper.dObject(t.map(D.toString(_) -> 1).toMap)

      def unapply(a: Raw): Option[Set[T]] =
        a match {
          case obj:RawObject@unchecked =>
            obj.foldLeft(Option(Set.empty[T])){
              case (Some(a), (key, 1)) =>
                D.unapply(key).map(a + _)
              case _ =>
                None
            }
          case _ =>
            None
        }

      def typeDefinition: TypeDefinition = ???
    }

  implicit val dNullCodec:DCodec[DNull.type] =
    new MatchCodec[DNull.type] with DirectCodec[DNull.type] {
      protected def isMatch(a: Raw): Boolean =
        a.isInstanceOf[DNull.type]

      def typeDefinition:TypeDefinition =
        NullDefinition
    }

  implicit def dNullableCodec[T](implicit D:DCodec[T]): DCodec[DNullable[T]] =
    new DCodec[DNullable[T]] {
      def apply(t: DNullable[T]): Data =
        t match {
          case DNull => DNull
          case DSome(t) => D(t)
        }

      def unapply(a: Any): Option[DNullable[T]] =
        a match {
          case DNull => Some(DNull)
          case D(v) => Some(DSome(v))
          case _ => None
        }
      def typeDefinition:TypeDefinition =
        TypeDefinition.nullable(D.typeDefinition)
    }

  implicit def tupleCodec[T1,T2](implicit D1:DCodec[T1], D2:DCodec[T2]):DCodec[(T1, T2)] =
    new DCodec[(T1, T2)] {
      override def apply(t: (T1, T2)): DArray =
        new DArray(Vector(D1(t._1).value, D2(t._2).value))

      def unapply(a:Raw): Option[(T1, T2)] =
        a match {
          case Vector(D1(t1), D2(t2)) =>
            Some(t1 -> t2)
          case _ =>
            None
        }

      def typeDefinition:TypeDefinition =
        ArrayDefinition(Vector(D1.typeDefinition, D2.typeDefinition))
    }

  implicit val pathCodec:StringCodec[Path] =
    new StringCodec[Path] {
      def unapply(a:Raw): Option[Path] =
        a match {
          case s:String =>
            Some(Path.fromString(s))
          case _ =>
            None
        }

      def toString(t: Path): String = t.toString

      def typeDefinition:TypeDefinition =
        TypeDefinition.anyDefinition
    }

  def dObjectLikeCodec[T <: DObjectLike[T] with DObject](wrap:Map[String, Any] => T):DCodec[T] =
    new DCodec[T] {
      def apply(t: T):T = t
      def unapply(a:Raw): Option[T] =
        a match {
          case m:RawObject@unchecked =>
            Some(wrap(m))
          case _ =>
            None
        }

      def typeDefinition:TypeDefinition =
        ObjectDefinition.empty
    }
}

trait PessimisticCodecs extends DefaultCodecs {

  implicit val stringCodec:StringCodec[String] =
    new StringCodec[String]  {

      def toString(t: String): String = t

      def unapply(a: Raw): Option[String] =
        a match {
          case s:String => Some(s)
          case _ => None
        }

      def typeDefinition:TypeDefinition =
        StringDefinition.empty
    }
  implicit val booleanCodec:DValueCodec[Boolean] =
    new DirectCodec[Boolean] with MatchCodec[Boolean] {
      protected def isMatch(a: Raw): Boolean =
        a.isInstanceOf[Boolean]
      def typeDefinition:TypeDefinition =
        BooleanDefinition
    }
  implicit val longCodec:DValueCodec[Long] =
    new DirectCodec[Long] {
      def unapply(a:Raw): Option[Long] =
        NumericPartialFunctions.long.lift(a)

      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Long.MinValue), maximum = Some(Long.MaxValue))
    }
  implicit val doubleCodec:DValueCodec[Double] =
    new DirectCodec[Double] {
      def unapply(a:Raw): Option[Double] =
        NumericPartialFunctions.double.lift(a)
      val typeDefinition:TypeDefinition =
        NumberDefinition(minimum = Some(Double.MinValue), maximum = Some(Double.MaxValue))
    }

  implicit val intCodec:DValueCodec[Int] =
    new DValueCodec[Int] {
      def apply(t: Int): DValue =
        new DValue(t.toLong)
      def unapply(a:Raw): Option[Int] =
        NumericPartialFunctions.int.lift(a)
      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Int.MinValue), maximum = Some(Int.MaxValue))
    }
  implicit val shortCodec:DValueCodec[Short] =
    new DValueCodec[Short] {
      def apply(t: Short): DValue =
        new DValue(t.toLong)
      def unapply(a:Raw): Option[Short] =
        NumericPartialFunctions.short.lift(a)
      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Short.MinValue), maximum = Some(Short.MaxValue))
    }
  implicit val byteCodec:DValueCodec[Byte] =
    new DValueCodec[Byte] {
      def apply(t: Byte): DValue =
        new DValue(t.toLong)
      def unapply(a:Raw): Option[Byte] =
        NumericPartialFunctions.byte.lift(a)
      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Byte.MinValue), maximum = Some(Byte.MaxValue))
    }
  implicit val floatCodec:DValueCodec[Float] =
    new DValueCodec[Float] {
      def apply(t: Float): DValue =
        new DValue(t.toDouble)
      def unapply(a:Raw): Option[Float] =
        NumericPartialFunctions.float.lift(a)
      val typeDefinition:TypeDefinition =
        NumberDefinition(minimum = Some(Float.MinValue), maximum = Some(Float.MaxValue))
    }
  implicit val numberCodec:DValueCodec[Number] =
    new DValueCodec[Number] {
      def apply(t: Number): DValue =
        new DValue(t)
      def unapply(a:Raw): Option[Number] =
        NumericPartialFunctions.number.lift(a)

      val typeDefinition:TypeDefinition =
        NumberDefinition.empty
    }

  implicit def optionCodec[T](implicit D:DCodec[T]): DCodec[Option[T]] =
    new DCodec[Option[T]] {
      def apply(t: Option[T]): Data =
        t.fold[Data](DNull)(t => D(t))

      def unapply(a:Raw): Option[Option[T]] =
        a match {
          case DNull => Some(None)
          case D(v) => Some(Some(v))
          case _ => None
        }

      def typeDefinition:TypeDefinition =
        TypeDefinition.nullable(D.typeDefinition)
    }

  implicit def listCodec[T](implicit C:DCodec[T]):DArrayCodec[T, List[T]] =
    new DArrayCodec[T, List[T]] {
      def apply(t: List[T]): DArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new DArray(t.toVector)
        else
          new DArray(t.map(C.apply(_).value).toVector)

      def unapply(a:Raw): Option[List[T]] =
        a match {
          case s:RawArray@unchecked =>
            s.toIterator.map(C.unapply).foldLeft[Option[ListBuffer[T]]](Some(new ListBuffer[T])){
              case (Some(lb), Some(t)) => Some(lb += t)
              case _ => None
            }.map(_.result())
          case _ =>
            None
        }

      def valueCodec: DCodec[T] = C

      def typeDefinition:TypeDefinition =
        ArrayDefinition(Vector(C.typeDefinition))
    }

  implicit def vectorCodec[T](implicit C:DCodec[T]):DArrayCodec[T, Vector[T]] =
    new DArrayCodec[T, Vector[T]] {
      def apply(t: Vector[T]): DArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new DArray(t)
        else
          new DArray(t.map(C.apply(_).value))

      def unapply(a:Raw): Option[Vector[T]] =
        a match {
          case s:RawArray@unchecked =>
            s.toIterator.map(C.unapply).foldLeft[Option[VectorBuilder[T]]](Some(new VectorBuilder[T])){
              case (Some(vb), Some(t)) => Some(vb += t)
              case _ => None
            }.map(_.result())
          case _ =>
            None
        }


      def valueCodec: DCodec[T] = C

      def typeDefinition:TypeDefinition =
        ArrayDefinition(Vector(C.typeDefinition))
    }

  implicit def fixedMapCodec[T](implicit D:DCodec[T]):DMapCodec[String, T] =
    if (D.isInstanceOf[DirectCodec[T]])
      new DMapCodec[String, T] {
        def apply(t: Map[String, T]): DObject =
          new DObjectInst(t)

        def unapply(a:Raw): Option[Map[String, T]] =
          toMapT(a)
        def typeDefinition:TypeDefinition =
          ObjectDefinition()

        def keyCodec: StringCodec[String] =
          stringCodec

        def valueCodec: DCodec[T] = D
      }
    else
      new DMapCodec[String, T] {
        def unapply(a:Raw): Option[Map[String, T]] =
          toMapT(a)(D)

        def apply(t: Map[String, T]): DObject =
          new DObjectInst(t.mapValues(D(_).value))

        def typeDefinition:TypeDefinition =
          ObjectDefinition(additionalProperties = Right(D.typeDefinition))

        def keyCodec: StringCodec[String] =
          stringCodec

        def valueCodec: DCodec[T] = D
      }

  implicit def fullMapCodec[K, T](implicit D:DCodec[T], K:StringCodec[K]):DMapCodec[K, T] =
    new DMapCodec[K, T] {
      def unapply(a:Raw): Option[Map[K, T]] =
        a match {
          case a:RawObject@unchecked =>
            a.toIterator.foldLeft(Option(Map.newBuilder[K, T])){
              case (Some(m), (K(k), D(v))) =>
                Some(m += (k -> v))
              case (Some(m), (_, DNull)) => //dont want deltas to break conversion
                Some(m)
              case _ =>
                None
            }.map(_.result())
          case _ =>
            None
        }

      def apply(t: Map[K, T]): DObject =
        new DObjectInst(t.map(p => K(p._1).value.toString -> D(p._2).value))

      def typeDefinition:TypeDefinition =
        K.typeDefinition match {
          case s:StringDefinition =>
            ObjectDefinition(propertyNames = Some(s), additionalProperties = Right(D.typeDefinition))
          case _ =>
            ObjectDefinition(additionalProperties = Right(D.typeDefinition))
        }

      def keyCodec: StringCodec[K] = K

      def valueCodec: DCodec[T] = D
    }

  private def toMapT[T](a:Any)(implicit D:DCodec[T]): Option[Map[String, T]] =
    a match {
      case a:RawObject@unchecked =>
        a.toIterator.foldLeft(Option(Map.newBuilder[String, T])){
          case (Some(m), (k, D(v))) =>
            Some(m += (k -> v))
          case (Some(m), (_, DNull)) => //dont want deltas to break conversion
            Some(m)
          case _ =>
            None
        }.map(_.result())
      case _ =>
        None
    }

}

trait OptimisticCodecs extends DefaultCodecs {

  import dsentric.util.ToStringContextOps._

  implicit val stringCodec:StringCodec[String] =
    new StringCodec[String] {

      def toString(t: String): String = t

      def unapply(a: Raw): Option[String] =
        Some(a.toString)

      def typeDefinition:TypeDefinition =
        StringDefinition.empty
    }

  implicit val booleanCodec:DValueCodec[Boolean] =
    new DirectCodec[Boolean] {
      def unapply(a:Raw): Option[Boolean] =
        a match {
          case true | i"true" | 1 => Some(true)
          case false | i"false" | 0 => Some(false)
          case _ => None
        }

      def typeDefinition:TypeDefinition =
        BooleanDefinition
    }
  implicit val longCodec:DValueCodec[Long] =
    new DirectCodec[Long] {
      def unapply(a:Raw): Option[Long] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.long.lift(a))(NumericPartialFunctions.long.lift)

      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Long.MinValue), maximum = Some(Long.MaxValue))

    }
  implicit val doubleCodec:DValueCodec[Double] =
    new DirectCodec[Double] {
      def unapply(a:Raw): Option[Double] =
        NumericPartialFunctions.stringDouble.lift(a)
          .orElse(NumericPartialFunctions.double.lift(a))

      val typeDefinition:TypeDefinition =
        NumberDefinition(minimum = Some(Double.MinValue), maximum = Some(Double.MaxValue))
    }

  implicit val intCodec:DValueCodec[Int] =
    new DValueCodec[Int] {
      def apply(t: Int): DValue =
        new DValue(t.toLong)
      def unapply(a:Raw): Option[Int] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.int.lift(a))(NumericPartialFunctions.int.lift)

      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Int.MinValue), maximum = Some(Int.MaxValue))
    }
  implicit val shortCodec:DValueCodec[Short] =
    new DValueCodec[Short] {
      def apply(t: Short): DValue =
        new DValue(t.toLong)
      def unapply(a:Raw): Option[Short] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.short.lift(a))(NumericPartialFunctions.short.lift)

      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Short.MinValue), maximum = Some(Short.MaxValue))
    }
  implicit val byteCodec:DValueCodec[Byte] =
    new DValueCodec[Byte] {
      def apply(t: Byte): DValue =
        new DValue(t.toLong)
      def unapply(a:Raw): Option[Byte] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.byte.lift(a))(NumericPartialFunctions.byte.lift)

      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Byte.MinValue), maximum = Some(Byte.MaxValue))
    }
  implicit val floatCodec:DValueCodec[Float] =
    new DValueCodec[Float] {
      def apply(t: Float): DValue =
        new DValue(t.toDouble)
      def unapply(a:Raw): Option[Float] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.float.lift(a))(NumericPartialFunctions.float.lift)

      val typeDefinition:TypeDefinition =
        NumberDefinition(minimum = Some(Float.MinValue), maximum = Some(Float.MaxValue))
    }
  implicit val numberCodec:DValueCodec[Number] =
    new DValueCodec[Number] {
      def apply(t: Number): DValue =
        new DValue(t)
      def unapply(a:Raw): Option[Number] =
        NumericPartialFunctions.stringDouble.lift(a)
          .fold(NumericPartialFunctions.number.lift(a))(NumericPartialFunctions.number.lift)

      val typeDefinition:TypeDefinition =
        NumberDefinition.empty
    }

  implicit def optionCodec[T](implicit D:DCodec[T]): DCodec[Option[T]] =
    new DCodec[Option[T]] {
      def apply(t: Option[T]): Data =
        t.fold[Data](DNull)(t => D(t))

      def unapply(a:Raw): Option[Option[T]] =
        a match {
          case DNull => Some(None)
          case D(v) => Some(Some(v))
          case _ => Some(None)
        }
      def typeDefinition:TypeDefinition =
        TypeDefinition.nullable(D.typeDefinition)
    }

  implicit def listCodec[T](implicit C:DCodec[T]):DArrayCodec[T, List[T]] =
    new DArrayCodec[T, List[T]] {
      def apply(t: List[T]): DArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new DArray(t.toVector)
        else
          new DArray(t.map(C.apply(_).value).toVector)

      def unapply(a:Raw): Option[List[T]] =
        a match {
          case s:RawArray@unchecked =>
            Some(s.toIterator.flatMap(C.unapply).toList)
          case _ =>
            None
        }


      def valueCodec: DCodec[T] = C

      def typeDefinition:TypeDefinition =
        ArrayDefinition(Vector(C.typeDefinition))
    }

  implicit def vectorCodec[T](implicit C:DCodec[T]):DArrayCodec[T, Vector[T]] =
    new DArrayCodec[T, Vector[T]] {
      def apply(t: Vector[T]): DArray =
        if (C.isInstanceOf[DirectCodec[T]])
          new DArray(t)
        else
          new DArray(t.map(C.apply(_).value))

      def unapply(a:Raw): Option[Vector[T]] =
        a match {
          case s:RawArray@unchecked =>
            Some(s.flatMap(C.unapply))
          case _ =>
            None
        }


      def valueCodec: DCodec[T] = C

      def typeDefinition:TypeDefinition =
        ArrayDefinition(Vector(C.typeDefinition))
    }

  implicit def fixedMapCodec[T](implicit D:DCodec[T]):DMapCodec[String, T] =
    if (D.isInstanceOf[DirectCodec[T]])
      new DMapCodec[String, T] {
        def apply(t: Map[String, T]): DObject =
          new DObjectInst(t)

        def unapply(a:Raw): Option[Map[String, T]] =
          toMapT(a)

        def typeDefinition:TypeDefinition =
          ObjectDefinition(additionalProperties = Right(D.typeDefinition))

        def keyCodec: StringCodec[String] = stringCodec

        def valueCodec: DCodec[T] = D
      }
    else
      new DMapCodec[String, T] {
        def unapply(a:Raw): Option[Map[String, T]] =
          toMapT(a)

        def apply(t: Map[String, T]): DObject =
          new DObjectInst(t.mapValues(D(_).value))

        def typeDefinition:TypeDefinition =
          ObjectDefinition(additionalProperties = Right(D.typeDefinition))

        def keyCodec: StringCodec[String] = stringCodec

        def valueCodec: DCodec[T] = D
      }

  private def toMapT[T](a:Raw)(implicit D:DCodec[T]): Option[Map[String, T]] =
    a match {
      case a:RawObject@unchecked =>
        Some(a.flatMap(p => D.unapply(p._2).map(p._1 -> _)))
      case _ =>
        None
    }
}

object DefaultCodecs extends DefaultCodecs

object PessimisticCodecs extends PessimisticCodecs

object OptimisticCodecs extends OptimisticCodecs


object DataMatch {
  def apply[T](implicit codec:DCodec[T]):DataMatcher[T] =
    new DataMatcher[T]
}
class DataMatcher[T](implicit codec:DCodec[T]) {
  def unapply(d:Data):Option[T] =
    codec.unapply(d.value)
}
