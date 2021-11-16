package dsentric.codecs.std

import dsentric._
import dsentric.codecs._
import dsentric.filter.DFilter
import dsentric.schema._

trait DValueCodecs {

  implicit val dataCodec: DCodec[Data] =
    DataCodec

  implicit val dValueCodec: DValueCodec[DValue] =
    new DValueBridge[DValue] {

      def bridge(t: DValue): DValue = t

      def unapply(a: Raw): Option[DValue] =
        a match {
          case _: Vector[_] => None
          case _: Map[_, _] => None
          case v            => Some(ForceWrapper.dValue(v))
        }

      def typeDefinition: TypeDefinition =
        TypeDefinition.anyVal
    }

  implicit val stringCodec: DStringCodec[String] with DirectCodec[String] =
    new DStringCodec[String] with DirectCodec[String] {

      def fromString(s: String): Option[String] =
        Some(s)

      def typeDefinition: StringDefinition =
        StringDefinition.empty
    }

  implicit val charCodec: DStringCodec[Char] =
    new DStringCodec[Char] {

      def apply(t: Char): String =
        t.toString

      def fromString(s: String): Option[Char] =
        if (s.size == 1)
          s.headOption
        else None

      def typeDefinition: StringDefinition =
        StringDefinition(maxLength = Some(1))
    }

  implicit val booleanCodec: DValueCodec[Boolean] =
    new MatchCodec[Boolean] {
      protected def isMatch(a: Raw): Boolean =
        a.isInstanceOf[Boolean]
      def typeDefinition: TypeDefinition     =
        BooleanDefinition
    }

  implicit val longCodec: DValueCodec[Long] =
    new DValueCodec[Long] with DirectCodec[Long] {
      def unapply(a: Raw): Option[Long] =
        NumericPartialFunctions.long.lift(a)

      val typeDefinition: TypeDefinition =
        IntegerDefinition(minimum = Some(Long.MinValue), maximum = Some(Long.MaxValue))
    }

  implicit val doubleCodec: DValueCodec[Double] =
    new DValueCodec[Double] with DirectCodec[Double] {
      def unapply(a: Raw): Option[Double] =
        NumericPartialFunctions.double.lift(a)
      val typeDefinition: TypeDefinition  =
        NumberDefinition(minimum = Some(Double.MinValue), maximum = Some(Double.MaxValue))
    }

  implicit val intCodec: DValueCodec[Int]       =
    new DValueCodec[Int] {
      def apply(t: Int): Raw             =
        t.toLong
      def unapply(a: Raw): Option[Int]   =
        NumericPartialFunctions.int.lift(a)
      val typeDefinition: TypeDefinition =
        IntegerDefinition(minimum = Some(Int.MinValue), maximum = Some(Int.MaxValue))
    }
  implicit val shortCodec: DValueCodec[Short]   =
    new DValueCodec[Short] {
      def apply(t: Short): Raw           =
        t.toLong
      def unapply(a: Raw): Option[Short] =
        NumericPartialFunctions.short.lift(a)
      def typeDefinition: TypeDefinition =
        IntegerDefinition(minimum = Some(Short.MinValue), maximum = Some(Short.MaxValue))
    }
  implicit val byteCodec: DValueCodec[Byte]     =
    new DValueCodec[Byte] {
      def apply(t: Byte): Raw            =
        t.toLong
      def unapply(a: Raw): Option[Byte]  =
        NumericPartialFunctions.byte.lift(a)
      def typeDefinition: TypeDefinition =
        IntegerDefinition(minimum = Some(Byte.MinValue), maximum = Some(Byte.MaxValue))
    }
  implicit val floatCodec: DValueCodec[Float]   =
    new DValueCodec[Float] {
      def apply(t: Float): Raw           =
        t.toDouble
      def unapply(a: Raw): Option[Float] =
        NumericPartialFunctions.float.lift(a)
      def typeDefinition: TypeDefinition =
        NumberDefinition(minimum = Some(Float.MinValue.toDouble), maximum = Some(Float.MaxValue.toDouble))
    }
  implicit val numberCodec: DValueCodec[Number] =
    new DValueCodec[Number] {
      def apply(t: Number): Raw           =
        t.doubleValue()
      def unapply(a: Raw): Option[Number] =
        NumericPartialFunctions.number.lift(a)

      def typeDefinition: TypeDefinition =
        NumberDefinition.empty
    }
  implicit val dObjectCodec: DValueCodec[DObject] =
    new DValueCodec[DObject] {

      def apply(t: DObject): RawValue =
        t.value

      def unapply(a: Raw): Option[DObject] =
        a match {
          case r: RawObject @unchecked =>
            Some(new DObjectInst(r))
          case _                       =>
            None
        }
      def typeDefinition: TypeDefinition   =
        ObjectDefinition.empty

    }

  implicit val dArrayCodec: DValueCodec[DArray] =
    new DValueCodec[DArray] {
      override def apply(t: DArray): RawArray = t.value

      def unapply(a: Raw): Option[DArray] =
        a match {
          case raw: RawArray @unchecked =>
            Some(new DArray(raw))
          case _                        =>
            None
        }

      def typeDefinition: TypeDefinition =
        ArrayDefinition(items = Vector(TypeDefinition.anyDefinition))
    }

  implicit val dFilterCodec: DValueCodec[DFilter] =
    new DValueCodec[DFilter] {

      def apply(t: DFilter): RawValue =
        t.value

      def unapply(a: Raw): Option[DFilter] =
        a match {
          case r: RawObject @unchecked =>
            Some(new DFilter(r))
          case _                       =>
            None
        }
      def typeDefinition: TypeDefinition   =
        ObjectDefinition.empty
    }

  implicit val dProjectionCodec: DValueCodec[DProjection] =
    new DValueCodec[DProjection] {

      def apply(t: DProjection): RawValue =
        t.value

      def unapply(a: Raw): Option[DProjection] =
        a match {
          case r: RawObject @unchecked =>
            Some(new DProjection(r))
          case _                       =>
            None
        }
      def typeDefinition: TypeDefinition       =
        ObjectDefinition.empty
    }

  implicit val deltaCodec: DValueCodec[Delta] =
    new DValueCodec[Delta] {

      def apply(t: Delta): RawValue =
        t.value

      def unapply(a: Raw): Option[Delta] =
        a match {
          case r: RawObject @unchecked =>
            Some(new DeltaInst(r))
          case _                       =>
            None
        }
      def typeDefinition: TypeDefinition =
        ObjectDefinition.empty
    }

  implicit val dNullCodec: DCodec[DNull.type] =
    new MatchCodec[DNull.type] {

      protected def isMatch(a: Raw): Boolean =
        a.isInstanceOf[DNull.type]

      def typeDefinition: TypeDefinition =
        NullDefinition
    }

  implicit val pathCodec: DStringCodec[Path] =
    new DStringCodec[Path] {

      override def apply(t: Path): String =
        t.toString

      def fromString(s: String): Option[Path] =
        Some(Path.fromString(s))

      def typeDefinition: StringDefinition =
        StringDefinition.empty
    }

}

object DValueCodecs extends DValueCodecs
