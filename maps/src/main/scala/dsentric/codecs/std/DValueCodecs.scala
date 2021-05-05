package dsentric.codecs.std

import dsentric.{DNull, DValue, Data, NumericPartialFunctions, Path, Raw}
import dsentric.codecs.{DCodec, DStringCodec, DValueCodec, DataCodec, DirectCodec, MatchCodec}
import dsentric.failure.{DCodecDeltaNotSupportedFailure, StructuralFailure}
import dsentric.schema.{BooleanDefinition, IntegerDefinition, NullDefinition, NumberDefinition, StringDefinition, TypeDefinition}

trait DValueCodecs {
  implicit val stringCodec:DStringCodec[String] with DirectCodec[String] =
    new DStringCodec[String] with DirectCodec[String] {

      def fromString(s: String): Option[String] =
        Some(s)

      def typeDefinition:StringDefinition =
        StringDefinition.empty
    }

  implicit val booleanCodec:DValueCodec[Boolean] =
    new MatchCodec[Boolean] {
      protected def isMatch(a: Raw): Boolean =
        a.isInstanceOf[Boolean]
      def typeDefinition:TypeDefinition =
        BooleanDefinition
    }

  implicit val longCodec:DValueCodec[Long] =
    new DValueCodec[Long] with DirectCodec[Long] {
      def unapply(a:Raw): Option[Long] =
        NumericPartialFunctions.long.lift(a)

      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Long.MinValue), maximum = Some(Long.MaxValue))
    }

  implicit val doubleCodec:DValueCodec[Double] =
    new DValueCodec[Double] with DirectCodec[Double] {
      def unapply(a:Raw): Option[Double] =
        NumericPartialFunctions.double.lift(a)
      val typeDefinition:TypeDefinition =
        NumberDefinition(minimum = Some(Double.MinValue), maximum = Some(Double.MaxValue))
    }

  implicit val intCodec:DValueCodec[Int] =
    new DValueCodec[Int] {
      def apply(t: Int): Raw =
        t.toLong
      def unapply(a:Raw): Option[Int] =
        NumericPartialFunctions.int.lift(a)
      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Int.MinValue), maximum = Some(Int.MaxValue))
    }
  implicit val shortCodec:DValueCodec[Short] =
    new DValueCodec[Short] {
      def apply(t: Short): Raw =
        t.toLong
      def unapply(a:Raw): Option[Short] =
        NumericPartialFunctions.short.lift(a)
      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Short.MinValue), maximum = Some(Short.MaxValue))
    }
  implicit val byteCodec:DValueCodec[Byte] =
    new DValueCodec[Byte] {
      def apply(t: Byte): Raw =
        t.toLong
      def unapply(a:Raw): Option[Byte] =
        NumericPartialFunctions.byte.lift(a)
      val typeDefinition:TypeDefinition =
        IntegerDefinition(minimum = Some(Byte.MinValue), maximum = Some(Byte.MaxValue))
    }
  implicit val floatCodec:DValueCodec[Float] =
    new DValueCodec[Float] {
      def apply(t: Float): Raw =
        t.toDouble
      def unapply(a:Raw): Option[Float] =
        NumericPartialFunctions.float.lift(a)
      val typeDefinition:TypeDefinition =
        NumberDefinition(minimum = Some(Float.MinValue), maximum = Some(Float.MaxValue))
    }
  implicit val numberCodec:DValueCodec[Number] =
    new DValueCodec[Number] {
      def apply(t: Number): Raw =
        t.doubleValue()
      def unapply(a:Raw): Option[Number] =
        NumericPartialFunctions.number.lift(a)

      val typeDefinition:TypeDefinition =
        NumberDefinition.empty
    }

  implicit val dNullCodec:DCodec[DNull.type] =
    new MatchCodec[DNull.type] {

      protected def isMatch(a: Raw): Boolean =
        a.isInstanceOf[DNull.type]

      override def verify(deltaValue: Raw, currentValue: Raw): List[StructuralFailure] =
        List(DCodecDeltaNotSupportedFailure(this, Path.empty))

      def typeDefinition:TypeDefinition =
        NullDefinition
    }

  implicit val pathCodec:DStringCodec[Path] =
    new DStringCodec[Path] {

      override def apply(t: Path): String =
        t.toString

      def fromString(s: String): Option[Path] =
        Some(Path.fromString(s))

      def typeDefinition:StringDefinition =
        StringDefinition.empty
    }

  implicit val dataCodec:DCodec[Data] =
    DataCodec
}

object DValueCodecs extends DValueCodecs
