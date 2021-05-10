package dsentric.codecs.std

import dsentric.{Available, DNull, DNullable, DSome, Failed, Found, NotFound, Path, Raw}
import dsentric.codecs.{DCodec, DCoproductCodec}
import dsentric.failure.{DCodecDeltaNotSupportedFailure, DCodecTypeFailure, Failure, StructuralFailure}
import dsentric.schema.{MultipleTypeDefinition, SingleTypeDefinition, TypeDefinition}

/**
 * Validation failures are quite hard to resolve, which codec to return,
 * IE
 *   Nullable returns internal codec so null option isnt immediately obvious.
 *   But returning Nullable Contract, wont make sense when Internal codec is an Contract Codec
 */
trait DCoproductCodecs {

  implicit def dNullableCodec[T](implicit D:DCodec[T]): DCodec[DNullable[T]] =
    new DCoproductCodec[DNullable[T]] {

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def verify(a: Raw): List[StructuralFailure] =
        a match {
          case DNull => Nil
          case a => D.verify(a)
        }

      def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):List[Failure] =
        List(DCodecDeltaNotSupportedFailure(this, Path.empty))

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def get(a: Raw): Available[DNullable[T]] =
        a match {
          case DNull => Found(DNull)
          case t => D.get(t) match {
            case Found(f) => Found(DSome(f))
            case NotFound => NotFound
            case f:Failed => f
          }
        }

      def apply(t: DNullable[T]): Raw =
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

  implicit def eitherCodec[L, R](implicit DL: DCodec[L], DR: DCodec[R]): DCodec[Either[L, R]] =
    new DCoproductCodec[Either[L, R]] {

      def apply(t: Either[L, R]): Raw =
        t.fold(DL(_), DR(_))

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def verify(a: Raw): List[StructuralFailure] =
        DL.unapply(a)
          .fold(DR.unapply(a).fold(List(DCodecTypeFailure(this, a)))(_ => Nil))(_ => Nil)

      def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):List[Failure] =
        ???
      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def get(a: Raw): Available[Either[L, R]] =
        DL.unapply(a)
          .fold[Available[Either[L, R]]](DR.unapply(a).fold[Available[Either[L, R]]](Failed(DCodecTypeFailure(this, a)))(r => Found.apply(Right(r))))(l => Found.apply(Left(l)))

      def unapply(a: Any): Option[Either[L, R]] =
        a match {
          case DL(l) => Some(Left(l))
          case DR(r) => Some(Right(r))
          case _ => None
        }

      def typeDefinition: MultipleTypeDefinition = (DL.typeDefinition, DR.typeDefinition) match {
        case (l: SingleTypeDefinition, r: SingleTypeDefinition) => MultipleTypeDefinition(l, r)
        case (l: SingleTypeDefinition, MultipleTypeDefinition(rs)) => MultipleTypeDefinition(l +: rs)
        case (MultipleTypeDefinition(ls), r: SingleTypeDefinition) => MultipleTypeDefinition(ls :+ r)
        case (MultipleTypeDefinition(ls), MultipleTypeDefinition(rs)) => MultipleTypeDefinition(ls ++ rs)
      }
    }
}
