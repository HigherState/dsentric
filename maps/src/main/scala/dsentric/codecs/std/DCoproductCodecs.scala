package dsentric.codecs.std

import dsentric.{DNull, DNullable, DSome, Raw}
import dsentric.codecs.{DCodec, DCoproductCodec}
import dsentric.schema.{MultipleTypeDefinition, SingleTypeDefinition, TypeDefinition}
import shapeless.{::, HNil}

trait DCoproductCodecs {

  implicit def dNullableCodec[T](implicit D:DCodec[T]): DCodec[DNullable[T]] =
    new DCoproductCodec[DNullable[T], DCodec[dsentric.DNull.type] :: DCodec[T] :: HNil](DValueCodecs.dNullCodec :: D :: HNil) {

      def apply(t: DNullable[T]): Raw =
        t match {
          case DNull => DNull
          case DSome(t) => D(t)
        }

      def unapply(a: Raw): Option[DNullable[T]] =
        a match {
          case DNull => Some(DNull)
          case D(v) => Some(DSome(v))
          case _ => None
        }

      def lift[A](a:A, codec:DCodec[A]):Option[DNullable[T]] = {
        if (codec == DValueCodecs.dNullCodec) Some(DNull)
        else if (codec == D) Some(DSome(a.asInstanceOf[T]))
        else None
      }

      def typeDefinition:TypeDefinition =
        TypeDefinition.nullable(D.typeDefinition)
    }

  implicit def eitherCodec[L, R](implicit DL: DCodec[L], DR: DCodec[R]): DCodec[Either[L, R]] =
    new DCoproductCodec[Either[L, R], DCodec[L] :: DCodec[R] :: HNil](DL :: DR :: HNil) {

      def apply(t: Either[L, R]): Raw =
        t.fold(DL(_), DR(_))

      def unapply(a: Any): Option[Either[L, R]] =
        a match {
          case DL(l) => Some(Left(l))
          case DR(r) => Some(Right(r))
          case _ => None
        }

      def lift[A](a:A, codec:DCodec[A]):Option[Either[L, R]] = {
        if (codec == DL) Some(Left(a.asInstanceOf[L]))
        else if (codec == DR) Some(Right(a.asInstanceOf[R]))
        else None
      }

      def typeDefinition: MultipleTypeDefinition = (DL.typeDefinition, DR.typeDefinition) match {
        case (l: SingleTypeDefinition, r: SingleTypeDefinition) => MultipleTypeDefinition(l, r)
        case (l: SingleTypeDefinition, MultipleTypeDefinition(rs)) => MultipleTypeDefinition(l +: rs)
        case (MultipleTypeDefinition(ls), r: SingleTypeDefinition) => MultipleTypeDefinition(ls :+ r)
        case (MultipleTypeDefinition(ls), MultipleTypeDefinition(rs)) => MultipleTypeDefinition(ls ++ rs)
      }
    }
}
