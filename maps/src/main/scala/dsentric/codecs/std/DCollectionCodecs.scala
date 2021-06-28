package dsentric.codecs.std

import cats.data.NonEmptyList
import dsentric.codecs.{DCodec, DCollectionCodec}
import dsentric.schema.{ArrayDefinition, TypeDefinition}

import scala.reflect.ClassTag

trait DCollectionCodecs {

  implicit def arrayCodec[T](implicit D:DCodec[T], C:ClassTag[T]):DCollectionCodec[Array[T], T] =
    new DCollectionCodec[Array[T], T] {
      def valueCodec: DCodec[T] = D

      def build(t: Vector[T]): Option[Array[T]] =
        Some(t.toArray)

      def extract(s: Array[T]): Vector[T] =
        s.toVector

      def typeDefinition: TypeDefinition =
        ArrayDefinition(items = Vector(valueCodec.typeDefinition))
    }

  implicit def vectorCodec[T](implicit D:DCodec[T]):DCollectionCodec[Vector[T], T] =
    new DCollectionCodec[Vector[T], T] {
      def valueCodec: DCodec[T] = D

      def build(t: Vector[T]): Option[Vector[T]] =
        Some(t)

      def extract(s: Vector[T]): Vector[T] =
        s

      def typeDefinition: TypeDefinition =
        ArrayDefinition(items = Vector(valueCodec.typeDefinition))
    }

  implicit def listCodec[T](implicit D:DCodec[T]):DCollectionCodec[List[T], T] =
    new DCollectionCodec[List[T], T] {
      def valueCodec: DCodec[T] = D

      def build(t: Vector[T]): Option[List[T]] =
        Some(t.toList)

      def extract(s: List[T]): Vector[T] =
        s.toVector

      def typeDefinition: TypeDefinition =
        ArrayDefinition(items = Vector(valueCodec.typeDefinition))
    }

  implicit def nonEmptyListCodec[T](implicit D:DCodec[T]):DCollectionCodec[NonEmptyList[T], T] =
    new DCollectionCodec[NonEmptyList[T], T] {
      def valueCodec: DCodec[T] = D

      def build(t: Vector[T]): Option[NonEmptyList[T]] =
        t.toList match {
          case head :: tail =>
            Some(NonEmptyList(head, tail))
          case _ =>
            None
        }

      def extract(s: NonEmptyList[T]): Vector[T] =
        s.toList.toVector

      def typeDefinition: TypeDefinition =
        ArrayDefinition(Vector(D.typeDefinition), Some(1))
    }

}

object DCollectionCodecs extends DCollectionCodecs
