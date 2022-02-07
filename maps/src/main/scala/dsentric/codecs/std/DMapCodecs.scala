package dsentric.codecs.std

import cats.data.NonEmptySet
import dsentric.{Raw, RawValue}
import dsentric.codecs.{DCodec, DMapCodec, DStringCodec, DValueCodec}
import dsentric.schema.{NumberDefinition, ObjectDefinition, TypeDefinition}
import scala.collection.immutable.SortedSet

trait DMapCodecs {

  implicit def keyValueMapCodec[K, V](implicit K:DStringCodec[K], V:DCodec[V]):DMapCodec[Map[K, V], K, V] =
    new DMapCodec[Map[K, V], K, V]{
      def valueCodec: DCodec[V] = V

      def keyCodec: DStringCodec[K] = K

      def build(m: Map[K, V]): Option[Map[K, V]] =
        Some(m)

      def extract(m: Map[K, V]): Map[K, V] =
        m

      def typeDefinition: TypeDefinition =
        ObjectDefinition(additionalProperties = Right(V.typeDefinition), propertyNames = Some(K.typeDefinition))
    }

  implicit def setCodec[T](implicit D: DStringCodec[T], I: DCodec[1]):DMapCodec[Set[T], T, 1] =
    new DMapCodec[Set[T], T, 1] {
      def keyCodec: DStringCodec[T] = D

      def valueCodec: DCodec[1] = I

      def build(m: Map[T, 1]): Option[Set[T]] =
        Some(m.keySet)

      def extract(m: Set[T]): Map[T, 1] = {
        val mb = Map.newBuilder[T, 1]
        mb.addAll(m.map(_ -> valueOf[1]))
        mb.result()
      }


      def typeDefinition: TypeDefinition =
        ObjectDefinition(additionalProperties = Right(I.typeDefinition), propertyNames = Some(D.typeDefinition))
    }


  implicit def nonEmptySetCodec[T](implicit D: DStringCodec[T], O: Ordering[T], I: DCodec[1]):DMapCodec[NonEmptySet[T], T, 1] =
    new DMapCodec[NonEmptySet[T], T, 1] {
      def keyCodec: DStringCodec[T] = D

      def valueCodec: DCodec[1] = I

      def build(m: Map[T, 1]): Option[NonEmptySet[T]] =
        NonEmptySet.fromSet(SortedSet.from(m.keySet))

      def extract(m: NonEmptySet[T]): Map[T, 1] = {
        val mb = Map.newBuilder[T, 1]
        mb.addOne(m.head -> valueOf[1])
        mb.addAll(m.tail.iterator.map(_ -> valueOf[1]))
        mb.result()
      }

      def typeDefinition: TypeDefinition =
        ObjectDefinition(additionalProperties = Right(I.typeDefinition), propertyNames = Some(D.typeDefinition), minProperties = Some(1))
    }
}

object DMapCodecs extends DMapCodecs