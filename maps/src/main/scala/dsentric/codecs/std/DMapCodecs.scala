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

  private def oneCodec:DCodec[Long] = new DValueCodec[Long] {
    def apply(t: Long): RawValue = 1L

    def unapply(a: Raw): Option[Long] =
      a match {
        case 1L => Some(1L)
        case _ => None
      }

    def typeDefinition: TypeDefinition = NumberDefinition(List(1L))
  }

  implicit def setCodec[T](implicit D: DStringCodec[T]):DMapCodec[Set[T], T, Long] =
    new DMapCodec[Set[T], T, Long] {
      def keyCodec: DStringCodec[T] = D

      def valueCodec: DCodec[Long] = oneCodec

      def build(m: Map[T, Long]): Option[Set[T]] =
        Some(m.keySet)

      def extract(m: Set[T]): Map[T, Long] =
        m.iterator.map(_ -> 1L).toMap

      def typeDefinition: TypeDefinition =
        ObjectDefinition(additionalProperties = Right(oneCodec.typeDefinition), propertyNames = Some(D.typeDefinition))
    }


  implicit def nonEmptySetCodec[T](implicit D: DStringCodec[T], O: Ordering[T]):DMapCodec[NonEmptySet[T], T, Long] =
    new DMapCodec[NonEmptySet[T], T, Long] {
      def keyCodec: DStringCodec[T] = D

      def valueCodec: DCodec[Long] = oneCodec

      def build(m: Map[T, Long]): Option[NonEmptySet[T]] =
        NonEmptySet.fromSet(SortedSet.from(m.keySet))

      def extract(m: NonEmptySet[T]): Map[T, Long] = {
        val mb = Map.newBuilder[T, Long]
        mb.addOne(m.head -> 1L)
        mb.addAll(m.tail.iterator.map(_ -> 1L))
        mb.result()
      }

      def typeDefinition: TypeDefinition =
        ObjectDefinition(additionalProperties = Right(oneCodec.typeDefinition), propertyNames = Some(D.typeDefinition), minProperties = Some(1))
    }
}

object DMapCodecs extends DMapCodecs