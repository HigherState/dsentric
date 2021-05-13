package dsentric.codecs.std

import dsentric.codecs.{DCodec, DCollectionCodec}
import scala.reflect.ClassTag

trait DCollectionCodecs {

  implicit def arrayCodec[T](implicit D:DCodec[T], C:ClassTag[T]):DCollectionCodec[Array[T], T] =
    new DCollectionCodec[Array[T], T] {
      def valueCodec: DCodec[T] = D

      def build(t: Vector[T]): Array[T] =
        t.toArray

      def extract(s: Array[T]): Vector[T] =
        s.toVector
    }

  implicit def vectorCodec[T](implicit D:DCodec[T]):DCollectionCodec[Vector[T], T] =
    new DCollectionCodec[Vector[T], T] {
      def valueCodec: DCodec[T] = D

      def build(t: Vector[T]): Vector[T] =
        t

      def extract(s: Vector[T]): Vector[T] =
        s
    }

  implicit def listCodec[T](implicit D:DCodec[T]):DCollectionCodec[List[T], T] =
    new DCollectionCodec[List[T], T] {
      def valueCodec: DCodec[T] = D

      def build(t: Vector[T]): List[T] =
        t.toList

      def extract(s: List[T]): Vector[T] =
        s.toVector
    }

}
