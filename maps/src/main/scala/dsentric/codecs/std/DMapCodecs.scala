package dsentric.codecs.std

import dsentric.codecs.{DCodec, DMapCodec, DStringCodec}

trait DMapCodecs {

  implicit def keyValueMapCodec[K, V](implicit K:DStringCodec[K], V:DCodec[V]):DMapCodec[Map[K, V], K, V] =
    new DMapCodec[Map[K, V], K, V]{
      def valueCodec: DCodec[V] = V

      def keyCodec: DStringCodec[K] = K

      def build(m: Map[K, V]): Map[K, V] =
        m

      def extract(m: Map[K, V]): Map[K, V] =
        m
    }
}

object DMapCodecs extends DMapCodecs
