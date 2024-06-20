package dsentric.codecs.std

import dsentric.RawArray
import dsentric.codecs.{DCodec, DProductCodec}
import shapeless.{::, HNil}

trait DProductCodecs {

  implicit def tupleCodec[T1, T2](implicit
    D1: DCodec[T1],
    D2: DCodec[T2]
  ): DProductCodec[(T1, T2), T1 :: T2 :: HNil, DCodec[T1] :: DCodec[T2] :: HNil] =
    new DProductCodec[(T1, T2), T1 :: T2 :: HNil, DCodec[T1] :: DCodec[T2] :: HNil](D1 :: D2 :: HNil) {
      def apply(t: (T1, T2)): RawArray =
        Vector(D1(t._1), D2(t._2))

      def build(e: T1 :: T2 :: HNil): Option[(T1, T2)] =
        Some(e.tupled)
    }

  implicit def tuple3Codec[T1, T2, T3](implicit
    D1: DCodec[T1],
    D2: DCodec[T2],
    D3: DCodec[T3]
  ): DProductCodec[(T1, T2, T3), T1 :: T2 :: T3 :: HNil, DCodec[T1] :: DCodec[T2] :: DCodec[T3] :: HNil] =
    new DProductCodec[(T1, T2, T3), T1 :: T2 :: T3 :: HNil, DCodec[T1] :: DCodec[T2] :: DCodec[T3] :: HNil](
      D1 :: D2 :: D3 :: HNil
    ) {
      def apply(t: (T1, T2, T3)): RawArray =
        Vector(D1(t._1), D2(t._2), D3(t._3))

      def build(e: T1 :: T2 :: T3 :: HNil): Option[(T1, T2, T3)] =
        Some(e.tupled)

    }

}
