package dsentric.codecs.std

import cats.data.{NonEmptyList, NonEmptyVector}
import dsentric.DObject
import dsentric.codecs.{DCodec, DContractCodec, DStringCodec}
import dsentric.contracts.Contract

import scala.reflect.ClassTag

trait DCodecSyntax {

  implicit def contract2MapCodec[K](contract: Contract)(implicit D: DStringCodec[K]): DCodec[Map[K, DObject]] =
    DMapCodecs.keyValueMapCodec(D, DContractCodec(contract))

  implicit def valueCodec2MapCodec[K, V](valueCodec: DCodec[V])(implicit D: DStringCodec[K]): DCodec[Map[K, V]] =
    DMapCodecs.keyValueMapCodec(D, valueCodec)

  implicit def contract2Codec(contract: Contract): DCodec[DObject] =
    DContractCodec(contract)

  implicit def contract2VectorCodec(contract: Contract): DCodec[Vector[DObject]] =
    DCollectionCodecs.vectorCodec(DContractCodec(contract))

  implicit def contract2ArrayCodec(contract: Contract): DCodec[Array[DObject]] =
    DCollectionCodecs.arrayCodec(DContractCodec(contract), implicitly[ClassTag[DObject]])

  implicit def contract2ListCodec(contract: Contract): DCodec[List[DObject]] =
    DCollectionCodecs.listCodec(DContractCodec(contract))

  implicit def contract2NonEmptyListCodec(contract: Contract): DCodec[NonEmptyList[DObject]] =
    DCollectionCodecs.nonEmptyListCodec(DContractCodec(contract))

  implicit def contract2NonEmptyVectorCodec(contract: Contract): DCodec[NonEmptyVector[DObject]] =
    DCollectionCodecs.nonEmptyVectorCodec(DContractCodec(contract))

  implicit def contract2LeftCodec[R](contract: Contract)(implicit D: DCodec[R]): DCodec[Either[DObject, R]] =
    DCoproductCodecs.eitherCodec(DContractCodec(contract), D)

  implicit def contract2RightCodec[L](contract: Contract)(implicit D: DCodec[L]): DCodec[Either[L, DObject]] =
    DCoproductCodecs.eitherCodec(D, DContractCodec(contract))

}
