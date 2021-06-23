package dsentric.codecs.std

import dsentric.DObject
import dsentric.codecs.{DCodec, DContractCodec, DStringCodec}
import dsentric.contracts.Contract

import scala.reflect.ClassTag

object DCodecs
  extends DValueCodecs
  with DCollectionCodecs
  with DMapCodecs
  with DCoproductCodecs
  with DCodecSyntax {

  def contractCodec(contract:Contract):DCodec[DObject] =
    DContractCodec(contract)

  def mapContractCodec[K](contract:Contract)(implicit K:DStringCodec[K]): DCodec[Map[K, DObject]] =
    keyValueMapCodec[K, DObject](K, DContractCodec(contract))

  def listContractCodec(contract:Contract):DCodec[List[DObject]] =
    listCodec(DContractCodec(contract))

  def vectorContractCodec(contract:Contract):DCodec[Vector[DObject]] =
    vectorCodec(DContractCodec(contract))

  def arrayContractCodec(contract:Contract):DCodec[Array[DObject]] =
    arrayCodec[DObject](DContractCodec(contract), implicitly[ClassTag[DObject]])

  def rightContractCodec[L](contract:Contract)(implicit L:DCodec[L]):DCodec[Either[L, DObject]] =
    eitherCodec[L, DObject](L, DContractCodec(contract))

  def leftContractCodec[R](contract:Contract)(implicit R:DCodec[R]):DCodec[Either[DObject, R]] =
    eitherCodec[DObject, R](DContractCodec(contract), R)
}
