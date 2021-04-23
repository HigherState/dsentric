package dsentric

import dsentric.contracts.{Contract, ContractFor, ObjectLens}
import dsentric.failure.{DCodecTypeFailure, StructuralFailure}
import dsentric.schema.TypeDefinition

case class ContractDCodec(contract:Contract) extends DObjectCodec[DObject] {

  def apply(t: DObject): Data = t

  //TODO creates unneccessary failures, optimise
  def unapply(a: Raw): Option[DObject] =
    a match {
      case m:RawObject@unchecked =>
        contract.$get(new DObjectInst(m)).toOption
      case _ =>
        None
    }

  override def verify(a: Raw): List[StructuralFailure] =
    a match {
      case m:RawObject@unchecked =>
        ObjectLens.propertyVerifier(contract, m)
      case _ =>
        List(DCodecTypeFailure(this, a))
    }

  override def get(a: Raw): Available[DObject] =
    a match {
      case m:RawObject@unchecked =>
        ObjectLens.propertyVerifier(contract, m) match {
          case head :: tail =>
            Failed(head, tail)
          case Nil =>
            Found(new DObjectInst(m))
        }
      case _ =>
        Failed(DCodecTypeFailure(this, a), Nil)
    }

  def typeDefinition: TypeDefinition = ???
}

abstract class ContractForDCodec[D <: DObject](contract:ContractFor[D]) extends DObjectCodec[D] {

  override def verify(a: Raw): List[StructuralFailure] =
    unapply(a) match {
      case None =>
        List(DCodecTypeFailure(this, a))
      case Some(d) =>
        ObjectLens.propertyVerifier(contract, d.value)
    }

  override def get(a: Raw): Available[D] =
    unapply(a) match {
      case None =>
        Failed(DCodecTypeFailure(this, a))
      case Some(d) =>
        ObjectLens.propertyVerifier(contract, d.value) match {
          case head :: tail =>
            Failed(head, tail)
          case Nil =>
            Found(d)
        }
    }
}
