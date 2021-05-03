package dsentric.codecs.std

import dsentric.{Available, DObject, DObjectInst, Failed, Found, Raw, RawObject}
import dsentric.codecs.DContractCodec
import dsentric.contracts.{Contract, ContractFor, ObjectLens}
import dsentric.failure.{DCodecTypeFailure, StructuralFailure}
import dsentric.schema.TypeDefinition

trait DContractCodecs {

  def apply(_contract:Contract):DContractCodec[DObject] =
    new DContractCodec[DObject]{

    def apply(t: DObject): RawObject = t.value
    def contract: ContractFor[DObject] = _contract

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

  def apply[D <: DObject](_contract:ContractFor[D])(to:DObject => D)(from:D => RawObject): DContractCodec[D] =
    new DContractCodec[D]{

      def apply(t: D): RawObject = from(t)
      def contract: ContractFor[D] = _contract

      //TODO creates unneccessary failures, optimise
      def unapply(a: Raw): Option[D] =
        a match {
          case m:RawObject@unchecked =>
            contract.$get(to(new DObjectInst(m))).toOption
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

      override def get(a: Raw): Available[D] =
        a match {
          case m:RawObject@unchecked =>
            ObjectLens.propertyVerifier(contract, m) match {
              case head :: tail =>
                Failed(head, tail)
              case Nil =>
                Found(to(new DObjectInst(m)))
            }
          case _ =>
            Failed(DCodecTypeFailure(this, a), Nil)
        }

      def typeDefinition: TypeDefinition = ???
    }


}
