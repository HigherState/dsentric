package dsentric

import dsentric.contracts.PropertyOps

object Dsentric extends DataMatchers with AndMatcher with PropertyOps[DObject] with ToExtensionOps {

  implicit def strictness:Strictness = EmptyOnWrongType

  type Contract = contracts.Contract
  type SubContract = contracts.SubContract

  type ContractFor[D <: DObject] = contracts.ContractFor[D]
  type SubContractFor[D <: DObject] = contracts.SubContractFor[D]

  type ContractType = contracts.ContractType
  type ContractTypeFor[D <: DObject] = contracts.ContractTypeFor[D]

}
