package dsentric

import dsentric.contracts.PropertySyntax

object Dsentric extends DataMatchers with AndMatcher with Syntax with PropertySyntax {

  type Contract = contracts.Contract
  type SubContract = contracts.SubContract

  type ContractFor[D <: DObject] = contracts.ContractFor[D]
  type SubContractFor[D <: DObject] = contracts.SubContractFor[D]

  type ContractType = contracts.ContractType
  type ContractTypeFor[D <: DObject] = contracts.ContractTypeFor[D]

}
