package dsentric.operators

import dsentric.{DObject, Delta}
import dsentric.contracts.ContractFor
import dsentric.failure.{StructuralFailure, ValidResult}

trait DeltaOps {

  def validate[D <: DObject](contract:ContractFor[D], state:D, delta:Delta):List[StructuralFailure]

  def validateAndReduce[D <: DObject](contract:ContractFor[D], state:D, delta:Delta):ValidResult[Delta]

  def validateReduceAndApply[D <: DObject](contract:ContractFor[D], state:D, delta:Delta):ValidResult[Delta]
}
