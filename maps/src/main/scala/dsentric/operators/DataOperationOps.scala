package dsentric.operators

import dsentric.contracts.BaseContract
import dsentric.DObject
import dsentric.failure.ValidationFailures

class DataOperationOps[D <: DObject](contract:BaseContract[D]) {

  def validate(value:D):ValidationFailures =
    Validation.validateContract(contract, value.value, None)

  def validate(value:DObject, currentState:D):ValidationFailures =
    Validation.validateContract(contract, value.value, Some(currentState.value))

  //include validateAndReduce so we get a minimumDelta

}
