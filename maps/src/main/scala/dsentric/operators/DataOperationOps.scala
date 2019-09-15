package dsentric.operators

import dsentric.contracts.BaseContract
import dsentric.{DObject, PathFailures}

class DataOperationOps[D <: DObject](contract:BaseContract[D]) {

  def validate(value:D):PathFailures =
    Validation.validateContract(contract, value.value, None)

  def validate(value:DObject, currentState:D):PathFailures =
    Validation.validateContract(contract, value.value, Some(currentState.value))

}
