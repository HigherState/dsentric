package dsentric.operators

import dsentric.contracts.BaseContract
import dsentric.DObject
import dsentric.failure.ValidationFailures

class DataOperationOps[D <: DObject](contract:BaseContract[D]) {

  def sanitize(value:D):D =
    Sanitization.sanitizeContract(contract, value.value)
      .fold[D](value)(value.internalWrap(_).asInstanceOf[D])
}
