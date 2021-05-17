package dsentric.operators

import dsentric.{DObject, Path, Raw}
import dsentric.contracts.ContractFor
import dsentric.failure.{ReservedFailure, ValidationFailures}

object Internal extends DeltaConstraint[Option[Nothing]] with Sanitizer[Option[Nothing]]{

  def verifyDelta[D <: DObject](
                                 contract: ContractFor[D],
                                 path: Path,
                                 delta:Raw,
                                 currentState: Raw): ValidationFailures =
    ValidationFailures(ReservedFailure(contract, path))

  def sanitize(value: Option[Raw]): Option[Raw] = None
}