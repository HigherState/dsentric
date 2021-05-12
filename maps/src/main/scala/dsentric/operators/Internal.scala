package dsentric.operators

import dsentric.{DObject, Path, Raw}
import dsentric.contracts.ContractFor
import dsentric.failure.{ReservedFailure, ValidationFailures}

object Internal extends DeltaConstraint[Option[Nothing]] with Sanitizer[Option[Nothing]]{

  def verifyDelta[S >: Option[Nothing], D <: DObject](
                                                       contract: ContractFor[D],
                                                       path: Path,
                                                       currentState: Option[S],
                                                       finalState: Option[S]): ValidationFailures =
    ValidationFailures(ReservedFailure(contract, path))

  def sanitize(value: Option[Raw]): Option[Raw] = None
}