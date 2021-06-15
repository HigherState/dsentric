package dsentric.operators

import dsentric.{Available, DObject, DeltaReduce, Path, Raw}
import dsentric.contracts.ContractFor
import dsentric.failure.{ReservedFailure, ValidationFailures}

/**
 * Internal properties cannot be set or altered, even setting to the same value will cause failure as we dont
 * want values to be fished for
 */
object Internal extends Constraint[Option[Nothing]] with Sanitizer[Option[Nothing]]{

  def verify[D <: DObject](contract: ContractFor[D], path: Path, value: Available[Raw]): ValidationFailures =
    ValidationFailures(ReservedFailure(contract, path))

  /**
   * Verify the reduced delta value against the current State
   *
   * @param contract
   * @param path
   * @param reducedDelta
   * @param currentState
   * @tparam S
   * @tparam D
   * @return
   */
  def verify[D <: DObject](contract:  ContractFor[D], path:  Path, current: Raw, delta:  DeltaReduce[Raw]): ValidationFailures =
    ValidationFailures(ReservedFailure(contract, path))

  def sanitize(value: Option[Raw]): Option[Raw] = None
}