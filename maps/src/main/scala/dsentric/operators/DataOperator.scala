package dsentric.operators

import dsentric.contracts.ContractFor
import dsentric.failure.ValidationFailures
import dsentric.{Available, DObject, DeltaReduce, Path, Raw}
import dsentric.schema.TypeDefinition

sealed trait Condition

trait Expected extends Condition

trait Optional extends Condition

sealed trait DataOperator[+T] { this: Condition =>
  def definition[D <: TypeDefinition]: PartialFunction[D, D] = { case t =>
    t
  }
}

trait ContextValidator[+T] extends DataOperator[T] { this: Condition =>
  def apply[S >: T](path: Path, value: Option[S], currentState: => Option[S]): ValidationFailures
}

trait Transform[+T] extends DataOperator[T] { this: Condition =>
  def transform[S >: T](value: Option[S]): Option[S]
}

trait ContextTransform[C, +T] extends DataOperator[T] { this: Condition =>
  def transform[S >: T](context: C, value: Option[S]): Option[S]
}

trait Constraint[+T] extends DataOperator[T] { this: Condition =>

  def verify[D <: DObject](contract: ContractFor[D], path: Path, value: Available[Raw]): ValidationFailures

  /**
   * Verify the reduced delta value against the current State
   * @param contract
   * @param path
   * @param reducedDelta
   * @param currentState
   * @tparam S
   * @tparam D
   * @return
   */
  def verify[D <: DObject](
    contract: ContractFor[D],
    path: Path,
    current: Raw,
    delta: DeltaReduce[Raw]
  ): ValidationFailures
}

trait Sanitizer[+T] extends DataOperator[T] with Optional with Expected {

  def sanitize(value: Option[Raw]): Option[Raw]
}
