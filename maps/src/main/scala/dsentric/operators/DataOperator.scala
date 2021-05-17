package dsentric.operators

import dsentric.contracts.ContractFor
import dsentric.failure.ValidationFailures
import dsentric.{DObject, Path, Raw}
import dsentric.schema.TypeDefinition


sealed trait DataOperator[+T] {
  def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
    case t => t
  }
}


trait ContextValidator[+T] extends DataOperator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]): ValidationFailures
}

trait Transform[+T] extends DataOperator[T] {
  def transform[S >: T](value: Option[S]):Option[S]
}

trait ContextTransform[C, +T] extends DataOperator[T] {
  def transform[S >: T](context:C, value:Option[S]):Option[S]
}

trait DeltaConstraint[+T] extends DataOperator[T] {
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
def verifyDelta[D <: DObject](
                                 contract:ContractFor[D],
                                 path:Path,
                                 reducedDelta:Raw,
                                 currentState: Raw
                               ): ValidationFailures
}

trait Sanitizer[+T] extends DataOperator[T] {

  def sanitize(value:Option[Raw]):Option[Raw]
}


