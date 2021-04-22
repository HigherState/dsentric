package dsentric.operators

import dsentric.contracts.ContractFor
import dsentric.failure.{ValidResult, ValidationFailures}
import dsentric.{DObject, Data, Path, Raw}
import dsentric.schema.{ObjectDefinition, TypeDefinition}


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

trait Constraint[+T] extends DataOperator[T] {
  def verifyDelta[S >: T, D <: DObject](
                                 contract:ContractFor[D],
                                 path:Path,
                                 currentState: Option[S],
                                 finalState: Option[S]
                               ): ValidationFailures
}

trait Sanitizer[+T] extends DataOperator[T] {

  def sanitize(value:Option[Raw]):Option[Raw]
}


