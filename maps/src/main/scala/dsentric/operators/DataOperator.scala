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

sealed trait ExpectedDataOperator[+T] extends DataOperator[T]

sealed trait MaybeDataOperator[+T] extends DataOperator[T]

sealed trait DefaultDataOperator[+T] extends DataOperator[T]

sealed trait UniversalDataOperator[+T] extends DataOperator[T]



sealed trait Validator[+T] extends DataOperator[T]

trait RawValidator[+T] extends Validator[T] {

  def apply[D <: DObject](contract:ContractFor[D], path:Path, value:Option[Raw], currentState:Option[Raw]):ValidationFailures
}

trait ValueValidator[+T] extends Validator[T] {

  def apply[S >: T, D <: DObject](contract:ContractFor[D], path:Path, value:S, currentState: => Option[S]): ValidationFailures

  def &&[S >: T] (v:ValueValidator[S]):ValueValidator[S] =
    AndValidator(this, v)

  def ||[S >: T] (v:ValueValidator[S]):ValueValidator[S] =
    OrValidator(this, v)
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

trait Sanitizer[+T] extends DataOperator[T] {

  def sanitize(value:Option[Raw]):Option[Raw]
}


