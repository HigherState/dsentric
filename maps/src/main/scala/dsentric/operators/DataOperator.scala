package dsentric.operators

import dsentric.{Failures, Path, Raw}
import dsentric.schema.{ObjectDefinition, TypeDefinition}

sealed trait DataOperator[+T]

trait Validator[+T] extends DataOperator[T]{

  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]): Failures

  def &&[S >: T] (v:Validator[S]):Validator[S] =
    AndValidator(this, v)

  def ||[S >: T] (v:Validator[S]):Validator[S] =
    OrValidator(this, v)

  def apply[A <: TypeDefinition](t:A, objDefs:Vector[ObjectDefinition], forceNested:Boolean):(A, Vector[ObjectDefinition]) =
    withObjsDefinition(forceNested).lift(t -> objDefs).getOrElse(t -> objDefs).asInstanceOf[(A, Vector[ObjectDefinition])]

  def withObjsDefinition(forceNested:Boolean):PartialFunction[(TypeDefinition, Vector[ObjectDefinition]), (TypeDefinition, Vector[ObjectDefinition])] = {
    case (t, o) if definition.isDefinedAt(t) => definition(t) -> o
  }

  def definition:PartialFunction[TypeDefinition, TypeDefinition] = {
    case t => t
  }

  private[dsentric] def removalDenied:Boolean = false
  private[dsentric] def isEmpty:Boolean = false
}

trait ContextValidator[+T] extends DataOperator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]): Failures
}

trait Transform[+T] extends DataOperator[T] {
  def transform[S >: T](value: Option[S]):Option[S]
}

trait ContextTransform[C, +T] extends DataOperator[T] {
  def transform[S >: T](context:C, value:Option[S]):Option[S]
}

trait Sanitizer[+T] extends DataOperator[T] {
  def sanitize[S >: T](value: Option[S]):Option[Raw]
}


