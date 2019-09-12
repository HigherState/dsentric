package dsentric.operators

import dsentric.{Failures, Path}
import dsentric.schema.TypeDefinition

case class AndValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]):Failures =
    left(path, value, currentState) ++ right(path, value, currentState)

  override def definition:PartialFunction[TypeDefinition, TypeDefinition] =
    left.definition.andThen(right.definition)

  private[dsentric] override def removalDenied:Boolean =
    left.removalDenied || right.removalDenied
}
