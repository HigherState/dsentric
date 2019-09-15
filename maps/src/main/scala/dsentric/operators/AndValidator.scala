package dsentric.operators

import dsentric.{PathFailures, Path}
import dsentric.schema.TypeDefinition

case class AndValidator[+T, A <: T, B <: T](left:ValueValidator[A], right:ValueValidator[B]) extends ValueValidator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]):PathFailures =
    left(path, value, currentState) ++ right(path, value, currentState)

  override def definition:PartialFunction[TypeDefinition, TypeDefinition] =
    left.definition.andThen(right.definition)
}
