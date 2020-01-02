package dsentric.operators

import dsentric.contracts.ContractFor
import dsentric.failure.ValidationFailures
import dsentric.{DObject, Path}
import dsentric.schema.TypeDefinition

case class AndValidator[+T, A <: T, B <: T](left:ValueValidator[A], right:ValueValidator[B]) extends ValueValidator[T] {
  def apply[S >: T, D <: DObject](contract:ContractFor[D], path:Path, value:Option[S], currentState: => Option[S]):ValidationFailures =
    left(contract, path, value, currentState) ++ right(contract, path, value, currentState)

  override def definition[D <: TypeDefinition]:PartialFunction[D, D] =
    left.definition[D].andThen(right.definition[D])
}
