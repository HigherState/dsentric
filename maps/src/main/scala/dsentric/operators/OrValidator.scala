package dsentric.operators

import dsentric.contracts.ContractFor
import dsentric.failure.ValidationFailures
import dsentric.{DObject, Path}

case class OrValidator[+T, A <: T, B <: T](left:ValueValidator[A], right:ValueValidator[B]) extends ValueValidator[T] {
  def apply[S >: T, D <: DObject](contract: ContractFor[D], path:Path, value:S, currentState: => Option[S]):ValidationFailures =
    left(contract, path, value, currentState) match {
      case ValidationFailures.empty =>
        ValidationFailures.empty
      case list =>
        right(contract, path, value, currentState) match {
          case ValidationFailures.empty =>
            ValidationFailures.empty
          case list2 if list2.size < list.size =>
            list2
          case _ =>
            list
        }
    }
}