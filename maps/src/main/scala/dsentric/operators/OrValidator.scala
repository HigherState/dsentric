package dsentric.operators

import dsentric.{PathFailures, Path}

case class OrValidator[+T, A <: T, B <: T](left:ValueValidator[A], right:ValueValidator[B]) extends ValueValidator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]):PathFailures =
    left(path, value, currentState) match {
      case PathFailures.empty =>
        PathFailures.empty
      case list =>
        right(path, value, currentState) match {
          case PathFailures.empty =>
            PathFailures.empty
          case list2 if list2.size < list.size =>
            list2
          case _ =>
            list
        }
    }
}