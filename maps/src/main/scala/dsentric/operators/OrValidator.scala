package dsentric.operators

import dsentric.{Failures, Path}

case class OrValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]):Failures =
    left(path, value, currentState) match {
      case Failures.empty =>
        Failures.empty
      case list =>
        right(path, value, currentState) match {
          case Failures.empty =>
            Failures.empty
          case list2 if list2.size < list.size =>
            list2
          case _ =>
            list
        }
    }

  private[dsentric] override def removalDenied:Boolean =
    left.removalDenied || right.removalDenied
}