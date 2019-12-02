package dsentric

import cats.data.NonEmptyList

package object failure {
  type ValidResult[+T] = Either[NonEmptyList[Failure], T]
  type ValidStructural[+T] = Either[NonEmptyList[StructuralFailure], T]
  type ValidationFailures = List[Failure]

  object ValidationFailures {
    val empty:ValidationFailures = Nil

    def apply(v:Failure*):ValidationFailures =
      List(v:_*)
  }
}
