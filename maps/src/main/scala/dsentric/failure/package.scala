package dsentric

import cats.data.NonEmptyList

package object failure {
  type ValidResult[+T]    = Either[NonEmptyList[Failure], T]
  type ValidationFailures = List[Failure]

  object ValidationFailures {
    val empty: ValidationFailures = Nil

    def apply(v: Failure*): ValidationFailures =
      List(v: _*)
  }

  implicit def toValidResultOps[T](validResult: ValidResult[T]): ValidResultOps[T] =
    new ValidResultOps[T](validResult)
}
