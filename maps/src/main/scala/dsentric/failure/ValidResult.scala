package dsentric.failure

import cats.data.NonEmptyList

object ValidResult {

  def failure[T](failure:Failure, additional:Failure*):ValidResult[T] =
    Left(NonEmptyList(failure, additional.toList))

  def success[T](t:T):ValidResult[T] =
    Right(t)

}