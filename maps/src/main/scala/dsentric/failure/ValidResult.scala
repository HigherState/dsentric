package dsentric.failure

import cats.CommutativeApplicative
import cats.data.{NonEmptyList, Validated}

object ValidResult {

  def failure[T](failure:StructuralFailure, additional:List[StructuralFailure] = Nil):ValidStructural[T] =
    Left(NonEmptyList(failure, additional))

  def failure[T](failure:Failure, additional:List[Failure] = Nil):ValidResult[T] =
    Left(NonEmptyList(failure, additional))

  def success[T](t:T):Right[Nothing, T] =
    Right(t)

  def fromList(t:List[Failure]):ValidResult[Unit] =
    t match {
      case head :: tail =>
        Left(NonEmptyList(head, tail))
      case _ =>
        ValidResult.unit
    }

  val none: Right[Nothing, None.type] =
    ValidResult.success(None)

  val unit: ValidResult[Unit] =
    ValidResult.success(())

  def sequence2[S, T](s:ValidResult[S], t:ValidResult[T]):ValidResult[(S, T)] = {
    (s, t) match {
      case (Right(sv), Right(tv)) =>
        Right(sv -> tv)
      case (Left(sf), Left(tf)) =>
        Left(sf ++ tf.toList)
      case (Left(sf), _) =>
        Left(sf)
      case (_, Left(tf)) =>
        Left(tf)
    }
  }

  implicit val commutativeApplicative: CommutativeApplicative[ValidResult] = new CommutativeApplicative[ValidResult] {
    def pure[A](x: A): ValidResult[A] = Right(x)

    def ap[A, B](ff: ValidResult[A => B])(fa: ValidResult[A]): ValidResult[B] =
      sequence2(ff, fa).map(p => p._1(p._2))
  }

  implicit val commutativeApplicativeValidated:CommutativeApplicative[({type L[A] = cats.data.Validated[NonEmptyList[Failure], A]})#L] =
    new CommutativeApplicative[({type L[A] = cats.data.Validated[NonEmptyList[Failure], A]})#L] {
      def pure[A](x: A): Validated[NonEmptyList[Failure], A] =
        Validated.Valid(x)

      def ap[A, B](ff: Validated[NonEmptyList[Failure], A => B])(fa: Validated[NonEmptyList[Failure], A]): Validated[NonEmptyList[Failure], B] =
        (ff, fa) match {
          case (Validated.Valid(sv), Validated.Valid(tv)) =>
            Validated.Valid(sv(tv))
          case (Validated.Invalid(sf), Validated.Invalid(tf)) =>
            Validated.Invalid(sf ++ tf.toList)
          case (Validated.Invalid(sf), _) =>
            Validated.Invalid(sf)
          case (_, Validated.Invalid(tf)) =>
            Validated.Invalid(tf)
        }
    }
}
