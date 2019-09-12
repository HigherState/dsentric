package dsentric.operators

import dsentric.{Failures, Path, Raw}

trait ValidatorSanitizers {

  val internal: Validator[Option[Nothing]] with Sanitizer[Nothing, Nothing] =
    new Validator[Option[Nothing]] with Sanitizer[Nothing, Nothing]{
      def apply[S >: Option[Nothing]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        value.fold(Failures.empty)(_ => Failures(path -> "Value is reserved and cannot be provided."))

      def sanitize[S >: Nothing](value: Option[S]):Option[Raw] =
        None

    }
}
