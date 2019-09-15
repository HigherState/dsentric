package dsentric.operators

import dsentric.{Data, Path, PathFailures, Raw}

trait ValidatorSanitizers {

  //Shouldnt be used in an And or Or validator
  val internal: RawValidator[Option[Nothing]] with Sanitizer[Nothing] =
    new RawValidator[Option[Nothing]] with Sanitizer[Nothing]{

      def apply(path:Path, value:Option[Raw], currentState:Option[Raw]): PathFailures =
        value.fold(PathFailures.empty)(_ => PathFailures(path -> "Value is reserved and cannot be provided."))

      def sanitize[S >: Nothing](value: Option[S]):Option[Data] =
        None

    }
}
