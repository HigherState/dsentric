package dsentric.operators

import dsentric.{DCodec, Data, Empty, Path, PathFailures, Raw, ValidResult}

trait ValidatorSanitizers {

  //Shouldnt be used in an And or Or validator
  val internal: RawValidator[Option[Nothing]] with Sanitizer[Nothing] =
    new RawValidator[Option[Nothing]] with Sanitizer[Nothing]{

      def apply(path:Path, value:Option[Raw], currentState:Option[Raw]): PathFailures =
        value.fold(PathFailures.empty)(_ => PathFailures(path -> "Value is reserved and cannot be provided."))

      def sanitize[S >: Nothing]: Function[ValidResult[S], Option[Data]] = {
        _ => None
      }
    }

  def mask[T](mask:T)(implicit D:DCodec[T]):RawValidator[Option[Nothing]] with Sanitizer[Nothing] =
    new RawValidator[Option[Nothing]] with Sanitizer[Nothing] {
      private val dataMask = D(mask)
      def sanitize[S >: Nothing]: ValidResult[S] => Option[Data] = {
        case Empty =>
          None
        case _ =>
          Some(dataMask)
      }

      def apply(path: Path, value: Option[Raw], currentState: Option[Raw]): PathFailures =
        if (value.contains(dataMask.value))
          PathFailures(path -> "Value cannot be the same as its mask.")
        else
          PathFailures.empty
    }

  def maskEmpty[T](mask:T)(implicit D:DCodec[T]):RawValidator[Option[Nothing]] with Sanitizer[Nothing] =
    new RawValidator[Option[Nothing]] with Sanitizer[Nothing] {
      private val dataMask = D(mask)

      def sanitize[S >: Nothing]: ValidResult[S] => Option[Data] = {
        _ => Some(dataMask)
      }

      def apply(path: Path, value: Option[Raw], currentState: Option[Raw]): PathFailures =
        if (value.contains(dataMask.value))
          PathFailures(path -> "Value cannot be the same as its mask.")
        else
          PathFailures.empty
    }

}
