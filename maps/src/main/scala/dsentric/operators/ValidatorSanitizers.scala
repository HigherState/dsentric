package dsentric.operators

import dsentric.failure._
import dsentric._
import dsentric.contracts.ContractFor

trait ValidatorSanitizers {

  //Shouldnt be used in an And or Or validator
  val internal: RawValidator[Option[Nothing]] with Sanitizer[Nothing] =
    new RawValidator[Option[Nothing]] with Sanitizer[Nothing]{

      def apply[D <: DObject](contract:ContractFor[D], path:Path, value:Option[Raw], currentState:Option[Raw]): ValidationFailures =
        value.fold(ValidationFailures.empty)(_ => ValidationFailures(ReservedFailure(contract, path)))

      def sanitize[S >: Nothing]: Function[ValidResult[S], Option[Data]] = {
        _ => None
      }
    }

  def mask[T](mask:T)(implicit D:DCodec[T]):RawValidator[Option[Nothing]] with Sanitizer[Nothing] =
    new RawValidator[Option[Nothing]] with Sanitizer[Nothing] {
      private val dataMask = D(mask)
      def sanitize[S >: Nothing]: ValidResult[S] => Option[Data] = {
        case Left(_) =>
          None
        case _ =>
          Some(dataMask)
      }

      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
        if (value.contains(dataMask.value))
          ValidationFailures(MaskFailure(contract, path, mask))
        else
          ValidationFailures.empty
    }

  def maskEmpty[T](mask:T)(implicit D:DCodec[T]):RawValidator[Option[Nothing]] with Sanitizer[Nothing] =
    new RawValidator[Option[Nothing]] with Sanitizer[Nothing] {
      private val dataMask = D(mask)

      def sanitize[S >: Nothing]: ValidResult[S] => Option[Data] = {
        _ => Some(dataMask)
      }

      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
        if (value.contains(dataMask.value))
          ValidationFailures(MaskFailure(contract, path, mask))
        else
          ValidationFailures.empty
    }

}
