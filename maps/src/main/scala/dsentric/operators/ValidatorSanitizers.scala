package dsentric.operators

import dsentric.failure._
import dsentric._
import dsentric.contracts.ContractFor

trait ValidatorSanitizers {

  //Shouldnt be used in an And or Or validator
  val internal: RawValidator[Option[Nothing]] with Sanitizer[Option[Nothing]] =
    new RawValidator[Option[Nothing]] with Sanitizer[Option[Nothing]]{

      def apply[D <: DObject](contract:ContractFor[D], path:Path, value:Option[Raw], currentState:Option[Raw]): ValidationFailures =
        value.fold(ValidationFailures.empty)(_ => ValidationFailures(ReservedFailure(contract, path)))

      def sanitize(value: Option[Raw]): Option[Raw] = None
    }

  def mask[T](mask:T)(implicit D:DCodec[T]):RawValidator[Optionable[Nothing]] with Sanitizer[Optionable[Nothing]] =
    new RawValidator[Optionable[Nothing]] with Sanitizer[Optionable[Nothing]] {
      private val dataMask:Raw = D(mask).value

      def sanitize(value: Option[Raw]): Option[Raw] =
        value.map(_ => dataMask)

      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
        if (value.contains(dataMask))
          ValidationFailures(MaskFailure(contract, path, mask))
        else
          ValidationFailures.empty
    }

  def maskFunction[T, U](function:T => Option[U], default:Option[U])(implicit DT:DCodec[T], DU:DCodec[U]):Sanitizer[Optionable[T]] =
    (value: Option[Raw]) => value.flatMap(DT.unapply).fold(default)(function)

  def maskEmpty[T](mask:T)(implicit D:DCodec[T]):RawValidator[Option[Nothing]] with Sanitizer[Option[Nothing]] =
    new RawValidator[Option[Nothing]] with Sanitizer[Nothing] {
      private val dataMask: Raw = D(mask).value

      def sanitize(value: Option[Raw]): Option[Raw] =
        Some(dataMask)

      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
        if (value.contains(dataMask))
          ValidationFailures(MaskFailure(contract, path, mask))
        else
          ValidationFailures.empty
    }

}

object Sanitizers extends ValidatorSanitizers
