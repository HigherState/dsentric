package dsentric.operators

import com.github.ghik.silencer.silent
import dsentric.codecs.DCodec
import dsentric.{
  Available,
  DObject,
  DeltaEmpty,
  DeltaFailed,
  DeltaReduce,
  DeltaReduced,
  Failed,
  Found,
  NotFound,
  Path,
  Raw
}
import dsentric.contracts.ContractFor
import dsentric.failure.{ImmutableFailure, MaskFailure, ReservedFailure, ValidationFailures}

trait StandardOperators {
  //Shouldnt be used in an And or Or validator
  val internal: Internal.type = Internal

  val reserved: Constraint[Nothing] with Optional =
    new Constraint[Nothing] with Optional {

      def verify[D <: DObject](contract: ContractFor[D], path: Path, value: Available[Raw]): ValidationFailures =
        value match {
          case NotFound | _: Failed =>
            ValidationFailures.empty
          case _                    =>
            ValidationFailures(ReservedFailure(contract, path))
        }

      /**
       * Verify the reduced delta value against the current State
       *
       * @param contract
       * @param path
       * @param reducedDelta
       * @param currentState
       * @tparam S
       * @tparam D
       * @return
       */
      def verify[D <: DObject](
        contract: ContractFor[D],
        path: Path,
        current: Raw,
        delta: DeltaReduce[Raw]
      ): ValidationFailures =
        delta match {
          case DeltaEmpty =>
            ValidationFailures.empty
          case _          =>
            ValidationFailures(ReservedFailure(contract, path))
        }
    }

  val immutable: Constraint[Nothing] with Optional with Expected =
    new Constraint[Nothing] with Optional with Expected {

      def verify[D <: DObject](contract: ContractFor[D], path: Path, value: Available[Raw]): ValidationFailures = Nil

      /**
       * Verify the reduced delta value against the current State
       *
       * @param contract
       * @param path
       * @param reducedDelta
       * @param currentState
       * @tparam S
       * @tparam D
       * @return
       */
      def verify[D <: DObject](
        contract: ContractFor[D],
        path: Path,
        current: Raw,
        delta: DeltaReduce[Raw]
      ): ValidationFailures =
        delta match {
          case DeltaEmpty | _: DeltaFailed =>
            ValidationFailures.empty
          case _                           =>
            ValidationFailures(ImmutableFailure(contract, path))
        }
    }

//  val increment: Constraint[Numeric] =
//    new Constraint[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          c <- currentState.toList
//          (r, a, b) <- resolve(c, value)
//          if r > 0
//        } yield NumericalFailure(contract, path, a, b, "greater than or equal to")
//    }
//
//  val decrement: ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          c <- currentState.toList
//          (r, a, b) <- resolve(c, value)
//          if r < 0
//        } yield NumericalFailure(contract, path, a, b, "less than or equal to")
//
//    }

  def mask[T](mask: T, maskIfEmpty: Boolean = false)(implicit D: DCodec[T]): Constraint[Nothing] with Sanitizer[Nothing] =
    new Constraint[Nothing] with Sanitizer[Nothing] {
      private val dataMask: Raw = D(mask)

      def sanitize(value: Option[Raw]): Option[Raw] =
        if (maskIfEmpty)
          Some(dataMask)
        else
          value.map(_ => dataMask)

      def verify[D <: DObject](contract: ContractFor[D], path: Path, value: Available[Raw]): ValidationFailures =
        value match {
          case Found(delta) if delta == dataMask =>
            ValidationFailures(MaskFailure(contract, path, mask))
          case _                                 =>
            ValidationFailures.empty
        }

      /**
       * Verify the reduced delta value against the current State
       *
       * @param contract
       * @param path
       * @param reducedDelta
       * @param currentState
       * @tparam S
       * @tparam D
       * @return
       */
      def verify[D <: DObject](
        contract: ContractFor[D],
        path: Path,
        current: Raw,
        delta: DeltaReduce[Raw]
      ): ValidationFailures =
        delta match {
          case DeltaReduced(delta) if delta == dataMask =>
            ValidationFailures(MaskFailure(contract, path, mask))
          case _                                        =>
            ValidationFailures.empty
        }

    }
  @silent
  def maskTo[T, U](function: T => Option[U], default: Option[U])(implicit DT: DCodec[T], DU: DCodec[U]): Sanitizer[T]    =
    (value: Option[Raw]) => value.flatMap(DT.unapply).fold(default)(function)

  //Include:
  //Min max key constraint
  //Cant remove key constraint
}

object StandardOperators extends StandardOperators
