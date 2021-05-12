package dsentric.operators

import dsentric.codecs.DCodec
import dsentric.{DObject, Path, Raw, RawObjectOps}
import dsentric.contracts.ContractFor
import dsentric.failure.{ExpectedFailure, ImmutableFailure, MaskFailure, ReservedFailure, ValidationFailures, WriteOnceFailure}

trait StandardOperators {
  //Shouldnt be used in an And or Or validator
  val internal: Internal.type = Internal

  val reserved: DeltaConstraint[Option[Nothing]] =
    new DeltaConstraint[Option[Nothing]] {

      def verifyDelta[S >: Option[Nothing], D <: DObject](
                                                           contract:ContractFor[D],
                                                           path:Path,
                                                           delta:Raw,
                                                           currentState: Option[Raw]): ValidationFailures =
        ValidationFailures(ReservedFailure(contract, path))
    }

  val immutable: DeltaConstraint[Nothing] =
    new DeltaConstraint[Nothing] {

      def verifyDelta[S >: Nothing, D <: DObject](contract:ContractFor[D],
                                                  path:Path,
                                                  delta:Raw,
                                                  currentState: Option[Raw]): ValidationFailures =
        if (currentState.exists { s =>
          RawObjectOps.rightReduceConcatMap()
        })
          ValidationFailures(ImmutableFailure(contract, path))
        else
          ValidationFailures.empty
    }

  val writeOnce: DeltaConstraint[Option[Nothing]] =
    new DeltaConstraint[Nothing] {

      def verifyDelta[S >: Nothing, D <: DObject](contract: ContractFor[D], path: Path, currentState: Option[S], finalState: Option[S]): ValidationFailures =
        if (currentState.exists(s => !finalState.contains(s)))
          ValidationFailures(WriteOnceFailure(contract, path))
        else
          ValidationFailures.empty
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

  def mask[T](mask:T, maskIfEmpty:Boolean = false)(implicit D:DCodec[T]):DeltaConstraint[Optionable[Nothing]] with Sanitizer[Optionable[Nothing]] =
    new DeltaConstraint[Optionable[Nothing]] with Sanitizer[Optionable[Nothing]] {
      private val dataMask:Raw = D(mask)

      def sanitize(value: Option[Raw]): Option[Raw] =
        if (maskIfEmpty)
          Some(dataMask)
        else
          value.map(_ => dataMask)

      def verifyDelta[S >: Optionable[Nothing], D <: DObject](contract: ContractFor[D], path: Path, currentState: Option[S], finalState: Option[S]): ValidationFailures =
        if (finalState.contains(dataMask))
          ValidationFailures(MaskFailure(contract, path, mask))
        else
          ValidationFailures.empty
    }

  def maskTo[T, U](function:T => Option[U], default:Option[U])(implicit DT:DCodec[T], DU:DCodec[U]):Sanitizer[Optionable[T]] =
    (value: Option[Raw]) => value.flatMap(DT.unapply).fold(default)(function)


  //Include:
  //Min max key constraint
  //Cant remove key constraint
}
