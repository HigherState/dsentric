package dsentric

import dsentric.contracts.{BaseAux, ContractFor}
import dsentric.failure.{Failure, StructuralFailure, ValidResult, ValidStructural}

sealed trait MaybeAvailable[+T] {
  def toValidOption:ValidStructural[Option[T]]

  def rebase(base:BaseAux):MaybeAvailable[T]
  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):MaybeAvailable[T]

  def flatMap[A](f:T => MaybeAvailable[A]):MaybeAvailable[A]

  def failNotFound(failure: => StructuralFailure):MaybeAvailable[T]
}
sealed trait Available[+T] extends MaybeAvailable[T] {
  def rebase(base:BaseAux):Available[T]
  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):Available[T]
  def failNotFound(failure: => StructuralFailure):Available[T]
}

sealed trait Valid[+T] extends Available[T] {
  def toValid:ValidStructural[T]
}

case object PathEmptyMaybe extends MaybeAvailable[Nothing] {
  def toValidOption: ValidStructural[Option[Nothing]] =
    ValidResult.none

  def rebase(baseContract:BaseAux): MaybeAvailable[Nothing] = this
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): MaybeAvailable[Nothing] = this

  def flatMap[A](f: Nothing => MaybeAvailable[A]): MaybeAvailable[A] = this

  def failNotFound(failure: => StructuralFailure):MaybeAvailable[Nothing] =
    PathEmptyMaybe
}

case object NotFound extends Available[Nothing] {
  def toValidOption: ValidStructural[Option[Nothing]] =
    ValidResult.none

  def rebase(base:BaseAux): Available[Nothing] = this
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Available[Nothing] = this

  def flatMap[A](f: Nothing => MaybeAvailable[A]): MaybeAvailable[A] = this

  def failNotFound(failure: => StructuralFailure): Available[Nothing] = Failed(failure, Nil)
}

final case class Found[+T](value:T) extends Valid[T] {
  def toValidOption: ValidStructural[Option[T]] =
    ValidResult.success((Some(value)))

  def rebase(base:BaseAux): Found[T] = this
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Found[T] = this

  def flatMap[A](f: T => MaybeAvailable[A]): MaybeAvailable[A] =
    f(value)


  def toValid: ValidStructural[T] =
    ValidResult.success(value)

  def failNotFound(failure: => StructuralFailure): Found[T] = this
}

final case class Failed(failure: StructuralFailure, tail: List[StructuralFailure] = Nil) extends Valid[Nothing] {
  def toValidOption: ValidStructural[Option[Nothing]] =
    ValidResult.structuralFailure(failure, tail)

  def rebase(base:BaseAux): Failed =
    rebase(base._root, base._path)
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failed =
    Failed(failure.rebase(rootContract, rootPath), tail.map(_.rebase(rootContract, rootPath)))

  def flatMap[A](f: Nothing => MaybeAvailable[A]): MaybeAvailable[A] = this

  def toValid: ValidStructural[Nothing] =
    ValidResult.structuralFailure(failure, tail)

  def failNotFound(failure: => StructuralFailure): Failed = this
}

object Available {
  def sequence2[S, T](s:Available[S], t:Available[T]):Available[(S, T)] = {
    (s, t) match {
      case (Found(sv), Found(tv)) =>
        Found(sv -> tv)
      case (Failed(head1, tail1), Failed(head2, tail2)) =>
       Failed(head1, tail1 :+ head2 :++ tail2)
      case (f:Failed, _) =>
        f
      case (_, f:Failed) =>
        f
      case _ =>
        NotFound
    }
  }
}

/**
 * Algebra for validating and reducing Delta
 * Reducing a delta removes any redundancy in the delta object
 * This can include removing null values which drop no value, or values which correspond to current values.
 * If the delta causes an object to become empty, we capture that in DeltaRemoving
 */
sealed trait DeltaReduce[+R]

final case class DeltaFailed(head:Failure, tail:List[Failure] = Nil) extends DeltaReduce[Nothing]
final case class DeltaReduced[R](delta:R) extends DeltaReduce[R]
final case class DeltaRemoving(delta:RawObject) extends DeltaReduce[RawObject]
case object DeltaEmpty extends DeltaReduce[Nothing]
case object DeltaRemove extends DeltaReduce[Nothing]

object DeltaReduce {
  def apply[R](value:Option[R]):DeltaReduce[R] =
    value.fold[DeltaReduce[R]](DeltaEmpty)(DeltaReduced(_))
}