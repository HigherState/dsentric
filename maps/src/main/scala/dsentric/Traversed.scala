package dsentric

import dsentric.contracts.{BaseAux, ContractFor}
import dsentric.failure.{Failure, StructuralFailure, ValidResult}

sealed trait Traversed[+T] {
  def toValidOption:ValidResult[Option[T]]

  def rebase(base:BaseAux):Traversed[T]
  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):Traversed[T]
  def rebase(rootPath:Path):Traversed[T]

  def flatMap[A](f:T => Traversed[A]):Traversed[A]

  def failNotFound(failure: => StructuralFailure):Traversed[T]
}
sealed trait Available[+T] extends Traversed[T] {
  def rebase(base:BaseAux):Available[T]
  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):Available[T]
  def rebase(rootPath:Path):Available[T]
  def failNotFound(failure: => StructuralFailure):Available[T]
}

case object PathEmptyMaybe extends Traversed[Nothing] {
  def toValidOption: ValidResult[Option[Nothing]] = ValidResult.none

  def rebase(baseContract:BaseAux): Traversed[Nothing] = this
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Traversed[Nothing] = this
  def rebase(rootPath: Path): Traversed[Nothing] = this

  def flatMap[A](f: Nothing => Traversed[A]): Traversed[A] = this

  def failNotFound(failure: => StructuralFailure):Traversed[Nothing] =
    PathEmptyMaybe
}

case object NotFound extends Available[Nothing] {
  def toValidOption: ValidResult[Option[Nothing]] = ValidResult.none

  def rebase(base:BaseAux): Available[Nothing] = this
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Available[Nothing] = this
  def rebase(rootPath: Path): Available[Nothing] = this

  def flatMap[A](f: Nothing => Traversed[A]): Traversed[A] = this

  def failNotFound(failure: => StructuralFailure): Available[Nothing] = Failed(failure, Nil)
}

final case class Found[+T](value:T) extends Available[T] {
  def toValidOption: ValidResult[Option[T]] = ValidResult.success((Some(value)))

  def rebase(base:BaseAux): Found[T] = this
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Found[T] = this
  def rebase(rootPath: Path): Available[T] = this

  def flatMap[A](f: T => Traversed[A]): Traversed[A] =
    f(value)

  def failNotFound(failure: => StructuralFailure): Found[T] = this
}

final case class Failed(failure: StructuralFailure, tail: List[StructuralFailure] = Nil) extends Available[Nothing] {
  def toValidOption: ValidResult[Option[Nothing]] = ValidResult.failure(failure, tail:_*)

  def rebase(base:BaseAux): Failed =
    rebase(base._root, base._path)
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failed =
    Failed(failure.rebase(rootContract, rootPath), tail.map(_.rebase(rootContract, rootPath)))
  def rebase(rootPath: Path): Available[Nothing] =
    Failed(failure.rebase(rootPath), tail.map(_.rebase(rootPath)))
  def flatMap[A](f: Nothing => Traversed[A]): Traversed[A] = this

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
 */
sealed trait DeltaReduce[+R]

final case class DeltaFailed(head:Failure, tail:List[Failure] = Nil) extends DeltaReduce[Nothing]
final case class DeltaReduced[R](delta:R) extends DeltaReduce[R]
case object DeltaEmpty extends DeltaReduce[Nothing]
case object DeltaRemove extends DeltaReduce[Nothing]

object DeltaReduce {
  def apply[R](value:Option[R]):DeltaReduce[R] =
    value.fold[DeltaReduce[R]](DeltaEmpty)(DeltaReduced(_))
}