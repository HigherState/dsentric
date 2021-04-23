package dsentric

import dsentric.contracts.{BaseAux, ContractFor}
import dsentric.failure.{StructuralFailure, ValidResult}

sealed trait Traversed[+T] {
  def toValidOption:ValidResult[Option[T]]

  def rebase(base:BaseAux):Traversed[T]
  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):Traversed[T]

  def flatMap[A](f:T => Traversed[A]):Traversed[A]
}
sealed trait Available[+T] extends Traversed[T] {
  def rebase(base:BaseAux):Available[T]
  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):Available[T]
}

case object PathEmptyMaybe extends Traversed[Nothing] {
  def toValidOption: ValidResult[Option[Nothing]] = ValidResult.none

  def rebase(baseContract:BaseAux): Traversed[Nothing] = this

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Traversed[Nothing] = this

  def flatMap[A](f: Nothing => Traversed[A]): Traversed[A] = this
}

case object NotFound extends Available[Nothing] {
  def toValidOption: ValidResult[Option[Nothing]] = ValidResult.none

  def rebase(base:BaseAux): Available[Nothing] = this
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Available[Nothing] = this

  def flatMap[A](f: Nothing => Traversed[A]): Traversed[A] = this
}

final case class Found[+T](value:T) extends Available[T] {
  def toValidOption: ValidResult[Option[T]] = ValidResult.success((Some(value)))

  def rebase(base:BaseAux): Found[T] = this
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Found[T] = this

  def flatMap[A](f: T => Traversed[A]): Traversed[A] =
    f(value)
}

final case class Failed(failure: StructuralFailure, tail: List[StructuralFailure] = Nil) extends Available[Nothing] {
  def toValidOption: ValidResult[Option[Nothing]] = ValidResult.failure(failure, tail:_*)

  def rebase(base:BaseAux): Failed =
    rebase(base._root, base._path)

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failed =
    Failed(failure.rebase(rootContract, rootPath), tail.map(_.rebase(rootContract, rootPath)))

  def flatMap[A](f: Nothing => Traversed[A]): Traversed[A] = this
}