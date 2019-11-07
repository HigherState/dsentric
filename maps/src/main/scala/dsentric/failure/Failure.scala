package dsentric.failure

import dsentric.contracts.{ContractFor, PropertyLens}
import dsentric.{DCodec, DObject, Path}

sealed trait Failure {
  def path:Path

  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):Failure
}

case class ExpectedFailure[D <: DObject](contract: ContractFor[D], path:Path) extends Failure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ExpectedFailure[G] =
    ExpectedFailure[G](rootContract, rootPath ++ path)
}

case class IncorrectTypeFailure[D <: DObject, T](contract: ContractFor[D], path:Path, codec:DCodec[T]) extends Failure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path):  IncorrectTypeFailure[G, T] =
    IncorrectTypeFailure[G, T](rootContract, rootPath ++ path, codec)
}

object ExpectedFailure {
  def apply[D <: DObject, T](property: PropertyLens[D, T]):ExpectedFailure[D] =
    ExpectedFailure(property._root, property._path)
}

object IncorrectTypeFailure {
  def apply[D <: DObject, T](property:PropertyLens[D, T]):IncorrectTypeFailure[D, T] =
    IncorrectTypeFailure(property._root, property._path, property._codec)
}