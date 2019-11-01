package dsentric.failure

import dsentric.contracts.PropertyLens
import dsentric.{DCodec, DObject, Path}

sealed trait Failure {
  def path:Path
}

case class ExpectedFailure(path:Path) extends Failure

case class IncorrectTypeFailure[T](path:Path, codec:DCodec[T]) extends Failure {

}

object ExpectedFailure {
  def apply[D <: DObject, T](property: PropertyLens[D, T]):ExpectedFailure =
    ExpectedFailure(property._path)
}

object IncorrectTypeFailure {
  def apply[D <: DObject, T](property:PropertyLens[D, T]):IncorrectTypeFailure[T] = IncorrectTypeFailure(property._path, property._codec)
}