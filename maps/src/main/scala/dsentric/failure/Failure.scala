package dsentric.failure

import dsentric.{DCodec, Path}

sealed trait Failure {
  def path:Path
}

case class ExpectedFailure(path:Path) extends Failure

case class IncorrectTypeFailure[T](path:Path, codec:DCodec[T]) extends Failure