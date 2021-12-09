package dsentric.failure

import cats.data.NonEmptyList
import dsentric.{DObject, Path}
import dsentric.contracts.ContractLike

trait ValidResultSyntax {

  implicit def toValidResultOps[T](validResult: ValidResult[T]): ValidResultOps[T] =
    new ValidResultOps[T](validResult)
}

final class ValidResultOps[T](val validResult: ValidResult[T]) extends AnyVal {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): ValidResult[T] =
    validResult match {
      case Left(vf) =>
        Left(vf.map(_.rebase(rootContract, rootPath)))
      case r        =>
        r
    }

  def toFailureList: List[Failure] =
    validResult match {
      case Left(NonEmptyList(head, tail)) => head :: tail
      case _                              => Nil
    }
}
