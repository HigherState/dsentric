package dsentric.failure

import dsentric.codecs.{DCodec, DCoproductCodec}
import dsentric.contracts.{ContractFor, PropertyLens}
import dsentric.{DObject, Data, Path, Raw, RawObject}
import shapeless.HList

import scala.util.matching.Regex

sealed trait Failure {
  def path: Path

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failure

  def message: String
}

sealed trait TypeFailure extends Failure

final case class ExpectedFailure[D <: DObject](contract: ContractFor[D], path: Path) extends Failure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ExpectedFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message = "Expected value not found."
}

final case class IncorrectTypeFailure[D <: DObject, T](
  contract: ContractFor[D],
  path: Path,
  codec: DCodec[T],
  foundRaw: Raw
) extends TypeFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): IncorrectTypeFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message = s"Type '${codec.typeDefinition.name}' was expected, $foundRaw was found."
}

final case class IncorrectKeyTypeFailure[D <: DObject, T](
  contract: ContractFor[D],
  path: Path,
  codec: DCodec[T],
  foundRaw: String
) extends TypeFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): IncorrectKeyTypeFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message = s"Type '${codec.typeDefinition.name}' was expected for additional properties key, $foundRaw was found."
}

final case class ClosedContractFailure[D <: DObject](contract: ContractFor[D], path: Path, field: String)
    extends Failure {
  def message: String = s"Contract is closed and cannot have value for field '$field'."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ClosedContractFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class ContractFieldFailure[D <: DObject](contract: ContractFor[D], path: Path, field: String) extends Failure {
  def message: String = s"Contract field '$field' is already defined and cannot be used for additional property."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failure =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class ContractTypeResolutionFailure[D <: DObject](contract: ContractFor[D], path: Path, foundRaw: RawObject)
    extends Failure {
  def message: String = s"Contract type cannot be resolved with given object."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failure =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MissingElementFailure[D <: DObject, T](contract: ContractFor[D], codec: DCodec[T], path: Path)
    extends Failure {
  def message = s"Type '${codec.typeDefinition.name} was expected, nothing was found."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): MissingElementFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class AdditionalElementFailure[D <: DObject](contract: ContractFor[D], path: Path) extends Failure {
  def message = s"Array contained unexpected element."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): AdditionalElementFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class UnexpectedValueFailure[D <: DObject, T](
  contract: ContractFor[D],
  codec: DCodec[T],
  expectedValue: Raw,
  unexpectedValue: Raw,
  path: Path
) extends Failure {
  def message =
    s"Type '${codec.typeDefinition.name}' expected value $expectedValue value, but $unexpectedValue was found."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): UnexpectedValueFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}
final case class CoproductTypeValueFailure[D <: DObject, T, H <: HList](
  contract: ContractFor[D],
  codec: DCoproductCodec[T, H],
  path: Path,
  coproductFailures: List[Failure],
  foundRaw: Raw
) extends Failure {
  def message = s"Type '${codec.typeDefinition.name}' was expected, $foundRaw was found."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): CoproductTypeValueFailure[G, T, H] =
    copy(contract = rootContract, path = rootPath ++ path)

}

final case class DeltaNotSupportedFailure[D <: DObject, T](contract: ContractFor[D], codec: DCodec[T], path: Path)
    extends Failure {
  def message = s"Type '${codec.typeDefinition.name}' does not support Delta operation."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): DeltaNotSupportedFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

}

final case class CustomPathFailure[D <: DObject, T](contract: ContractFor[D], path: Path, message: String)
    extends Failure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failure =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MatchFailure[D <: DObject](contract: ContractFor[D], path: Path, value: Data) extends Failure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failure =
    copy(contract = rootContract, path = rootPath ++ path)

  def message: String = "Unexpected match failure"
}

object EmptyPropertyFailure extends Failure {
  def path: Path = Path.empty

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): Failure = this

  def message: String = "Unexpected operation performed on an empty property"
}

object ExpectedFailure {
  def apply[D <: DObject, T](property: PropertyLens[D, T]): ExpectedFailure[D] =
    ExpectedFailure(property._root, property._path)
}

object IncorrectTypeFailure {
  def apply[D <: DObject, T](property: PropertyLens[D, T], foundRaw: Raw): IncorrectTypeFailure[D, T] =
    IncorrectTypeFailure(property._root, property._path, property._codec, foundRaw)
}

object ClosedContractFailure {
  def apply[D <: DObject, T](property: PropertyLens[D, T], field: String): ClosedContractFailure[D] =
    ClosedContractFailure(property._root, property._path, field)
}

object ContractFieldFailure {
  def apply[D <: DObject, T](property: PropertyLens[D, T], field: String): ContractFieldFailure[D] =
    ContractFieldFailure(property._root, property._path, field)
}

object MatchFailure {

  def apply[D <: DObject, T](property: PropertyLens[D, T], value: Data): MatchFailure[D] =
    MatchFailure(property._root, property._path, value)
}

sealed trait ConstraintFailure extends Failure

final case class RequiredFailure[D <: DObject](contract: ContractFor[D], path: Path) extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): RequiredFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message: String = "Value is required and cannot be dropped."
}

final case class ReservedFailure[D <: DObject](contract: ContractFor[D], path: Path) extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ReservedFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message: String = "Value is reserved and cannot be provided."
}

final case class ImmutableFailure[D <: DObject](contract: ContractFor[D], path: Path) extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ImmutableFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message: String = "Value is immutable and cannot be changed."
}

final case class WriteOnceFailure[D <: DObject](contract: ContractFor[D], path: Path) extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): WriteOnceFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message: String = "Once provided, value cannot be changed."
}

final case class NumericalFailure[D <: DObject](
  contract: ContractFor[D],
  path: Path,
  found: Number,
  compare: Number,
  operation: String
) extends ConstraintFailure {
  def message: String = s"Value $found is not $operation $compare."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): NumericalFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MinimumLengthFailure[D <: DObject](
  contract: ContractFor[D],
  path: Path,
  minLength: Int,
  foundLength: Int
) extends ConstraintFailure {
  def message: String = s"Value with length of $foundLength must have a minimum length of $minLength."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): MinimumLengthFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MaximumLengthFailure[D <: DObject](
  contract: ContractFor[D],
  path: Path,
  maxLength: Int,
  foundLength: Int
) extends ConstraintFailure {
  def message: String = s"Value with length of $foundLength must have a maximum length of $maxLength."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): MaximumLengthFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class InvalidValueFailure[D <: DObject, T](contract: ContractFor[D], path: Path, value: T)
    extends ConstraintFailure {
  def message: String = s"Value $value is not a permitted value."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): InvalidValueFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class NonEmptyOrWhitespaceFailure[D <: DObject](contract: ContractFor[D], path: Path)
    extends ConstraintFailure {
  def message: String = s"String value cannot be empty or whitespace."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): NonEmptyOrWhitespaceFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class CustomConstraintFailure[D <: DObject, T](
  contract: ContractFor[D],
  path: Path,
  value: T,
  message: String
) extends ConstraintFailure {

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): CustomConstraintFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class RegexFailure[D <: DObject](
  contract: ContractFor[D],
  path: Path,
  regex: Regex,
  value: String,
  message: String
) extends ConstraintFailure {

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): RegexFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class KeyRemovalFailure[D <: DObject, T](contract: ContractFor[D], path: Path, keyString: String)
    extends ConstraintFailure {
  def message: String = s"Key '$keyString' cannot be removed."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): KeyRemovalFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MaskFailure[D <: DObject, T](contract: ContractFor[D], path: Path, mask: T) extends ConstraintFailure {
  def message: String = s"Value cannot be the same as the mask value '$mask'."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): MaskFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}
