package dsentric.failure

import dsentric.contracts.{ContractFor, Expected, PropertyLens}
import dsentric.{DCodec, DObject, Path, Raw, StringCodec}

import scala.util.matching.Regex

sealed trait Failure {
  def path:Path

  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):Failure

  def message:String
}

sealed trait StructuralFailure extends Failure {
  def rebase[G <: DObject](rootContract:ContractFor[G], rootPath:Path):StructuralFailure
}

sealed trait TypeFailure extends StructuralFailure

final case class ExpectedFailure[D <: DObject](contract: ContractFor[D], path:Path) extends StructuralFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ExpectedFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message = "Expected value not found."
}

final case class IncorrectTypeFailure[D <: DObject, T](contract: ContractFor[D], path:Path, codec:DCodec[T], foundRaw:Raw) extends TypeFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path):  IncorrectTypeFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message = s"Type '${codec.typeDefinition.name} was expected, type ${foundRaw.getClass.getSimpleName} was found."
}

final case class IncorrectKeyTypeFailure[D <: DObject, T](contract: ContractFor[D], path:Path, codec:DCodec[T]) extends TypeFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path):  IncorrectKeyTypeFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message = s"Type '${codec.typeDefinition.name} was expected for additional properties key."
}

final case class ClosedContractFailure[D <: DObject](contract: ContractFor[D], path:Path, field:String) extends StructuralFailure {
  def message: String = s"Contract is closed and cannot have value for field '$field'."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ClosedContractFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class ContractFieldFailure[D <: DObject](contract: ContractFor[D], path:Path, field:String) extends StructuralFailure {
  def message: String = s"Contract field '$field' is already defined and cannot be used for additional property."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): StructuralFailure =
    copy(contract = rootContract, path = rootPath ++ path)
}

object ExpectedFailure {
  def apply[D <: DObject, T](property: PropertyLens[D, T]):ExpectedFailure[D] =
    ExpectedFailure(property._root, property._path)
}

object IncorrectTypeFailure {
  def apply[D <: DObject, T](property:PropertyLens[D, T], foundRaw:Raw):IncorrectTypeFailure[D, T] =
    IncorrectTypeFailure(property._root, property._path, property._codec, foundRaw)
}

object ClosedContractFailure {
  def apply[D <: DObject, T](property:PropertyLens[D, T], field:String):ClosedContractFailure[D] =
    ClosedContractFailure(property._root, property._path, field)
}

object ContractFieldFailure {
  def apply[D <: DObject, T](property:PropertyLens[D, T], field:String):ContractFieldFailure[D] =
    ContractFieldFailure(property._root, property._path, field)
}



sealed trait ValidationFailure extends Failure

final case class ReservedFailure[D <: DObject](contract: ContractFor[D], path:Path) extends ValidationFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ReservedFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message:String = "Value is reserved and cannot be provided."
}

final case class ImmutableFailure[D <: DObject](contract: ContractFor[D], path:Path) extends ValidationFailure {
  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): ImmutableFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message:String = "Value is immutable and cannot be changed."
}

final case class NumericalFailure[D <: DObject](contract: ContractFor[D], path:Path, found:Number, compare:Number, operation:String) extends ValidationFailure {
  def message: String = s"Value $found is not $operation $compare."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): NumericalFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MinimumLengthFailure[D <: DObject](contract: ContractFor[D], path:Path, minLength:Int, foundLength:Int) extends ValidationFailure {
  def message: String = s"Value with length of $foundLength must have a minimum length of $minLength."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): MinimumLengthFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MaximumLengthFailure[D <: DObject](contract: ContractFor[D], path:Path, maxLength:Int, foundLength:Int) extends ValidationFailure {
  def message: String = s"Value with length of $foundLength must have a maximum length of $maxLength."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): MaximumLengthFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class InvalidValueFailure[D <: DObject, T](contract: ContractFor[D], path:Path, value:T) extends ValidationFailure {
  def message: String = s"Value $value is not a permitted value."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): InvalidValueFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class NonEmptyOrWhitespaceFailure[D <: DObject](contract: ContractFor[D], path:Path) extends ValidationFailure {
  def message: String = s"String value cannot be empty or whitespace."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): NonEmptyOrWhitespaceFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class CustomValidationFailure[D <: DObject, T](contract: ContractFor[D], path:Path, value:T, message:String) extends ValidationFailure {

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): CustomValidationFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class RegexFailure[D <: DObject](contract: ContractFor[D], path:Path, regex:Regex, value:String, message:String) extends ValidationFailure {

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): RegexFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class KeyRemovalFailure[D <: DObject, T](contract: ContractFor[D], path:Path, keyString:String) extends ValidationFailure {
  def message:String = s"Key '$keyString' cannot be removed."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): KeyRemovalFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MaskFailure[D <: DObject, T](contract: ContractFor[D], path:Path, mask:T) extends ValidationFailure {
  def message:String = s"Value cannot be the same as the mask value '$mask'."

  def rebase[G <: DObject](rootContract: ContractFor[G], rootPath: Path): MaskFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}