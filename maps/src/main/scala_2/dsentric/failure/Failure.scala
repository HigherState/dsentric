package dsentric.failure

import dsentric._
import dsentric.codecs.{DCodec, DCoproductCodec}
import dsentric.contracts.{ContractLike, PropertyLens}
import dsentric.schema._
import shapeless.HList

import scala.util.matching.Regex

sealed trait Failure {
  def path: Path

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): Failure

  def message(implicit renderer: Renderer): String

  protected def convertJson(raw: Raw)(implicit renderer: Renderer) = {
    val a = renderer.print(raw)
    if (a.length > 50) "'" + a.take(50) ++ "...'"
    else "'" + a + "'"
  }
}

sealed trait TypeFailure extends Failure

final case class ExpectedFailure[D <: DObject](contract: ContractLike[D], path: Path) extends Failure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): ExpectedFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer) = "Expected value not found."
}

final case class IncorrectTypeFailure[D <: DObject, T](
  contract: ContractLike[D],
  path: Path,
  codec: DCodec[T],
  foundRaw: Raw
) extends TypeFailure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): IncorrectTypeFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer) =
    s"${TypeDescriber.apply(codec.typeDefinition)} was expected, ${convertJson(foundRaw)} was found."
}

final case class IncorrectKeyTypeFailure[D <: DObject, T](
  contract: ContractLike[D],
  path: Path,
  codec: DCodec[T],
  foundRaw: String
) extends TypeFailure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): IncorrectKeyTypeFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer) =
    s"${TypeDescriber.apply(codec.typeDefinition)} was expected for additional properties key, ${convertJson(foundRaw)} was found."
}

final case class ClosedContractFailure[D <: DObject](contract: ContractLike[D], path: Path, field: String)
    extends Failure {
  def message(implicit renderer: Renderer): String = s"Contract is closed and cannot have value for field '$field'."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): ClosedContractFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class ContractFieldFailure[D <: DObject](contract: ContractLike[D], path: Path, field: String)
    extends Failure {
  def message(implicit renderer: Renderer): String =
    s"Contract field '$field' is already defined and cannot be used for additional property."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): Failure =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class ContractTypeResolutionFailure[D <: DObject](contract: ContractLike[D], path: Path, foundRaw: RawObject)
    extends Failure {
  def message(implicit renderer: Renderer): String = s"Contract type cannot be resolved with given object."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): Failure =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MissingElementFailure[D <: DObject, T](contract: ContractLike[D], codec: DCodec[T], path: Path)
    extends Failure {
  def message(implicit renderer: Renderer): String =
    s"${TypeDescriber.apply(codec.typeDefinition)} was expected, nothing was found."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): MissingElementFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class AdditionalElementFailure[D <: DObject](contract: ContractLike[D], path: Path) extends Failure {
  def message(implicit renderer: Renderer): String = s"Array contained unexpected element."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): AdditionalElementFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class UnexpectedValueFailure[D <: DObject, T](
  contract: ContractLike[D],
  codec: DCodec[T],
  expectedValue: Raw,
  unexpectedValue: Raw,
  path: Path
) extends Failure {
  def message(implicit renderer: Renderer) =
    s"${TypeDescriber.apply(codec.typeDefinition)} expected value ${convertJson(expectedValue)} value, but ${convertJson(unexpectedValue)} was found."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): UnexpectedValueFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}
final case class CoproductTypeValueFailure[D <: DObject, T, H <: HList](
  contract: ContractLike[D],
  codec: DCoproductCodec[T, H],
  path: Path,
  coproductFailures: List[Failure],
  foundRaw: Raw
) extends Failure {
  def message(implicit renderer: Renderer) =
    s"${TypeDescriber.apply(codec.typeDefinition)} was expected, ${convertJson(foundRaw)} was found."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): CoproductTypeValueFailure[G, T, H] =
    copy(contract = rootContract, path = rootPath ++ path)

}

final case class DeltaNotSupportedFailure[D <: DObject, T](contract: ContractLike[D], codec: DCodec[T], path: Path)
    extends Failure {
  def message(implicit renderer: Renderer) = s"Delta operation not supported."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): DeltaNotSupportedFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

}

final case class CustomPathFailure[D <: DObject, T](contract: ContractLike[D], path: Path, msg: String)
    extends Failure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): Failure =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = msg
}

final case class MatchFailure[D <: DObject](contract: ContractLike[D], path: Path, value: Data) extends Failure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): Failure =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = "Unexpected match failure."
}

object EmptyPropertyFailure extends Failure {
  def path: Path = Path.empty

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): Failure = this

  def message(implicit renderer: Renderer): String = "Unexpected operation performed on an empty property."
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

final case class RequiredFailure[D <: DObject](contract: ContractLike[D], path: Path) extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): RequiredFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = "Value is required and cannot be dropped."
}

final case class ReservedFailure[D <: DObject](contract: ContractLike[D], path: Path) extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): ReservedFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = "Value is reserved and cannot be provided."
}

final case class ImmutableFailure[D <: DObject](contract: ContractLike[D], path: Path) extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): ImmutableFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = "Value is immutable and cannot be changed."
}

final case class DeprecatedFailure[D <: DObject](contract: ContractLike[D], path: Path, remediation: String)
    extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): DeprecatedFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = s"Field is deprecated, value should not be provided. $remediation"
}

final case class WriteOnceFailure[D <: DObject](contract: ContractLike[D], path: Path) extends ConstraintFailure {
  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): WriteOnceFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = "Once provided, value cannot be changed."
}

final case class NumericalFailure[D <: DObject](
  contract: ContractLike[D],
  path: Path,
  found: Number,
  compare: Number,
  operation: String
) extends ConstraintFailure {
  def message(implicit renderer: Renderer): String = s"Value $found is not $operation $compare."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): NumericalFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MinimumLengthFailure[D <: DObject](
  contract: ContractLike[D],
  path: Path,
  minLength: Int,
  foundLength: Int
) extends ConstraintFailure {
  def message(implicit renderer: Renderer): String =
    s"Value with length of $foundLength must have a minimum length of $minLength."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): MinimumLengthFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MaximumLengthFailure[D <: DObject](
  contract: ContractLike[D],
  path: Path,
  maxLength: Int,
  foundLength: Int
) extends ConstraintFailure {
  def message(implicit renderer: Renderer): String =
    s"Value with length of $foundLength must have a maximum length of $maxLength."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): MaximumLengthFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class InvalidValueFailure[D <: DObject, T](contract: ContractLike[D], path: Path, value: T)
    extends ConstraintFailure {
  def message(implicit renderer: Renderer): String = s"Value $value is not a permitted value."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): InvalidValueFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class NonEmptyOrWhitespaceFailure[D <: DObject](contract: ContractLike[D], path: Path)
    extends ConstraintFailure {
  def message(implicit renderer: Renderer): String = s"String value cannot be empty or whitespace."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): NonEmptyOrWhitespaceFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class CustomConstraintFailure[D <: DObject, T](contract: ContractLike[D], path: Path, value: T, msg: String)
    extends ConstraintFailure {

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): CustomConstraintFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = msg
}

final case class RegexFailure[D <: DObject](
  contract: ContractLike[D],
  path: Path,
  regex: Regex,
  value: String,
  msg: String
) extends ConstraintFailure {

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): RegexFailure[G] =
    copy(contract = rootContract, path = rootPath ++ path)

  def message(implicit renderer: Renderer): String = msg
}

final case class KeyRemovalFailure[D <: DObject, T](contract: ContractLike[D], path: Path, keyString: String)
    extends ConstraintFailure {
  def message(implicit renderer: Renderer): String = s"Key '$keyString' cannot be removed."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): KeyRemovalFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

final case class MaskFailure[D <: DObject, T](contract: ContractLike[D], path: Path, mask: T)
    extends ConstraintFailure {
  def message(implicit renderer: Renderer): String = s"Value cannot be the same as the mask value '$mask'."

  def rebase[G <: DObject](rootContract: ContractLike[G], rootPath: Path): MaskFailure[G, T] =
    copy(contract = rootContract, path = rootPath ++ path)
}

object TypeDescriber {

  def apply: TypeDefinition => String = {
    case StringDefinition(enumeration, _, _, _, _, _, _) if enumeration.nonEmpty                =>
      s"A string of value ${enumeration.mkString(",")}"
    case StringDefinition(_, Some(format), _, _, _, _, _)                                       =>
      s"A string with format '$format'"
    case StringDefinition(_, _, Some(pattern), _, _, _, _)                                      =>
      s"A string matching regex '$pattern'"
    case StringDefinition(_, _, _, maybeMinLength, maybeMaxLength, None, None)                  =>
      applyLengths("A string", maybeMinLength, maybeMaxLength, false, "length")
    case StringDefinition(_, _, _, maybeMinLength, maybeMaxLength, Some(contentEncoding), None) =>
      applyLengths(s"A string with encoding $contentEncoding", maybeMinLength, maybeMaxLength, true, "length")
    case StringDefinition(_, _, _, maybeMinLength, maybeMaxLength, None, Some(mediaType))       =>
      applyLengths(s"A string with media type $mediaType", maybeMinLength, maybeMaxLength, true, "length")
    case BooleanDefinition                                                                      =>
      "A boolean"
    case IntegerDefinition(enumeration, _, _, _, _, _) if enumeration.nonEmpty                  =>
      s"An integer of value ${enumeration.mkString(",")}"
    case i: IntegerDefinition                                                                   =>
      applyRange(
        "An integer",
        i.minimum.map(_.toString),
        i.exclusiveMinimum.map(_.toString),
        i.maximum.map(_.toString),
        i.exclusiveMinimum.map(_.toString),
        i.multipleOf.map(_.toString)
      )
    case i: NumberDefinition                                                                    =>
      applyRange(
        "A number",
        i.minimum.map(_.toString),
        i.exclusiveMinimum.map(_.toString),
        i.maximum.map(_.toString),
        i.exclusiveMinimum.map(_.toString),
        i.multipleOf.map(_.toString)
      )
    case NullDefinition                                                                         =>
      "A null value"
    case ArrayDefinition(_, maybeMin, maybeMax, false)                                          =>
      applyLengths("An array", maybeMin, maybeMax, false, "length")
    case ArrayDefinition(_, maybeMin, maybeMax, true)                                           =>
      applyLengths("An array with unique elements", maybeMin, maybeMax, true, "length")
    case ObjectDefinition(_, _, _, _, _, _, _, maybeMin, maybeMax, _)                           =>
      applyLengths("An object", maybeMin, maybeMax, false, "properties")
    case MultipleTypeDefinition(types)                                                          =>
      types.map(apply).mkString(" or ")
    case definition                                                                             =>
      s"A value of type ${definition.name}"
  }

  private def applyLengths(
    s: String,
    maybeMinLength: Option[Int],
    maybeMaxLength: Option[Int],
    and: Boolean,
    collective: String
  ): String =
    (maybeMinLength, maybeMaxLength, and) match {
      case (None, None, _)               => s
      case (Some(min), None, false)      =>
        s"$s with minimum $min $collective"
      case (Some(min), None, true)       =>
        s"$s and minimum $min $collective"
      case (None, Some(max), false)      =>
        s"$s with maximum $max $collective"
      case (None, Some(max), true)       =>
        s"$s and maximum $max $collective"
      case (Some(min), Some(max), false) =>
        s"$s with minimum $min and maximum $max $collective"
      case (Some(min), Some(max), true)  =>
        s"$s and minimum $min and maximum $max $collective"
    }

  private def applyRange(
    s: String,
    minimum: Option[String],
    exclusiveMinimum: Option[String],
    maximum: Option[String],
    exclusiveMaximum: Option[String],
    multipleOf: Option[String]
  ): String =
    (minimum, exclusiveMinimum, maximum, exclusiveMaximum, multipleOf) match {
      case (None, None, None, None, Some(multipleOf))          =>
        s"$s as multiple of $multipleOf"
      case (Some(min), _, None, None, None)                    =>
        s"$s greater than or equal to $min"
      case (Some(min), _, None, None, Some(multipleOf))        =>
        s"$s greater than or equal to $min and is a multiple of $multipleOf"
      case (Some(min), _, Some(max), None, None)               =>
        s"$s between $min and $max inclusive"
      case (Some(min), _, Some(max), None, Some(multipleOf))   =>
        s"$s between $min and $max inclusive and is a multiple of $multipleOf"
      case (Some(min), _, None, Some(maxEx), None)             =>
        s"$s between $min inclusive and $maxEx exclusive"
      case (Some(min), _, None, Some(maxEx), Some(multipleOf)) =>
        s"$s between $min inclusive and $maxEx exclusive and is a multiple of $multipleOf"

      case (_, Some(minEx), None, None, None)                    =>
        s"$s greater than $minEx"
      case (_, Some(minEx), None, None, Some(multipleOf))        =>
        s"$s greater than $minEx and multiple of $multipleOf"
      case (_, Some(minEx), Some(max), None, None)               =>
        s"$s between $minEx exclusive and $max inclusive"
      case (_, Some(minEx), Some(max), None, Some(multipleOf))   =>
        s"$s between $minEx exclusive and $max inclusive and is a multiple of $multipleOf"
      case (_, Some(minEx), None, Some(maxEx), None)             =>
        s"$s between $minEx and $maxEx exclusive"
      case (_, Some(minEx), None, Some(maxEx), Some(multipleOf)) =>
        s"$s between $minEx and $maxEx exclusive and is a multiple of $multipleOf"

      case (_, None, Some(max), None, None)             =>
        s"$s less than or equal to $max"
      case (_, None, Some(max), None, Some(multipleOf)) =>
        s"$s less than or equal to $max and is a multiple of $multipleOf"

      case (_, None, None, Some(maxEx), None)             =>
        s"$s less than $maxEx"
      case (_, None, None, Some(maxEx), Some(multipleOf)) =>
        s"$s less than $maxEx and is a multiple of $multipleOf"
      case _                                              => //shouldnt happen
        s
    }
}
