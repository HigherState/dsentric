package dsentric.schema

import dsentric.*
import dsentric.codecs.DStringCodec
import dsentric.contracts.*
import dsentric.operators.Internal

import scala.reflect.ClassTag

object Definition {
  type Definitions = Vector[ObjectDefinition]
  type Infos       = Vector[ContractInfo]

  def nestedContractObjectDefinition[D <: DObject, C <: BaseContract[D]](contract: C)(using
    typeContract: TypeContract[C]
  ): ObjectDefinition =
    baseContractObjectDefinition(contract._fields, typeContract.contractInfo, typeContract.infos, Vector.empty)

  def nestedContractObjectDefinitionRuntime[D <: DObject, C <: BaseContract[D]](contract: C)(implicit C: ClassTag[C]): ObjectDefinition =
    val (contractInfo, infos) = SchemaReflection.getContractInfoNested(contract)
    baseContractObjectDefinition(contract._fields, contractInfo, infos, Vector.empty)

  private def baseContractObjectDefinition[D <: DObject](
    fields: Map[String, Property[D, ?]],
    contractInfo: ContractInfo,
    infos: Infos,
    defs: Definitions
  ): ObjectDefinition = {
    val properties           = findPropertyAnnotations(fields, contractInfo)
    val (propertyDefs, _, _) = contractPropertyDefinitions(properties, infos, defs)

    ObjectDefinition(
      contractInfo.schemaAnnotations.typeName.orElse(contractInfo.displayName),
      contractInfo.schemaAnnotations.title,
      contractInfo.schemaAnnotations.description,
      Vector.empty,
      propertyDefs
    )
  }

  private def contractPropertyDefinitions[D <: DObject](
    properties: Iterable[(String, Property[D, ?], SchemaAnnotations)],
    infos: Infos,
    defs: Definitions
  ): (Set[PropertyDefinition], Infos, Definitions) =
    properties
      .foldLeft((Set.empty[PropertyDefinition], infos, defs)) {
        case (a, (_, prop, _)) if skipProperty(prop) =>
          a
        //Nested, display all properties
        case ((p, infos0, defs0), (name, b: (BaseContract[D] & Property[D, ?])@unchecked, schema)) =>
          val (objectDefinition, infos1, defs1) = resolveNestedContract(b, infos0, defs0)
          val resolvedDefinition                = resolveDataOperators(b, objectDefinition)
          val property                          = PropertyDefinition(
            name,
            resolvedDefinition,
            schema.examples,
            getDefault(b),
            isRequired(b),
            schema.description
          )
          (p + property, infos1, defs1)

        case ((p, infos, defs), (name, prop, schema)) =>
          val resolvedDefinition = resolveDataOperators(prop, prop._codec.typeDefinition)
          val property           = PropertyDefinition(
            name,
            resolvedDefinition,
            schema.examples,
            getDefault(prop),
            isRequired(prop),
            schema.description
          )
          (p + property, infos, defs)
      }

  //TODO work out schema override
  private def resolveNestedContract[D <: DObject](
    contract: BaseContract[D],
    infos: Infos,
    defs: Definitions
  ): (ObjectDefinition, Infos, Definitions) = {
    val (bInfo, infos1)                        = SchemaReflection.getContractInfoNested(contract, infos)
    val subProperties                          = findPropertyAnnotations(contract._fields, bInfo)
    val (subPropertyDefs, infos2, defs1)       = contractPropertyDefinitions(subProperties, infos1, defs)
    val (additional, propNames, infos3, defs2) = additionalPropertiesDefinition(contract, infos2, defs1)

    val objectDefinition =
      ObjectDefinition(
        bInfo.schemaAnnotations.typeName,
        bInfo.schemaAnnotations.title,
        bInfo.schemaAnnotations.description,
        Vector.empty,
        subPropertyDefs,
        additional,
        propNames
      )
    (objectDefinition, infos3, defs2)
  }

  private def resolveDataOperators[D <: DObject, T <: TypeDefinition](property: Property[D, ?], typeDef: T): T =
    property._dataOperators.foldLeft(typeDef)((a, d) => d.definition.lift(a).getOrElse(a))

  private def getDefault[D <: DObject](p: Property[D, ?]): Option[Any] =
    p match {
      case d: DefaultProperty[?, Any] @unchecked => Some(d._codec(d._default))
      case _                                     => None
    }

  private def skipProperty[D <: DObject](p: Property[D, ?]): Boolean =
    p._dataOperators.contains(Internal)

  private def isRequired[D <: DObject](p: Property[D, ?]): Boolean =
    p.isInstanceOf[ExpectedProperty[D, ?]] || p.isInstanceOf[ExpectedObjectProperty[D]] || p.isInstanceOf[MaybeExpectedProperty[D, ?]]

  private def findPropertyAnnotations[D <: DObject](
    fields: Map[String, Property[D, ?]],
    info: ContractInfo
  ): Iterable[(String, Property[D, ?], SchemaAnnotations)] =
    fields.flatMap { case (key, value) =>
      info.getInheritedFieldAnnotation(key).map((key, value, _))
    }

  private def additionalPropertiesDefinition[D <: DObject](
    contract: BaseContract[D],
    infos: Infos,
    defs: Definitions
  ): (Either[Boolean, TypeDefinition], Option[StringDefinition], Infos, Definitions) = {
    def getPattern(c: DStringCodec[?]): Option[StringDefinition] = {
      val s = c.typeDefinition
      if (s == StringDefinition.empty) None
      else Some(s)
    }

    contract match {
      case a: AdditionalProperties[Any, Any] @unchecked =>
        (Right(a._additionalValueCodec.typeDefinition), getPattern(a._additionalKeyCodec), infos, defs)
      case _                                            =>
        (Left(false), None, infos, defs)
    }
  }

}
