package dsentric.schema

import dsentric.*
import dsentric.codecs.DStringCodec
import dsentric.contracts.*
import dsentric.operators.Internal
import dsentric.meta.TypeTag

object Definition {
  type Definitions = Vector[ObjectDefinition]
  type Infos       = Vector[ContractInfo]

  def contractName[D <: DObject](contract: ContractFor[D]): String =
    SchemaReflection.getDisplayName(contract)

  def contractObjectDefinitionRef[D <: DObject, C <: ContractFor[D]](
    contract: C,
    infos: Infos,
    defs: Definitions,
    forceNested: Boolean = false
  )(using typeTag: TypeTag[C]): (String, Infos, Definitions) = {
    val name = contractName(contract)
    defs
      .find(_.definition.contains(name))
      .map(_ => (name, infos, defs))
      .getOrElse {
        val (contractInfo, infos1) = SchemaReflection.getContractInfo(contract, infos)(using typeTag)
        val (c, infos2, d)         =
          baseContractObjectDefinition(contract._fields, contract, contractInfo, infos1, Vector.empty, forceNested)
        (name, infos2, d :+ c)
      }
  }

  def nestedContractObjectDefinition[D <: DObject, C <: BaseContract[D]](contract: C)(using typeTag: TypeTag[C]): ObjectDefinition = {
    val (contractInfo, infos) = SchemaReflection.getContractInfo(contract, Vector.empty)(using typeTag)
    val (c, _, _)             = baseContractObjectDefinition(contract._fields, contract, contractInfo, infos, Vector.empty, true)
    c
  }

  def contractObjectDefinitions[D <: DObject, C <: BaseContract[D]](contract: C)(using typeTag: TypeTag[C]): (ObjectDefinition, Definitions) = {
    val (contractInfo, newInfos) = SchemaReflection.getContractInfo(contract, Vector.empty)(using typeTag)
    val (c, _, d)                = baseContractObjectDefinition(contract._fields, contract, contractInfo, newInfos, Vector.empty, false)
    c -> d
  }

  private def baseContractObjectDefinition[D <: DObject](
    fields: Map[String, Property[D, ?]],
    contract: BaseContract[D],
    contractInfo: ContractInfo,
    infos: Infos,
    defs: Definitions,
    forceNested: Boolean
  ): (ObjectDefinition, Infos, Definitions) = {
    val properties                                   = findPropertyAnnotations(fields, contractInfo, forceNested)
    val (propertyDefs, newInfos, newDefs)            = contractPropertyDefinitions(properties, infos, defs, forceNested)
    val (referencedDefinitions, newInfos2, newDefs2) =
      if (!contractInfo.schemaAnnotations.nested && !forceNested)
        inheritFold(fields, contract, contractInfo.inherits, newInfos, newDefs, forceNested)
      else
        (Vector.empty, newInfos, newDefs)

    val objDef =
      ObjectDefinition(
        contractInfo.schemaAnnotations.typeName.orElse(contractInfo.displayName),
        contractInfo.schemaAnnotations.title,
        contractInfo.schemaAnnotations.description,
        referencedDefinitions,
        propertyDefs
      )
    (objDef, newInfos2, newDefs2)
  }

  private def contractPropertyDefinitions[D <: DObject](
    properties: Iterable[(String, Property[D, ?], SchemaAnnotations)],
    infos: Infos,
    defs: Definitions,
    forceNested: Boolean
  ): (Set[PropertyDefinition], Infos, Definitions) =
    properties
      .foldLeft((Set.empty[PropertyDefinition], infos, defs)) {
        case (a, (_, prop, _)) if skipProperty(prop) =>
          a
        //Nested, display all properties
        case ((p, infos0, defs0), (name, b: (BaseContract[D] & Property[D, ?])@unchecked, schema))
            if schema.nested || forceNested =>
          val (objectDefinition, infos1, defs1) = resolveNestedContract(b, infos0, defs0, forceNested)
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

        case ((p, infos0, defs0), (name, b: (BaseContract[D]& Property[D, ?])@unchecked, schema)) =>
          val (bInfo, infos1) = SchemaReflection.getContractInfo(b, infos0)
          //Internal object is a single type inheritance
          if (bInfo.inherits.size == 1 && bInfo.fields.isEmpty && b._dataOperators.isEmpty) {
            val (ref, infos2, defs1) =
              baseContractObjectDefinition(b._fields, b, bInfo.inherits.head, infos1, defs0, false)
            val refName              = ref.definition.getOrElse(
              throw SchemaGenerationException("Referenced trait cannot be unnamed")
            ) //should never happen
            val property = PropertyDefinition(
              name,
              ByRefDefinition(refName),
              schema.examples,
              getDefault(b),
              isRequired(b),
              schema.description
            )
            (p + property, infos2, defs1 :+ ref)
          } else {
            val subProperties                         = findPropertyAnnotations(b._fields, bInfo, false)
            val (subPropertyDefs, infos2, defs1)      = contractPropertyDefinitions(subProperties, infos1, defs0, false)
            val (inheritedRef, infos3, defs2)         = inheritFold(b._fields, b, bInfo.inherits, infos2, defs1, false)
            //dunno how to handle inheritance
            val (addtional, propNames, infos4, defs3) = additionalPropertiesDefinition(b, infos3, defs2)
            val objectDefinition                      =
              ObjectDefinition(
                None,
                bInfo.schemaAnnotations.title,
                bInfo.schemaAnnotations.description,
                inheritedRef,
                subPropertyDefs,
                addtional,
                propNames
              )
            val resolvedDefinition                    = resolveDataOperators(b, objectDefinition)

            val property = PropertyDefinition(
              name,
              resolvedDefinition,
              schema.examples,
              getDefault(b),
              isRequired(b),
              schema.description
            )
            (p + property, infos4, defs3)
          }

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
    defs: Definitions,
    forceNested: Boolean
  ): (ObjectDefinition, Infos, Definitions) = {
    val (bInfo, infos1)                        = SchemaReflection.getContractInfo(contract, infos)(using TypeTag.ofClass(contract.getClass.asInstanceOf[Class[BaseContract[D]]]))
    val subProperties                          = findPropertyAnnotations(contract._fields, bInfo, true)
    val (subPropertyDefs, infos2, defs1)       = contractPropertyDefinitions(subProperties, infos1, defs, forceNested)
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
      case d: DefaultProperty[?, Any] @unchecked =>
        Some(d._codec(d._default))
      case _                                     => None
    }

  private def skipProperty[D <: DObject](p: Property[D, ?]): Boolean =
    p._dataOperators.contains(Internal)

  private def isRequired[D <: DObject](p: Property[D, ?]): Boolean =
    p.isInstanceOf[ExpectedProperty[D, ?]] || p.isInstanceOf[ExpectedObjectProperty[D]] || p.isInstanceOf[MaybeExpectedProperty[D, ?]]

  private def inheritFold[D <: DObject](
    fields: Map[String, Property[D, ?]],
    contract: BaseContract[D],
    inherits: Vector[ContractInfo],
    infos: Vector[ContractInfo],
    defs: Vector[ObjectDefinition],
    forceNested: Boolean
  ): (Vector[String], Vector[ContractInfo], Vector[ObjectDefinition]) =
    inherits.foldLeft((Vector.empty[String], infos, defs)) { case ((r, infos0, defs0), ci) =>
      val n = ci.schemaAnnotations.typeName
        .orElse(ci.displayName)
        .getOrElse(throw SchemaGenerationException("Inherited contract cannot be anonymous"))
      defs0.find(o => o.definition.contains(n)) match {
        case Some(_) =>
          (r :+ n, infos0, defs0)
        case None    =>
          val (o, infos1, defs1) = baseContractObjectDefinition(fields, contract, ci, infos0, defs0, forceNested)
          (r :+ n, infos1, defs1 :+ o)
      }
    }

  private def findPropertyAnnotations[D <: DObject](
    fields: Map[String, Property[D, ?]],
    info: ContractInfo,
    nestedOverride: Boolean
  ): Iterable[(String, Property[D, ?], SchemaAnnotations)] =
    fields.flatMap { case (key, value) =>
      val schema =
        if (nestedOverride || info.schemaAnnotations.nested)
          info.getInheritedFieldAnnotation(key)
        else
          info.fields.get(key)
      schema.map((key, value, _))
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
//      case a:AdditionalPropertyObjects[Any, DObject]@unchecked =>
//        if (forceNested) {
//          val (objectDefinition, infos1, defs1) = resolveNestedContract(a._additionalContract, infos, defs, forceNested, None)
//          (Right(objectDefinition), getPattern(a._additionalKeyCodec), infos1, defs1)
//        }
//        else {
//          val (name, infos1, defs1) = contractObjectDefinitionRef(a._additionalContract, infos, defs, false)
//          (Right(ByRefDefinition(name)), getPattern(a._additionalKeyCodec), infos, defs1)
//        }
      case _                                            =>
        (Left(false), None, infos, defs)
    }
  }

}
