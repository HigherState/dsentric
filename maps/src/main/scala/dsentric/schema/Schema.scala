package dsentric.schema

import dsentric._

object Schema {

  def contractName[D <: DObject](contract:ContractFor[D]):String = {
    SchemaReflection.getDisplayName(contract)
  }

  def contractObjectDefinitionRef[D <: DObject](contract:ContractFor[D], defs:Vector[ObjectDefinition]):(String, Vector[ObjectDefinition]) = {
    val name = contractName(contract)
    defs.find(_.definition.contains(name))
      .map{d => name -> defs}
      .getOrElse{
        val newDefs = contractSeqObjectDefinitions(Seq(contract), defs)
        val d = newDefs.find(_.definition.contains(name)).getOrElse(throw SchemaGenerationException(s"Unable to find definition '$name'"))
        name -> newDefs
      }
  }

  def contractObjectDefinitions(contracts:BaseContract[_]*):Vector[ObjectDefinition] = {
    contractSeqObjectDefinitions(contracts.map(_.asInstanceOf[BaseContract[DObject]]), Vector.empty)
  }


  private def contractSeqObjectDefinitions[D <: DObject](contracts:Seq[BaseContract[D]], defs:Vector[ObjectDefinition]):Vector[ObjectDefinition] = {
    val (_, definitions) =
      contracts.foldLeft(Vector.empty[ContractInfo] -> defs){ case ((infos, newDefs), contract) =>
        val (contractInfo, newInfos) = SchemaReflection.getContractInfo(contract, infos)
        contractInfo.displayName.flatMap(tn => newDefs.find(_.name.contains(tn))) match {
          case Some(r) =>
            newInfos -> newDefs
          case None =>
            val schema = contractInfo.schemaAnnotations
            val (_, i, d) = baseContractObjectDefinition(contract._fields,contractInfo, newInfos, newDefs)
            i -> d
        }
     }
    definitions
  }

  private def baseContractObjectDefinition[D <: DObject](fields:Vector[(String, Property[D, Any])], contractInfo:ContractInfo, infos:Vector[ContractInfo], defs:Vector[ObjectDefinition]):(ObjectDefinition, Vector[ContractInfo], Vector[ObjectDefinition]) = {
    val properties = findPropertyAnnotations(fields, contractInfo, false)
    val (propertyDefs, newInfos, newDefs) = contractPropertyDefinitions(properties, infos, defs)
    val (referencedDefinitions, newInfos2, newDefs2) =
      if (!contractInfo.schemaAnnotations.nested)
        inheritFold(fields, contractInfo.inherits, newInfos, newDefs)
      else
        (Vector.empty, newInfos, newDefs)

    val objDef =
      ObjectDefinition(contractInfo.schemaAnnotations.typeName.orElse(contractInfo.displayName),
        contractInfo.schemaAnnotations.title,
        contractInfo.schemaAnnotations.description,
        referencedDefinitions,
        propertyDefs
      )
    (objDef, newInfos2, newDefs2 :+ objDef)
  }

  private def contractPropertyDefinitions[D <: DObject](properties:Seq[(String, Property[D, Any], SchemaAnnotations)], infos:Vector[ContractInfo], defs:Vector[ObjectDefinition]):(Vector[PropertyDefinition], Vector[ContractInfo], Vector[ObjectDefinition]) = {
    properties
      .foldLeft((Vector.empty[PropertyDefinition], infos, defs)) {
        //Nested, display all properties
        case ((p, i, d), (name, b:BaseContract[D]@unchecked with Property[D, _], schema)) if schema.nested =>
          val (bInfo, newInfos) = SchemaReflection.getContractInfo(b, i)
          val subProperties = findPropertyAnnotations(b._fields, bInfo, true)
          val (subPropertyDefs, newInfos2, newDefs) = contractPropertyDefinitions(subProperties, newInfos, d)
          val (typeDefinition, newDefs2) =
            b._pathValidator(
              ObjectDefinition(
                schema.typeName,
                schema.title,
                schema.description,
                Vector.empty,
                subPropertyDefs
              ), newDefs)
          val property = PropertyDefinition(name, typeDefinition, schema.examples, getDefault(b), isRequired(b), schema.description)
          (p :+ property, newInfos2, newDefs2)

        case ((p, i, d), (name, b:BaseContract[D]@unchecked with Property[D ,_], schema)) =>
          val (bInfo, newInfos) = SchemaReflection.getContractInfo(b, i)
          //Internal object is a single type inheritance
          if (bInfo.inherits.size == 1 && bInfo.fields.isEmpty && b._pathValidator.isEmpty) {
            val (ref, newInfos2, newDefs) = baseContractObjectDefinition(b._fields, bInfo.inherits.head, newInfos, d)
            val refName = ref.definition.getOrElse(throw SchemaGenerationException("Referenced trait cannot be unnamed")) //should never happen
            val property = PropertyDefinition(name, ByRefDefinition(refName), schema.examples, getDefault(b), isRequired(b), schema.description)
            (p :+ property, newInfos2, newDefs)
          }
          else {
            val subProperties = findPropertyAnnotations(b._fields, bInfo, false)
            val (subPropertyDefs, newInfos2, newDefs) = contractPropertyDefinitions(subProperties, newInfos, d)
            val (inheritedRef, newInfos3, newDefs3) = inheritFold(b._fields, bInfo.inherits, newInfos2, newDefs)
            val (objectDefinition, newDefs4) =
              b._pathValidator(
                ObjectDefinition(
                  None,
                  bInfo.schemaAnnotations.title,
                  bInfo.schemaAnnotations.description,
                  inheritedRef,
                  subPropertyDefs
                ), newDefs3
              )
            val property = PropertyDefinition(name, objectDefinition, schema.examples, getDefault(b), isRequired(b), schema.description)
            (p :+ property, newInfos3, newDefs4)
          }

        case ((p, i, d), (name, prop, schema)) =>
          val (typeDef, newDefs) = prop._pathValidator(prop._codec.typeDefinition, d)
          val property = PropertyDefinition(name, typeDef, schema.examples, getDefault(prop), isRequired(prop), schema.description)
          (p :+ property, i, newDefs)
    }
  }

  private def getDefault[D <: DObject](p:Property[D, _]): Option[Any] =
    p match {
      case d:Default[_, Any]@unchecked =>
        Some(d._codec(d._default).value)
      case _ => None
    }

  private def isRequired[D <: DObject](p:Property[D, _]): Boolean =
    p.isInstanceOf[Expected[D, _]]

  private def inheritFold[D <: DObject](fields:Vector[(String, Property[D, Any])], inherits:Vector[ContractInfo], infos:Vector[ContractInfo], defs:Vector[ObjectDefinition]): (Vector[String], Vector[ContractInfo], Vector[ObjectDefinition]) =
    inherits.foldLeft((Vector.empty[String], infos, defs)){
      case ((r, i, d), ci) =>
        val n = ci.schemaAnnotations.typeName.orElse(ci.displayName).getOrElse(throw SchemaGenerationException("Inherited contract cannot be anonymous"))
        defs.find(o => o.definition.contains(n)) match {
          case Some(o) =>
            (r :+ n, i, d)
          case None =>
            val (o, ni, nd) = baseContractObjectDefinition(fields, ci, i, d)
            (r :+ n, ni, nd)
        }
    }

  private def findPropertyAnnotations[D <: DObject](fields:Vector[(String, Property[D, Any])], info:ContractInfo, nestedOverride:Boolean): Seq[(String, Property[D, Any], SchemaAnnotations)] =
    fields.flatMap { field =>
      val schema =
        if (nestedOverride || info.schemaAnnotations.nested)
          info.getInheritedFieldAnnotation(field._1)
        else
          info.fields.get(field._1)
      schema.map(s => (field._1, field._2, s))
    }

}
