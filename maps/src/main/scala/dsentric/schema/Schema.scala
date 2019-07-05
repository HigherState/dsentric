package dsentric.schema

import dsentric.{BaseContract, DObject, Default, Expected, Property}

object Schema {

  def contractObjectDefinitions[D <: DObject](contract:BaseContract[D], current:Vector[ObjectDefinition]):Vector[ObjectDefinition] = {
    val schema = SchemaAnnotations.getContractSchemaAnnotations(contract)
    schema.typeName.flatMap(tn => current.find(_.name.contains(tn))) match {
      case Some(r) =>
        current
      case None =>
        val (properties, newObjects) = contractPropertyDefinitions(contract, current)
        val objectDefinition = ObjectDefinition(schema.typeName, schema.examples.map(_.toString).headOption, schema.description, Vector.empty, properties)
        newObjects :+ objectDefinition
    }
  }

  private def contractPropertyDefinitions[D <: DObject](contract:BaseContract[D], current:Vector[ObjectDefinition]):(Vector[PropertyDefinition], Vector[ObjectDefinition]) = {
    val annotatedChildren = SchemaAnnotations.getFieldsSchemaAnnotations(contract)

    contract._fields.foldLeft(Vector.empty[PropertyDefinition] -> current) {
      case ((properties, objects), (name, b:BaseContract[D]@unchecked with Property[D,_]@unchecked)) =>
        val schema = annotatedChildren.getOrElse(name, SchemaAnnotations.empty)
        val default = b match {
          case d:Default[D, D]@unchecked => Some(d._default.value)
          case _ => None
        }
        val required = b.isInstanceOf[Expected[_, _]]

        //TODO: multiinheritance and internal properties
        if (schema.nested || schema.typeName.isEmpty) {
          val (subProperties, newObjects) = contractPropertyDefinitions(b, objects)
          val typeDefinition =
            ObjectDefinition(
              schema.typeName,
              schema.title,
              schema.description,
              Vector.empty,
              subProperties
            )
          val property = PropertyDefinition(name, typeDefinition, schema.examples, default, required, schema.description)
          (properties :+ property) -> newObjects
        }
        else {
          val typeName = schema.typeName.get
          val newObjects = contractObjectDefinitions(contract, objects)
          val property = PropertyDefinition(name, ByRefDefinition(typeName), schema.examples, default, required, schema.description)
          (properties :+ property) -> newObjects
        }

      case ((fields, records), (name, p)) =>
        val default = p match {
          case d:Default[D, D]@unchecked => Some(d._default.value)
          case _ => None
        }
        val required = p.isInstanceOf[Expected[_, _]]

        val schema = annotatedChildren.getOrElse(name, SchemaAnnotations.empty)
        val field = PropertyDefinition(name, p._codec.typeDefinition, schema.examples, default, required, schema.description)
        (fields :+ field) -> records
    }
  }

}
