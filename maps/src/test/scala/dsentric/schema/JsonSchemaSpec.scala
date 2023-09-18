package dsentric.schema

import dsentric.Dsentric._
import dsentric.contracts._
import dsentric._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import dsentric.codecs.std.DCodecs._

class JsonSchemaSpec extends AnyFunSpec with Matchers {

  ignore("can convert properties to json schema types") {
    it("handles optional object property") {
      val requiredProperty = PropertyDefinition(
        key = "required",
        typeDefinition = NullDefinition,
        examples = List.empty,
        default = None,
        required = true,
        description = None
      )
      val optionalProperty = PropertyDefinition(
        key = "optional",
        typeDefinition = NullDefinition,
        examples = List.empty,
        default = None,
        required = false,
        description = None
      )

      val objectDefinition         =
        ObjectDefinition(properties = Set(requiredProperty, optionalProperty), additionalProperties = Left(false))
      val nestedPropertyDefinition = PropertyDefinition(
        key = "nested",
        typeDefinition = objectDefinition,
        examples = List.empty,
        default = None,
        required = false,
        description = None
      )
      val rootDefinition           =
        ObjectDefinition(properties = Set(nestedPropertyDefinition), additionalProperties = Left(false))
      val propertyDefinition       = PropertyDefinition(
        key = "example",
        typeDefinition = rootDefinition,
        examples = List.empty,
        default = None,
        required = false,
        description = None
      )
      println(JsonSchema.convertPropertyDefinition(propertyDefinition))
    }
  }

  ignore("contracts can generate json schema from a contract") {
    it("handles a empty contract with schema, additionalProperties, and, type present") {
      val jsonSchema = JsonSchema.convertObjectDefinition(ObjectDefinition.empty).asObject.get
      validateRootObject(jsonSchema)
    }
    it("handles a simple contract with optional, required, and, renamed fields") {
      val contractObjectDefinition = Definition.nestedContractObjectDefinition(SimpleContract)
      val jsonSchema               = JsonSchema.convertObjectDefinition(contractObjectDefinition).asObject.get
      SimpleContract.validate(jsonSchema)
      validateRootObject(jsonSchema)
    }
    it("handles a complex contract with inheritence, nested, and, operators") {
      val contractObjectDefinition = Definition.nestedContractObjectDefinition(ComplexContract)
      val jsonSchema               = JsonSchema.convertObjectDefinition(contractObjectDefinition).asObject.get
      ComplexContract.validate(jsonSchema)
      validateRootObject(jsonSchema)
    }
  }

  private def validateRootObject(schema: DObject): Unit = {
    (schema \ [Boolean] (Path("additionalProperties"))) should contain(true)
    (schema \ [String] (Path("type"))) should contain("object")
    (schema \ [String] (Path(s"$$schema"))) should contain("https://json-schema.org/draft/2020-12/schema")
    ()
  }

  private def validateObject(schema: DObject) = {
    (schema \ [Boolean] (Path("additionalProperties"))) should contain(false)
    (schema \ [String] (Path("type"))) should contain("object")
  }

  private def validateLong(longObj: DObject) = {
    longObj \ [Long] (Path("maximum")) should contain(Long.MaxValue)
    longObj \ [Long] (Path("minimum")) should contain(Long.MinValue)
    longObj \ [String] (Path("type")) should contain("integer")
  }

  private def validateInt(longObj: DObject) = {
    longObj \ [Long] (Path("maximum")) should contain(Int.MaxValue)
    longObj \ [Long] (Path("minimum")) should contain(Int.MinValue)
    longObj \ [String] (Path("type")) should contain("integer")
  }

  private def validateFloat(longObj: DObject) = {
    longObj \ [Float] (Path("maximum")) should contain(Float.MaxValue)
    longObj \ [Float] (Path("minimum")) should contain(Float.MinValue)
    longObj \ [String] (Path("type")) should contain("number")
  }

  private def validateString(longObj: DObject) =
    longObj \ [String] (Path("type")) should contain("string")

  private def findDObject(schema: DObject)(path: Path): DObject =
    schema.get(path).flatMap(_.asObject).getOrElse(DObject.empty)

  private def validateRequiredValues(schema: DObject)(required: String*) =
    (schema \ [DValue](Path("required"))).map(_.value) should contain(Set(required: _*))

  private object SimpleContract extends Contract {
    val optionalStringField = \?[String]
    val longField           = \[Long]
    val floatFieldWithName  = \[Float]("actualName")

    def validate(schema: DObject) = {
      val find = findDObject(schema)(_)
      validateLong(find("properties" \ "longField"))
      validateFloat(find("properties" \ "actualName"))
      validateString(find("properties" \ "optionalStringField"))
      validateRequiredValues(schema)("longField", "actualName")
    }
  }

  private object ComplexContract extends Contract {
    import dsentric.operators.StandardOperators._
    val expectedObject = new \\?(immutable) {
      val requiredProperty = \[Int]
      val optionalProperty = \?[String]
    }

    val extraField = \[Long]

    def validate(schema: DObject) = {
      val find = findDObject(schema)(_)
      validateLong(find("properties" \ "extraField"))
      validateObject(find("properties" \ "expectedObject"))
      validateInt(find("properties" \ "expectedObject" \ "properties" \ "requiredProperty"))
      validateString(find("properties" \ "expectedObject" \ "properties" \ "optionalProperty"))
      validateRequiredValues(schema)("extraField")
      validateRequiredValues(find("properties" \ "expectedObject"))("requiredProperty")
    }
  }
}
