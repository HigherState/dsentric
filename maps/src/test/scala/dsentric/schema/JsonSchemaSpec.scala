package dsentric.schema

import dsentric.Dsentric._
import dsentric._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import dsentric.codecs.std.DCodecs._

class JsonSchemaSpec extends AnyFunSpec with Matchers {
  it("empty schema has required fields") {
    val jsonSchema = JsonSchema.convertObjectDefinition(ObjectDefinition.empty).asObject.get
    validateSchema(jsonSchema)
  }

  it("can generate a json schema for a simple contract") {
    val contractObjectDefinition = Definition.nestedContractObjectDefinition(SimpleContract)
    val jsonSchema               = JsonSchema.convertObjectDefinition(contractObjectDefinition).asObject.get
    SimpleContract.validate(jsonSchema)
    validateSchema(jsonSchema)
  }

  it("can generate a json schema for a complex contract") {
    val contractObjectDefinition = Definition.nestedContractObjectDefinition(ComplexContract)
    val jsonSchema               = JsonSchema.convertObjectDefinition(contractObjectDefinition).asObject.get
    ComplexContract.validate(jsonSchema)
    validateSchema(jsonSchema)
  }

  private def validateSchema(schema: DObject) = {
    (schema \ [Boolean] (Path("additionalProperties"))) should contain(true)
    (schema \ [String] (Path("type"))) should contain("object")
    (schema \ [String] (Path(s"$$schema"))) should contain("https://json-schema.org/draft/2020-12/schema")
  }

  private def validateLong(longObj: DObject) = {
      longObj \ [Long] (Path("maximum")) should contain(Long.MaxValue)
      longObj \ [Long] (Path("minimum")) should contain(Long.MinValue)
      longObj \ [String] (Path("type")) should contain("integer")
  }

  private def validateFloat(longObj: DObject) = {
      longObj \ [Float] (Path("maximum")) should contain(Float.MaxValue)
      longObj \ [Float] (Path("minimum")) should contain(Float.MinValue)
      longObj \ [String] (Path("type")) should contain("number")
  }

  private def validateString(longObj: DObject) = {
      longObj \ [String] (Path("type")) should contain("string")
  }

  private def validateRequiredValues(schema: DObject)(required: String*) = {
      (schema \ [DValue](Path("required"))).map(_.value) should contain(Set(required:_*))
  }

  private def findDObject(schema: DObject)(path: Path): DObject = 
    schema.get(path).flatMap(_.asObject).getOrElse(DObject.empty)

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

  trait InheritenceContract extends Contract {
    val nestedSimpleContract = \[DObject](SimpleContract)
    val optionalStringField  = \?[String]
    val longField            = \[Long]
    val floatFieldWithName   = \[Float]("actualName")
  }

  private object ComplexContract extends InheritenceContract {
    import dsentric.operators.StandardOperators._
    val expectedObject = new \\?(immutable) {
      val property1 = \[Int]
      val property2 = \?[String]
    }

    val extraField  = \[Long]

    def validate(schema:  DObject) = {
      val find = findDObject(schema)(_)
      println(schema.mkString("\n"))
      validateLong(find("properties" \ "extraField"))
      validateRequiredValues(schema)("extraField")
    }
  }
}
