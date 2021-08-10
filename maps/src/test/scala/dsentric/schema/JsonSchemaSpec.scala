package dsentric.schema

import dsentric.Dsentric._
import dsentric._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import dsentric.codecs.std.DCodecs._


class JsonSchemaSpec extends AnyFunSpec with Matchers {
  // it("empty schema has required fields") {
  //   val jsonSchema = JsonSchema.convertObjectDefinition(ObjectDefinition.empty).asObject.get
  //   (jsonSchema \ [Boolean](Path("additionalProperties"))) should contain (true)
  //   (jsonSchema \ [String](Path("type"))) should contain ("object")
  //   (jsonSchema \ [String](Path("$schema"))) should contain ("https://json-schema.org/draft/2020-12/schema")
  // }

  it("can generate a json schema for a simple contract") {
    val contractObjectDefinition = Definition.nestedContractObjectDefinition(SimpleContract)
    val jsonSchema = JsonSchema.convertObjectDefinition(contractObjectDefinition).asObject.get
    (jsonSchema \ [Boolean](Path("additionalProperties"))) should contain (true)
    (jsonSchema \ [String](Path("type"))) should contain ("object")
    (jsonSchema \ [String](Path("$schema"))) should contain ("https://json-schema.org/draft/2020-12/schema")
    (jsonSchema \ [Long](Path("properties", "longField", "maximum"))) should contain (Long.MaxValue)
    (jsonSchema \ [Long](Path("properties", "longField", "minimum"))) should contain (Long.MinValue)
    (jsonSchema \ [String](Path("properties", "longField", "type"))) should contain ("integer")
    (jsonSchema \ [String](Path("properties", "optionalStringField", "type"))) should contain ("string")
    (jsonSchema \ [DValue](Path("required"))).map(_.value) should contain (Set("longField", "actualName"))
    (jsonSchema \ [Float](Path("properties", "actualName", "maximum"))) should contain (Float.MaxValue)
    (jsonSchema \ [Float](Path("properties", "actualName", "minimum"))) should contain (Float.MinValue)
    (jsonSchema \ [String](Path("properties", "actualName", "type"))) should contain ("number")

  }

  private object SimpleContract extends Contract {
    val optionalStringField  = \?[String]
    val longField = \[Long]
    val floatFieldWithName = \[Float]("actualName")
  }
}
