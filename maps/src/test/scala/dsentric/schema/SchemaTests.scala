package dsentric.schema

import dsentric.Dsentric._
import dsentric.PessimisticCodecs._
import dsentric._
import org.scalatest.{FunSuite, Matchers}


trait AnotherNested extends SubContract {
  val prop = \[Boolean]
}

object Query1 extends Contract {
  @Schema(example = "FIELD")
  val field = \?[String]
  @Schema(fieldName="field_2")
  val field2 = \[Long]
  @Schema(typeName = "FLOATIES")
  val fieldX = \[Float]("actualName")

  @Schema(typeName = "Anonymous")
  val nested = new \\? {
    val field2 = \?[String]
  }
}

class SchemaTests extends FunSuite with Matchers {
  test("Schema generation") {

    println(Schema.getAnnotationOverrides(Query1))

  }
}
