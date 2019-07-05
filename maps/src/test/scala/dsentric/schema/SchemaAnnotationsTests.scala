package dsentric.schema

import dsentric.Dsentric._
import dsentric.PessimisticCodecs._
import dsentric._
import namespaced.{AnotherNested, AnotherNested2}
import org.scalatest.{FunSuite, Matchers}

@Type("Renamed")
trait NestedNamed extends SubContract {
  val prop = \[Boolean]
}

@Description("Contract Description")
object Query1 extends Contract {
  @Examples(123, "Pants")
  val field = \?[String]

  val field2 = \[Long]
  @Type("FLOATIES")
  @Description("Its a float")
  val fieldX = \[Float]("actualName")

  @Nested
  val nested = new \\? {
    val field2 = \?[String]
  }
  @Description("This is inherited")
  val inherited = new \\? with AnotherNested

  @Type("Overwritten")
  val overwritten = new \\? with AnotherNested

  val renamed = new \\? with NestedNamed

  @Type("OverwrittenRenamed")
  val overwrittenRenamed = new \\? with AnotherNested

  val newOne = new \\ {
    val inside = \[Int]
  }

}

object Query2 extends Contract {
  val merged = new \\ with AnotherNested with AnotherNested2 {
    val additional = \?[String]
  }
}

class SchemaAnnotationsTests extends FunSuite with Matchers {
  test("field annotations") {
    val schema = SchemaAnnotations.getFieldsSchemaAnnotations(Query1)
    schema("field") shouldBe SchemaAnnotations(None, false, List(123, "Pants"), None)
    schema("fieldX") shouldBe SchemaAnnotations(Some("FLOATIES"), false, Nil, Some("Its a float"))
    schema("nested") shouldBe SchemaAnnotations(None, true, Nil, None)
  }
  test("Contract Name") {
    SchemaAnnotations.getContractSchemaAnnotations(Query1) shouldBe SchemaAnnotations(Some("Query1"), false, Nil, Some("Contract Description"))
  }
  test("Inherited Name") {
    val schema = SchemaAnnotations.getFieldsSchemas(Query1)
    val s = schema("inherited")
    val o = schema("overwritten")
    val n = schema("renamed")
    val on = schema("overwrittenRenamed")
    s.description shouldBe Some("This is inherited")
    s.typeName shouldBe Some("AnotherNested")
    o.typeName shouldBe Some("Overwritten")
    n.typeName shouldBe Some("Renamed")
    on.typeName shouldBe Some("OverwrittenRenamed")
  }
  test("Anonymous object") {
    val schema = SchemaAnnotations.getFieldsSchemas(Query1)
    val n = schema("newOne")
    n.typeName shouldBe None
  }

  test("Properties across subContracts") {
    val schema = SchemaAnnotations.getFieldsSchemas(Query2)
  }
}
