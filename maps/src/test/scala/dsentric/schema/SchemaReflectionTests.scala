package dsentric.schema

import dsentric.Dsentric.*
import namespaced.{AnotherNested, AnotherNested2, InheritedNested}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import dsentric.codecs.std.DCodecs.*

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

  val nested = new \\? {

    @Description("nested field")
    val field2 = \?[String]
  }
  @Description("This is inherited")
  val inherited = new \\ with AnotherNested

  @Type("Overwritten")
  val overwritten = new \\ with AnotherNested

  @Nested
  val renamed = new \\ with NestedNamed

  @Type("OverwrittenRenamed")
  val overwrittenRenamed = new \\ with AnotherNested

  val newOne = new \\ {
    val inside = \[Int]
  }
}

object Query2 extends Contract with AnotherNested with AnotherNested2

object Query3 extends Contract with InheritedNested {
  @Type("MyString")
  val field = \[String]
}

class SchemaReflectionTests extends AnyFunSuite with Matchers {
  test("field annotations") {
    val (contractInfo, _) = SchemaReflection.getContractInfo(Query1)
    contractInfo.fields("field") shouldBe SchemaAnnotations(None, None, false, List(123, "Pants"), None)
    contractInfo.fields("actualName") shouldBe SchemaAnnotations(Some("FLOATIES"), None, false, Nil, Some("Its a float"))
    contractInfo.fields("nested") shouldBe SchemaAnnotations(None, None, false, Nil, None)
    contractInfo.fields("renamed") shouldBe SchemaAnnotations(None, None, true, Nil, None)

    val (renamedInfo, _) = SchemaReflection.getContractInfo(Query1.renamed)
    renamedInfo.schemaAnnotations shouldBe SchemaAnnotations(None, None, false, Nil, None)

  }
  test("Inherited annotation") {
    val (query2, _) = SchemaReflection.getContractInfo(Query2)
    query2.inherits.find(_.displayName.contains("AnotherNested")).get.fields("prop") shouldBe SchemaAnnotations(
      None,
      None,
      false,
      Nil,
      Some("Another nested prop")
    )
  }

  //Not supported by reflection
  // test("Internal nested annotations") {
  //   val (nestedInfo, _) = SchemaReflection.getContractInfo(Query1.nested)
  //   nestedInfo.fields("field2") shouldBe SchemaAnnotations(None, None, false, Nil, Some("nested field"))
  // }

  test("internal inherited annotations") {
    val (inherited, _) = SchemaReflection.getContractInfo(Query1.inherited)
    inherited.fields.get("prop") shouldBe None
    inherited.inherits.find(_.displayName.contains("AnotherNested")).get.fields("prop") shouldBe SchemaAnnotations(
      None,
      None,
      false,
      Nil,
      Some("Another nested prop")
    )
  }
  test("Contract Info") {
    val (query1, _) = SchemaReflection.getContractInfo(Query1)
    query1.displayName shouldBe Some("Query1")
    query1.schemaAnnotations shouldBe SchemaAnnotations(None, None, false, Nil, Some("Contract Description"))
  }
  test("Inherited Name") {
    val (query1, _) = SchemaReflection.getContractInfo(Query1)
    val schema      = query1.fields
    val s           = schema("inherited")
    val o           = schema("overwritten")
    val n           = schema("renamed")
    val on          = schema("overwrittenRenamed")
    s.description shouldBe Some("This is inherited")
    s.typeName shouldBe None
    o.typeName shouldBe Some("Overwritten")
    n.typeName shouldBe None
    on.typeName shouldBe Some("OverwrittenRenamed")

    val (renamed, _) = SchemaReflection.getContractInfo(Query1.renamed)
    renamed.schemaAnnotations.typeName shouldBe None
  }

  test("Properties across subContracts") {
    val (schema, _)   = SchemaReflection.getContractInfo(Query2)
    schema.fields.get("prop") shouldBe None
    schema.fields.get("prop2") shouldBe None
    val anotherNested = schema.inherits.find(_.displayName.contains("AnotherNested")).get

    anotherNested.fields("prop") shouldBe SchemaAnnotations(None, None, false, Nil, Some("Another nested prop"))

    val anotherNested2 = schema.inherits.find(_.displayName.contains("AnotherNested2")).get
    anotherNested2.fields("prop2") shouldBe SchemaAnnotations(None, None, false, Nil, Some("Another nested 2 prop 2"))

    val (schema2, _) = SchemaReflection.getContractInfo(Query3)
    schema2.fields.get("prop") shouldBe None
    schema2.fields.get("propHigher") shouldBe None
    schema2.fields.get("field") shouldBe Some(SchemaAnnotations(Some("MyString"), None, false, Nil, None))

    val inherited = schema2.inherits.find(_.displayName.contains("InheritedNested")).get
    inherited.fields.get("propHigher") shouldBe Some(SchemaAnnotations.empty)
    inherited.fields.get("prop") shouldBe None

    schema2.inherits.find(_.displayName.contains("AnotherNested")) shouldBe None

    val anotherNestedI = inherited.inherits.find(_.displayName.contains("AnotherNested")).get
    anotherNestedI.fields.get("prop") shouldBe Some(SchemaAnnotations(None, None, false, Nil, Some("Another nested prop")))
  }
}
