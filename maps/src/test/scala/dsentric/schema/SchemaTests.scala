package dsentric.schema

import dsentric.contracts._
import namespaced.{AnotherNested, AnotherNested2, InheritedNested, ToRename}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import dsentric.codecs.std.DCodecs._

@Title("Schema1 Contract")
object Schema1 extends Contract with AnotherNested {
  val field = \[String]
}

@Nested
object Schema2 extends Contract with AnotherNested {
  val field = \[String]
}

object Schema3 extends Contract with InheritedNested {
  val field = \![String]("value")
}

object Schema4 extends Contract with ToRename {
  val field = \?[String]
}

@Description("directly nested field")
object Schema5 extends Contract {

  val directRef = new \\ with AnotherNested

}

object Schema6 extends Contract {
  val objectWithRef = new \\ with AnotherNested {
    val field = \[Long]
  }
}

object Schema7 extends Contract {
  val objectWithRefs = new \\ with AnotherNested with AnotherNested2
}

class SchemaTests extends AnyFunSpec with Matchers {
  val anotherNestedDef =
    ObjectDefinition(
      Some("AnotherNested"),
      None,
      None,
      Vector.empty,
      Set(PropertyDefinition("prop", BooleanDefinition, Nil, None, true, Some("Another nested prop")))
    )

  val anotherNested2Def =
    ObjectDefinition(
      Some("AnotherNested2"),
      None,
      None,
      Vector.empty,
      Set(PropertyDefinition("prop2", BooleanDefinition, Nil, None, true, Some("Another nested 2 prop 2")))
    )

  val inheritedDef = ObjectDefinition(
    Some("InheritedNested"),
    None,
    None,
    Vector.empty,
    Set(
      PropertyDefinition("prop", BooleanDefinition, Nil, None, true, Some("Another nested prop")),
      PropertyDefinition("propHigher", StringDefinition(), Nil, None, true, None)
    )
  )

  val toRenameDef = ObjectDefinition(
    Some("Renamed"),
    None,
    None,
    Vector("AnotherNested2", "AnotherNested"),
    Set(PropertyDefinition("propHigher", StringDefinition(), List("one", "two"), None, false, None))
  )

  it("Multiinheritance no nesting") {
    val (schema1, _) = Definition.contractObjectDefinitions(Schema1)
    val schema1Def   = ObjectDefinition(
      Some("Schema1"),
      Some("Schema1 Contract"),
      None,
      Vector("AnotherNested"),
      Set(PropertyDefinition("field", StringDefinition(), Nil, None, true, None))
    )
    schema1 shouldBe schema1Def
  }

  it("Multi-inheritance full nesting") {
    val (schema2, _) = Definition.contractObjectDefinitions(Schema2)
    val schema2Def   = ObjectDefinition(
      Some("Schema2"),
      None,
      None,
      Vector.empty,
      Set(
        PropertyDefinition("field", StringDefinition(), Nil, None, true, None),
        PropertyDefinition("prop", BooleanDefinition, Nil, None, true, Some("Another nested prop"))
      )
    )
    schema2 shouldBe schema2Def
  }

  it("Multi-inheritance partial nesting") {
    val (schema3, Vector(inherited)) = Definition.contractObjectDefinitions(Schema3)
    val schema3Def                   = ObjectDefinition(
      Some("Schema3"),
      None,
      None,
      Vector("InheritedNested"),
      Set(PropertyDefinition("field", StringDefinition(), Nil, Some("value"), false, None))
    )

    schema3 shouldBe schema3Def
    inherited shouldBe inheritedDef
  }

  it("Renamed inherited") {
    val (schema4, Vector(anotherNested2, anotherNested, toRename)) = Definition.contractObjectDefinitions(Schema4)
    val schema4Def                                                 = ObjectDefinition(
      Some("Schema4"),
      None,
      None,
      Vector("Renamed"),
      Set(PropertyDefinition("field", StringDefinition(), Nil, None, false, None))
    )
    anotherNested2 shouldBe anotherNested2Def
    anotherNested shouldBe anotherNestedDef
    toRename shouldBe toRenameDef
    schema4 shouldBe schema4Def
  }

  it("Direct inheritance reference") {
    val (schema5, Vector(anotherNested)) = Definition.contractObjectDefinitions(Schema5)
    val directRef                        = PropertyDefinition("directRef", ByRefDefinition("AnotherNested"), Nil, None, true, None)
    val schema5Def                       =
      ObjectDefinition(Some("Schema5"), None, Some("directly nested field"), Vector.empty, Set(directRef))
    anotherNested shouldBe anotherNestedDef
    schema5 shouldBe schema5Def
  }
  it("Object with Ref") {
    val (schema6, Vector(_)) = Definition.contractObjectDefinitions(Schema6)
    val schema6Def           = ObjectDefinition(
      Some("Schema6"),
      None,
      None,
      Vector(),
      Set(
        PropertyDefinition(
          "objectWithRef",
          ObjectDefinition(
            None,
            None,
            None,
            Vector("AnotherNested"),
            Set(
              PropertyDefinition(
                "field",
                IntegerDefinition(List(), Some(Long.MinValue), None, Some(Long.MaxValue), None, None),
                Nil,
                None,
                true,
                None
              )
            ),
            Left(false),
            None
          ),
          List(),
          None,
          true,
          None
        )
      )
    )
    schema6 shouldBe schema6Def
  }

  it("Object with Refs") {
    val (schema7, _) = Definition.contractObjectDefinitions(Schema7)
    val schema7Def   = ObjectDefinition(
      Some("Schema7"),
      None,
      None,
      Vector(),
      Set(
        PropertyDefinition(
          "objectWithRefs",
          ObjectDefinition(None, None, None, Vector("AnotherNested2", "AnotherNested"), Set(), Left(false), None),
          List(),
          None,
          true,
          None
        )
      ),
      Left(false),
      None
    )
    schema7 shouldBe schema7Def
  }

}
