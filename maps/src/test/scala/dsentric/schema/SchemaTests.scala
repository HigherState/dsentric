package dsentric.schema

import dsentric.Dsentric._
import dsentric.PessimisticCodecs._
import dsentric._
import namespaced.{AnotherNested, AnotherNested2, InheritedNested, ToRename}
import org.scalatest.{FunSuite, Matchers}

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
  val objectWithRef = new \\? with AnotherNested {
     val field = \[Long]
  }
}

object Schema7 extends Contract {
  val objectWithRefs = new \\ with AnotherNested with AnotherNested2
}

object Schema8 extends Contract {
  val directRename = new \\!(DObject("Value" := 3)) with ToRename
}


class SchemaTests extends FunSuite with Matchers {
  val anotherNestedDef =
    ObjectDefinition(
      Some("AnotherNested"),
      None,
      None,
      Vector.empty,
      Vector(PropertyDefinition("prop", BooleanDefinition, Nil, None, true, Some("Another nested prop")))
    )

  val anotherNested2Def =
    ObjectDefinition(
      Some("AnotherNested2"),
      None,
      None,
      Vector.empty,
      Vector(PropertyDefinition("prop2", BooleanDefinition, Nil, None, true, Some("Another nested 2 prop 2")))
    )

  val inheritedDef = ObjectDefinition(
    Some("InheritedNested"),
    None,
    None,
    Vector.empty,
    Vector(PropertyDefinition("prop", BooleanDefinition, Nil, None, true, Some("Another nested prop")), PropertyDefinition("propHigher", StringDefinition(), Nil, None, true, None))
  )

  val toRenameDef = ObjectDefinition(
    Some("Renamed"),
    None,
    None,
    Vector("AnotherNested2", "AnotherNested"),
    Vector(PropertyDefinition("propHigher", StringDefinition(), List("one", "two"), None, false, None))
  )

  test("Multiinheritance no nesting") {
    val schema1 = Schema.contractsObjectDefinitions(Schema1).find(_.definition.contains("Schema1")).head
    val schema1Def = ObjectDefinition(
      Some("Schema1"),
      Some("Schema1 Contract"),
      None,
      Vector("AnotherNested"),
      Vector(PropertyDefinition("field", StringDefinition(), Nil, None, true, None))
    )
    schema1 shouldBe schema1Def
  }

  test("Multi-inheritance full nesting") {
    val schema2 = Schema.contractsObjectDefinitions(Schema2).head
    val schema2Def = ObjectDefinition(
      Some("Schema2"),
      None,
      None,
      Vector.empty,
      Vector(PropertyDefinition("field", StringDefinition(), Nil, None, true, None),PropertyDefinition("prop", BooleanDefinition, Nil, None, true, Some("Another nested prop")))
    )
  }

  test("Multi-inheritance partial nesting") {
    val Vector(inherited, schema3) = Schema.contractsObjectDefinitions(Schema3)
    val schema3Def = ObjectDefinition(
      Some("Schema3"),
      None,
      None,
      Vector("InheritedNested"),
      Vector(PropertyDefinition("field", StringDefinition(), Nil, Some("value"), false, None))
    )

    schema3 shouldBe schema3Def
    inherited shouldBe inheritedDef
  }

  test("Renamed inherited") {
    val Vector(anotherNested2, anotherNested, toRename, schema4) = Schema.contractsObjectDefinitions(Schema4)
    val schema4Def = ObjectDefinition(
      Some("Schema4"),
      None,
      None,
      Vector("Renamed"),
      Vector(PropertyDefinition("field", StringDefinition(), Nil, None, false, None))
    )
    anotherNested2 shouldBe anotherNested2Def
    anotherNested shouldBe anotherNestedDef
    toRename shouldBe toRenameDef
    schema4 shouldBe schema4Def
  }

  test("Direct inheritance reference") {
    val Vector(anotherNested, schema5) = Schema.contractsObjectDefinitions(Schema5)
    val directRef = PropertyDefinition("directRef", ByRefDefinition("AnotherNested"), Nil, None, true, None)
    val schema5Def = ObjectDefinition(
      Some("Schema5"),
      None,
      Some("directly nested field"),
      Vector.empty,
      Vector(directRef)
    )
    anotherNested shouldBe anotherNestedDef
    schema5 shouldBe schema5Def
  }
  test("Object with Ref") {
    val Vector(anotherNested, schema6) = Schema.contractsObjectDefinitions(Schema6)
    val schema6Def = ObjectDefinition(
      Some("Schema6"),
      None,
      None,
      Vector(),
      Vector(
        PropertyDefinition("objectWithRef",
          ObjectDefinition(None,None,None,Vector("AnotherNested"),
            Vector(PropertyDefinition("field",IntegerDefinition(List(),Some(Long.MinValue),None,Some(Long.MaxValue),None,None),Nil,None, true,None)),
            true,None),
          List(),None,false,None)),
      )
    schema6 shouldBe schema6Def
  }

  test("Object with Refs") {
    val Vector(_, _, schema7) = Schema.contractsObjectDefinitions(Schema7)
    val schema7Def = ObjectDefinition(
      Some("Schema7"),
      None,
      None,
      Vector(),
      Vector(
        PropertyDefinition("objectWithRefs",
          ObjectDefinition(None,None,None,Vector("AnotherNested2", "AnotherNested"),Vector(),true,None),
          List(),None,true,None)),
      true,None)
    schema7 shouldBe schema7Def
  }

  test("Direct inheritance reference renamed") {
    val Vector(_, _, _, schema8) = Schema.contractsObjectDefinitions(Schema8)
    val directRef = PropertyDefinition("directRename", ByRefDefinition("Renamed"), Nil, Some(Map("Value" -> 3)), false, None)
    val schema8Def = ObjectDefinition(
      Some("Schema8"),
      None,
      None,
      Vector.empty,
      Vector(directRef)
    )
    schema8 shouldBe schema8Def
  }

}
