package dsentric.schema

import dsentric.contracts.{Contract, DefinedAdditionalProperties, Length4String}
import dsentric.Dsentric._
import dsentric.DObject
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import dsentric.codecs.std.DCodecs._

@Description("Test description")
@Title("Test title")
object Annotated extends Contract {

  @Description("Test property description")
  @Examples("First example", "Second example")
  val stringProp = \[String]
}


class DefinitionSpec extends AnyFunSpec with Matchers {
  it("Should include any annotations") {
    val stringProp = PropertyDefinition(
      "stringProp",
      StringDefinition(),
      List("First example", "Second example"),
      None,
      true,
      Some("Test property description")
    )
    val result     =
      ObjectDefinition(
        Some("Annotated"),
        Some("Test title"),
        Some("Test description"),
        Vector.empty,
        Set(stringProp),
        Left(false)
      )

    Definition.nestedContractObjectDefinition(Annotated) shouldBe result
  }

  describe("Nested Contract Object Definition") {

    it("Should give empty if object is empty") {
      object Empty extends Contract
      Definition.nestedContractObjectDefinition(Empty) shouldBe ObjectDefinition(Some("Empty"))
    }
    it("Should mark additional properties if contract is closed") {
      object Closed extends Contract
      Definition.nestedContractObjectDefinition(Closed) shouldBe ObjectDefinition(
        Some("Closed"),
        None,
        None,
        Vector.empty,
        Set.empty,
        Left(false)
      )
    }

    it("Should mark additional properties if contract is open") {
      object EmptySchemaWithAdditionalProperties extends DefinedAdditionalProperties[Length4String, Boolean] with Contract
      Definition.nestedContractObjectDefinition(EmptySchemaWithAdditionalProperties) shouldBe ObjectDefinition(
        Some("EmptySchemaWithAdditionalProperties"),
        None,
        None,
        Vector.empty,
        Set.empty,
        Right(BooleanDefinition)
      )
    }

    it("Should exclude internal properties") {
      object WithInternal extends Contract {
        import dsentric.operators.StandardOperators._
        val int = \?[Int](internal)
      }

      Definition.nestedContractObjectDefinition(WithInternal) shouldBe ObjectDefinition(
        Some("WithInternal"),
        None,
        None,
        Vector.empty,
        Set.empty,
        Left(false)
      )
    }
    it("Should define expected object properties") {
      object WithExpected extends Contract {
        val exp = new \\ {
          val prop = \?[String]
        }
      }
      val expDefinition =
        ObjectDefinition(
          properties = Set(PropertyDefinition("prop", StringDefinition(), Nil, None, false, None)),
          additionalProperties = Left(false)
        )

      Definition.nestedContractObjectDefinition(WithExpected) shouldBe
        ObjectDefinition(
          Some("WithExpected"),
          None,
          None,
          Vector.empty,
          Set(PropertyDefinition("exp", expDefinition, Nil, None, true, None)),
          Left(false)
        )
    }
    it("Should define expected object properties with sub Contract") {
      sealed trait ExpectedSub extends SubContract {
        val prop = \?[String]
      }
      object WithExpected      extends Contract    {
        val exp = new \\ with ExpectedSub {}
      }
      val expDefinition =
        ObjectDefinition(
          properties = Set(PropertyDefinition("prop", StringDefinition(), Nil, None, false, None)),
          additionalProperties = Left(false)
        )

      Definition.nestedContractObjectDefinition(WithExpected) shouldBe
        ObjectDefinition(
          Some("WithExpected"),
          None,
          None,
          Vector.empty,
          Set(PropertyDefinition("exp", expDefinition, Nil, None, true, None)),
          Left(false)
        )
    }
    it("Should define maybe object properties") {
      object WithMaybe extends Contract {
        val maybe = new \\? {
          val prop = \![Boolean](false)
        }
      }
      val maybeDefinition =
        ObjectDefinition(
          properties = Set(PropertyDefinition("prop", BooleanDefinition, Nil, Some(false), false, None)),
          additionalProperties = Left(false)
        )

      Definition.nestedContractObjectDefinition(WithMaybe) shouldBe
        ObjectDefinition(
          Some("WithMaybe"),
          None,
          None,
          Vector.empty,
          Set(PropertyDefinition("maybe", maybeDefinition, Nil, None, false, None)),
          Left(false)
        )
    }
    it("Should define array properties") {
      object ArrayContract extends Contract {
        val prop1 = \?[Int]()
        val prop2 = \![String]("one")
      }
      object WithArray     extends Contract {
        val array = \[Vector[DObject]](ArrayContract)
      }
      val arrayContractDef =
        ObjectDefinition(
          properties = Set(
            PropertyDefinition(
              "prop1",
              IntegerDefinition(exclusiveMinimum = Some(0), exclusiveMaximum = Some(12)),
              Nil,
              None,
              false,
              None
            ),
            PropertyDefinition("prop2", StringDefinition(List("one", "two", "three")), Nil, Some("one"), false, None)
          ),
          additionalProperties = Left(false)
        )
      val arrayDef =
        ArrayDefinition(Vector(arrayContractDef), Some(2), None, false)

      Definition.nestedContractObjectDefinition(WithArray) shouldBe
        ObjectDefinition(
          Some("WithArray"),
          None,
          None,
          Vector.empty,
          Set(PropertyDefinition("array", arrayDef, Nil, None, true, None))
        )
    }
  }

}
