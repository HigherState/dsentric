package dsentric.schema

import dsentric.contracts.{AdditionalProperties, ClosedProperties, Contract}
import dsentric.operators.{Sanitizers, Validators}
import dsentric.Dsentric._
import dsentric.PessimisticCodecs._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


//Annotations cant be resolved if nested in test
//TODO annotation specific tests

//@Description("Test description")
//@Title("Test title")
//object Annotated extends Contract {
//
//  @Description("Test property description")
//  @Examples("First example", "Second example")
//  val stringProp = \[String]
//}
//
//it("Should include any annotations") {
//val stringProp = PropertyDefinition("stringProp", StringDefinition(), List("First example", "Second example"), None, true, Some("Test property description"))
//val result =
//ObjectDefinition(Some("Annotated"), Some("Test title"), Some("Test description"), Vector.empty, Set(stringProp), Left(true))
//
//Definition.nestedContractObjectDefinition(Annotated) shouldBe result
//}



class DefinitionSpec extends AnyFunSpec with Matchers {


  describe("Nested Contract Object Definition") {

    it("Should give empty if object is empty") {
      object Empty extends Contract
      Definition.nestedContractObjectDefinition(Empty) shouldBe ObjectDefinition(Some("Empty"))
    }
    it("Should mark additional properties if contract is closed") {
      object Closed extends Contract {
        override def $additionalProperties: AdditionalProperties = ClosedProperties
      }
      Definition.nestedContractObjectDefinition(Closed) shouldBe ObjectDefinition(Some("Closed"), None, None, Vector.empty, Set.empty, Left(false))
    }
    it("Should include properties with validation") {
      object Properties extends Contract {
        val longProp = \[Long](Validators.<(4) && Validators.>=(0))
        val stringProp = \?[String](Validators.maxLength(3))
        val arrayProp = \?[Vector[Int]](Validators.nonEmpty)
        val doubleProp = \![Double](4.56, Validators.in(1.23, 4.56, 7.89))
        val mapProp = \[Map[String, (Long, Boolean)]](Validators.maxLength(10))
      }
      val longProp = PropertyDefinition("longProp", IntegerDefinition(exclusiveMaximum = Some(4), minimum = Some(0)), Nil, None, true, None)
      val stringProp = PropertyDefinition("stringProp", StringDefinition(maxLength = Some(3)), Nil, None, false, None)
      val arrayProp = PropertyDefinition("arrayProp", ArrayDefinition(Vector(IntegerDefinition(minimum = Some(-2147483648), maximum = Some(2147483647))), Some(1)), Nil, None, false, None)
      val doubleProp = PropertyDefinition("doubleProp", NumberDefinition(List(1.23, 4.56, 7.89), minimum = Some(-1.7976931348623157E308), maximum = Some(1.7976931348623157E308)), Nil, Some(4.56), false, None)

      val mapValueDefinition =
        ArrayDefinition(Vector(IntegerDefinition(minimum = Some(-9223372036854775808L), maximum = Some(9223372036854775807L)), BooleanDefinition))
      val mapProp = PropertyDefinition("mapProp", ObjectDefinition(additionalProperties = Right(mapValueDefinition), maxProperties = Some(10)), Nil, None, true, None)

      val result =
        ObjectDefinition(Some("Properties"), None, None, Vector.empty, Set(longProp, stringProp, arrayProp, doubleProp, mapProp), Left(true))

      Definition.nestedContractObjectDefinition(Properties) shouldBe result
    }

    it("Should exclude internal properties") {
       object WithInternal extends Contract {
         val int = \?[Int](Sanitizers.internal)
       }

      Definition.nestedContractObjectDefinition(WithInternal) shouldBe ObjectDefinition(Some("WithInternal"), None, None, Vector.empty, Set.empty, Left(true))
    }
    it("Should define expected object properties") {
      object WithExpected extends Contract {
        val exp = new \\ {
          val prop = \?[String]

          override def $additionalProperties: AdditionalProperties = ClosedProperties
        }
      }
      val expDefinition =
        ObjectDefinition(properties = Set(PropertyDefinition("prop", StringDefinition(), Nil, None, false, None)), additionalProperties = Left(false))

      Definition.nestedContractObjectDefinition(WithExpected) shouldBe
        ObjectDefinition(Some("WithExpected"), None, None, Vector.empty, Set(PropertyDefinition("exp", expDefinition, Nil, None, true, None)), Left(true))
    }
    it("Should define expected object properties with sub Contract") {
      sealed trait ExpectedSub extends SubContract {
        val prop = \?[String]
      }
      object WithExpected extends Contract {
        val exp = new \\ with ExpectedSub {
          override def $additionalProperties: AdditionalProperties = ClosedProperties
        }
      }
      val expDefinition =
        ObjectDefinition(properties = Set(PropertyDefinition("prop", StringDefinition(), Nil, None, false, None)), additionalProperties = Left(false))

      Definition.nestedContractObjectDefinition(WithExpected) shouldBe
        ObjectDefinition(Some("WithExpected"), None, None, Vector.empty, Set(PropertyDefinition("exp", expDefinition, Nil, None, true, None)), Left(true))
    }
    it("Should define maybe object properties") {
      object WithMaybe extends Contract{
        val maybe = new \\? {
          val prop = \![Boolean](false)
        }
        override def $additionalProperties: AdditionalProperties = ClosedProperties
      }
      val maybeDefinition =
        ObjectDefinition(properties = Set(PropertyDefinition("prop", BooleanDefinition, Nil, Some(false), false, None)), additionalProperties = Left(true))

      Definition.nestedContractObjectDefinition(WithMaybe) shouldBe
        ObjectDefinition(Some("WithMaybe"), None, None, Vector.empty, Set(PropertyDefinition("maybe", maybeDefinition, Nil, None, false, None)), Left(false))
    }
    it("Should define array properties") {
      object ArrayContract extends Contract {
        val prop1 = \?[Int](Validators.>(0), Validators.<(12))
        val prop2 = \![String]("one", Validators.in("one", "two", "three"))

        override def $additionalProperties: AdditionalProperties = ClosedProperties
      }
      object WithArray extends Contract {
        val array = \::(ArrayContract, Validators.minLength(2))
      }
      val arrayContractDef =
        ObjectDefinition(properties = Set(
          PropertyDefinition("prop1", IntegerDefinition(exclusiveMinimum = Some(0), exclusiveMaximum = Some(12)), Nil, None, false, None),
          PropertyDefinition("prop2", StringDefinition(List("one", "two", "three")), Nil, Some("one"), false, None)
        ), additionalProperties = Left(false))
      val arrayDef =
        ArrayDefinition(Vector(arrayContractDef), Some(2), None, false)

      Definition.nestedContractObjectDefinition(WithArray) shouldBe
        ObjectDefinition(Some("WithArray"), None, None, Vector.empty, Set(PropertyDefinition("array", arrayDef, Nil, None, true, None)))
    }
  }

}
