package dsentric.contracts

import dsentric.failure.{
  ClosedContractFailure,
  ContractFieldFailure,
  ExpectedFailure,
  IncorrectKeyTypeFailure,
  IncorrectTypeFailure
}
import dsentric.{DObject, Data, Dsentric}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ObjectPropertyLensSpec extends AnyFunSpec with Matchers with EitherValues {
  import Dsentric._
  import dsentric.Implicits._
  import dsentric.codecs.std.DCodecs._

  object Simple extends Contract with Open {
    val default = \![Int](999)
  }

  describe("Expected Object lens") {

    trait ExpectedContract extends SubContract {
      val empty = new \\ with Open

      val withoutExpected = new \\ with Open {
        val default = \![Int](1)
        val maybe   = \?[String]
      }

      val withExpected = new \\ with Open {
        val expected = \[String]
        val default  = \![Int](1)
        val maybe    = \?[String]
      }

      val expectedParent      = new \\ with Open {
        val expected = new \\ with Open {
          val expected = \[String]
          val default  = \![Int](1)
          val maybe    = \?[String]
        }
      }
      val expectedMaybeParent = new \\ with Open {
        val maybe = new \\? with Open {
          val default = \![Int](1)
          val maybe   = \?[String]
        }
      }
      val maybeParent         = new \\? with Open {
        val expected = new \\ with Open {
          val expected = \[String]
          val default  = \![Int](1)
          val maybe    = \?[String]
        }
      }

      val closedObject = new \\? {
        val expected = \[String]
        val maybe    = \?[Int]
      }
      val valueObject  = new \\\?[Length4String, Long]()() {
        val expected = \[String]
        val maybe    = \?[Int]
      }
      val objects      = new \\\?[Length4String, DObject](Simple)() {
        val expected = \[String]
        val maybe    = \?[Int]
      }
    }

    object ExpectedStructure extends Contract with ExpectedContract

    describe("$verify") {
      describe("No Path") {
        it("Should contain IncorrectTypeFailure if not an object") {
          val base = DObject("empty" := 1)
          ExpectedStructure.empty.$verify(base) should contain(IncorrectTypeFailure(ExpectedStructure.empty, 1))
        }
        it("Should return empty list if not present and no expected properties") {
          val base = DObject.empty
          ExpectedStructure.withoutExpected.$verify(base) shouldBe empty
        }
        it("Should return empty list if all expected present and all properties correct type") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := 2, "maybe" := "value"))
          ExpectedStructure.withExpected.$verify(base) shouldBe empty
        }
        it("Should return ExpectedFailure for expected properties if not present") {
          val base = DObject.empty
          ExpectedStructure.withExpected.$verify(base) should contain(
            ExpectedFailure(ExpectedStructure.withExpected.expected)
          )
        }
        it("Should return ExpectedFailure for expected properties if present but missing properties") {
          val base = DObject("withExpected" ::= ("default" := 2, "maybe" := "value"))
          ExpectedStructure.withExpected.$verify(base) should contain(
            ExpectedFailure(ExpectedStructure.withExpected.expected)
          )
        }
        it("Should return IncorrectTypeFailure for any properties with incorrect types") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := "four", "maybe" := false))
          ExpectedStructure.withExpected.$verify(base) should contain allOf (
            IncorrectTypeFailure(ExpectedStructure.withExpected.default, "four"),
            IncorrectTypeFailure(ExpectedStructure.withExpected.maybe, false)
          )
        }
      }
      describe("In Expected Path") {
        it("Should return ExpectedFailure for expected properties if parent not present") {
          val base = DObject.empty
          ExpectedStructure.expectedParent.expected.$verify(base) should contain(
            ExpectedFailure(ExpectedStructure.expectedParent.expected.expected)
          )
        }
      }
      describe("In Maybe Path") {
        it("Should return empty list if parent not present regardless of requirements of Expected properties") {
          val base = DObject.empty
          ExpectedStructure.maybeParent.expected.$verify(base) shouldBe empty
        }
      }
      describe("Closed for additional properties") {
        it("Should return empty list if no additional properties") {
          val base = DObject("closedObject" ::= ("expected" := "value"))
          ExpectedStructure.closedObject.$verify(base) shouldBe empty
        }
        it("Should return ClosedContractFailure if additional properties") {
          val base = DObject("closedObject" ::= ("expected" := "value", "additional" := 1))
          ExpectedStructure.closedObject.$verify(base) should contain(
            ClosedContractFailure(ExpectedStructure.closedObject, "additional")
          )
        }
        it("Should return ClosedContractFailure if additional properties and nested") {
          val base = DObject("closedObject" ::= ("expected" := "value", "additional" := 1))
          ExpectedStructure.closedObject.$verify(base) should contain(
            ClosedContractFailure(ExpectedStructure.closedObject, "additional")
          )
        }
      }
      describe("Additional properties") {
        it("Should return empty list if additional properties") {
          val base = DObject("withExpected" ::= ("expected" := "value", "additional" := 1))
          ExpectedStructure.withExpected.$verify(base) shouldBe empty
        }
        it("Should return empty list if additional properties and nested") {
          val base = DObject("withExpected" ::= ("expected" := "value", "additional" := 1))
          ExpectedStructure.withExpected.$verify(base) shouldBe empty
        }
      }
      describe("Additional values") {
        it("Should return empty list for valid additional values") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add1" := 56, "add2" := 0))
          ExpectedStructure.valueObject.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure for invalid value types") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add1" := "value", "add2" := 0))
          ExpectedStructure.valueObject.$verify(base) should contain(
            IncorrectTypeFailure(ExpectedStructure, ExpectedStructure.valueObject._path \ "add1", longCodec, "value")
          )
        }
        it("Should return IncorrectKeyTypeFailure for invalid key types") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add10" := 12, "add2" := 0))
          ExpectedStructure.valueObject.$verify(base) should contain(
            IncorrectKeyTypeFailure(
              ExpectedStructure,
              ExpectedStructure.valueObject._path,
              Length4String.fixedLength4StringCodec,
              "add10"
            )
          )
        }
      }
      describe("Additional objects") {
        it("Should return empty list for valid additional values") {
          val base =
            DObject("objects" ::= ("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2)))
          ExpectedStructure.objects.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure for invalid value types") {
          val base = DObject("objects" ::= ("expected" := "value", "add1" := "value", "add2" ::= ("default" := 2)))
          ExpectedStructure.objects.$verify(base) should contain(
            IncorrectTypeFailure(
              ExpectedStructure,
              ExpectedStructure.objects._path \ "add1",
              ExpectedStructure.objects._additionalValueCodec,
              "value"
            )
          )
        }
        it("Should return IncorrectTypeFailure for invalid contract values") {
          val base =
            DObject("objects" ::= ("expected" := "value", "add1" ::= ("default" := "value"), "add2" ::= ("default" := 2)))
          ExpectedStructure.objects.$verify(base) should contain(
            IncorrectTypeFailure(
              ExpectedStructure,
              ExpectedStructure.objects._path \ "add1" \ "default",
              intCodec,
              "value"
            )
          )
        }
        it("Should return IncorrectKeyTypeFailure for invalid key types") {
          val base = DObject(
            "objects" ::= ("expected" := "value", "add10" ::= ("default" := "value1"), "add2" ::= ("default" := "value2"))
          )
          ExpectedStructure.objects.$verify(base) should contain(
            IncorrectKeyTypeFailure(
              ExpectedStructure,
              ExpectedStructure.objects._path,
              Length4String.fixedLength4StringCodec,
              "add10"
            )
          )
        }
      }
    }
    describe("$get") {
      describe("No Path") {
        it("Should contain IncorrectTypeFailure if not an object") {
          val base = DObject("empty" := 1)
          ExpectedStructure.empty.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.empty, 1))
        }
        it("Should return empty object if not present and no expected properties") {
          val base = DObject.empty
          ExpectedStructure.empty.$get(base).value shouldBe DObject.empty
        }
        it("Should return object if all expected present and all properties correct type") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := 2, "maybe" := "value"))
          ExpectedStructure.withExpected.$get(base).value shouldBe DObject(
            "expected" := "value",
            "default" := 2,
            "maybe" := "value"
          )
        }
        it("Should return ExpectedFailure for expected properties if not present") {
          val base = DObject.empty
          ExpectedStructure.withExpected.$get(base).left.value should contain(
            ExpectedFailure(ExpectedStructure.withExpected.expected)
          )
        }
        it("Should return ExpectedFailure for expected properties if present but missing properties") {
          val base = DObject("withExpected" ::= ("default" := 2, "maybe" := "value"))
          ExpectedStructure.withExpected.$get(base).left.value should contain(
            ExpectedFailure(ExpectedStructure.withExpected.expected)
          )
        }
        it("Should provide default values if not set") {
          val base = DObject("withExpected" ::= ("expected" := "value", "maybe" := "value"))
          ExpectedStructure.withExpected.$get(base).value shouldBe DObject(
            "expected" := "value",
            "maybe" := "value",
            "default" := 1
          )
        }
        it("Should return IncorrectTypeFailure for any properties with incorrect types") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := "four", "maybe" := false))
          ExpectedStructure.withExpected.$get(base).left.value should contain allOf (
            IncorrectTypeFailure(ExpectedStructure.withExpected.default, "four"),
            IncorrectTypeFailure(ExpectedStructure.withExpected.maybe, false)
          )
        }
      }
      describe("In Expected Path") {
        it("Should return ExpectedFailure for expected properties if parent not present") {
          val base = DObject.empty
          ExpectedStructure.expectedParent.expected.$get(base).left.value should contain(
            ExpectedFailure(ExpectedStructure.expectedParent.expected.expected)
          )
        }
      }
      describe("In Maybe Path") {
        it("Should return empty list if parent not present regardless of presents of Expected properties") {
          val base = DObject.empty
          ExpectedStructure.maybeParent.expected.$get(base).value shouldBe None
        }
      }
      describe("Closed for additional properties") {
        it("Should return object if no additional properties") {
          val base = DObject("closedObject" ::= ("expected" := "value"))
          ExpectedStructure.closedObject.$get(base).value shouldBe Some(DObject("expected" := "value"))
        }
        it("Should fail with ClosedContractFailure if additional properties") {
          val base = DObject("closedObject" ::= ("expected" := "value", "additional" := 1))
          ExpectedStructure.closedObject.$get(base).left.value should contain(
            ClosedContractFailure(ExpectedStructure.closedObject, "additional")
          )
        }
      }
      describe("Additional properties") {
        it("Should return object if additional properties") {
          val base = DObject("withExpected" ::= ("expected" := "value", "additional" := 1))
          ExpectedStructure.withExpected.$get(base).value shouldBe DObject(
            "expected" := "value",
            "additional" := 1,
            "default" := 1
          )
        }
        it("Should return object if additional properties and nested") {
          val base = DObject("withExpected" ::= ("expected" := "value", "additional" := 1))
          ExpectedStructure.withExpected.$get(base).value shouldBe DObject(
            "expected" := "value",
            "additional" := 1,
            "default" := 1
          )
        }
      }
      describe("Additional values") {
        it("Should return obj for valid additional values") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add1" := 56, "add2" := 0))
          ExpectedStructure.valueObject.$get(base).value shouldBe Some(
            DObject("expected" := "value", "add1" := 56, "add2" := 0)
          )
        }
        it("Should fail with IncorrectTypeFailure for invalid value types") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add1" := "value", "add2" := 0))
          ExpectedStructure.valueObject.$get(base).left.value should contain(
            IncorrectTypeFailure(ExpectedStructure, ExpectedStructure.valueObject._path \ "add1", longCodec, "value")
          )
        }
        it("Should return IncorrectKeyTypeFailure for invalid key types") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add10" := 12, "add2" := 0))
          ExpectedStructure.valueObject.$get(base).left.value should contain(
            IncorrectKeyTypeFailure(
              ExpectedStructure,
              ExpectedStructure.valueObject._path,
              Length4String.fixedLength4StringCodec,
              "add10"
            )
          )
        }
      }
      describe("Additional objects") {
        it("Should return object for valid additional values") {
          val base =
            DObject("objects" ::= ("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2)))
          ExpectedStructure.objects.$get(base).value shouldBe Some(
            DObject("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2))
          )
        }
        it("Should return IncorrectTypeFailure for invalid value types") {
          val base = DObject("objects" ::= ("expected" := "value", "add1" := "value", "add2" ::= ("default" := 2)))
          ExpectedStructure.objects.$get(base).left.value should contain(
            IncorrectTypeFailure(
              ExpectedStructure,
              ExpectedStructure.objects._path \ "add1",
              ExpectedStructure.objects._additionalValueCodec,
              "value"
            )
          )
        }
        it("Should return IncorrectTypeFailure for invalid contract values") {
          val base =
            DObject("objects" ::= ("expected" := "value", "add1" ::= ("default" := "value"), "add2" ::= ("default" := 2)))
          ExpectedStructure.objects.$get(base).left.value should contain(
            IncorrectTypeFailure(
              ExpectedStructure,
              ExpectedStructure.objects._path \ "add1" \ "default",
              intCodec,
              "value"
            )
          )
        }
        it("Should return IncorrectKeyTypeFailure for invalid key types") {
          val base = DObject(
            "objects" ::= ("expected" := "value", "add10" ::= ("default" := "value1"), "add2" ::= ("default" := "value2"))
          )
          ExpectedStructure.objects.$get(base).left.value should contain(
            IncorrectKeyTypeFailure(
              ExpectedStructure,
              ExpectedStructure.objects._path,
              Length4String.fixedLength4StringCodec,
              "add10"
            )
          )
        }
        it("Should not apply contract defaults") {
          val base = DObject("objects" ::= ("expected" := "value", "add1" ::= ("value" := 123)))
          ExpectedStructure.objects.$get(base).value shouldBe Some(
            DObject("expected" := "value", "add1" ::= ("value" := 123))
          )
        }
      }
    }
    describe("$set") {
      describe("No path") {
        it("Should make no changes if setting an empty object property to empty") {
          val base = DObject.empty
          ExpectedStructure.empty.$set(DObject.empty)(base) shouldBe base
        }
        it("Should clear out the object if setting an object to empty") {
          val base = DObject("empty" ::= ("field" := "value"))
          ExpectedStructure.empty.$set(DObject.empty)(base) shouldBe DObject.empty
        }
        it("Should set a valid object") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "default" := 5, "maybe" := "value")
          ExpectedStructure.withExpected.$set(value)(base) shouldBe DObject("withExpected" := value)
        }
        it("Should still succeed if object doesn't contain expected properties") {
          val base  = DObject.empty
          val value = DObject("default" := 4)
          ExpectedStructure.withExpected.$set(value)(base) shouldBe DObject("withExpected" := value)
        }
        it("Should still succeed if object contains incorrect types") {
          val base  = DObject.empty
          val value = DObject("default" := false, "maybe" := 123)
          ExpectedStructure.withoutExpected.$set(value)(base) shouldBe DObject("withoutExpected" := value)
        }
        it("Should keep default value even if it matches the default property value") {
          val base  = DObject.empty
          val value = DObject("default" := 1)
          ExpectedStructure.withoutExpected.$set(value)(base) shouldBe DObject("withoutExpected" := value)
        }
        it("Should not remove empty artifacts") {
          val base  = DObject.empty
          val value = DObject("maybe" := DObject.empty)
          ExpectedStructure.expectedMaybeParent.$set(value)(base) shouldBe DObject("expectedMaybeParent" := value)
        }
        it("Should not remove empty other object values") {
          val base   = DObject.empty
          val value  = DObject("maybe" := DObject.empty, "additional" := DObject.empty)
          ExpectedStructure.expectedMaybeParent.$set(value)(base) shouldBe DObject(
            "expectedMaybeParent" ::= ("maybe" := DObject.empty, "additional" := DObject.empty)
          )
          val value2 = DObject("maybe" := DObject("additional" := DObject.empty))
          ExpectedStructure.expectedMaybeParent.$set(value2)(base) shouldBe DObject(
            "expectedMaybeParent" ::= ("maybe" ::= ("additional" := DObject.empty))
          )
        }
      }
      describe("In Expected Path") {
        it("Should create parent object if not present when setting") {
          val base  = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base) shouldBe DObject(
            "expectedParent" ::= ("expected" := value)
          )
        }
        it("Should replace parent object if not correct type when setting") {
          val base  = DObject("expectedParent" := 1)
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base) shouldBe DObject(
            "expectedParent" ::= ("expected" := value)
          )
        }
        it("Should merge in with existing properties when setting if present") {
          val base  = DObject("expectedParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base) shouldBe DObject(
            "expectedParent" ::= ("additional" := "value", "expected" := value)
          )
        }
      }
      describe("In Maybe Path") {
        it("Should create parent object if not present when setting") {
          val base  = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base) shouldBe DObject(
            "maybeParent" ::= ("expected" := value)
          )
        }
        it("Should replace parent object if not correct type when setting") {
          val base  = DObject("maybeParent" := 1)
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base) shouldBe DObject(
            "maybeParent" ::= ("expected" := value)
          )
        }
        it("Should merge in with existing properties when setting if present") {
          val base  = DObject("maybeParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base) shouldBe DObject(
            "maybeParent" ::= ("additional" := "value", "expected" := value)
          )
        }
      }
      describe("Closed for additional properties") {
        it("Should succeed if no additional properties") {
          val base  = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.closedObject.$set(value)(base) shouldBe DObject("closedObject" ::= ("expected" := "value"))
        }
        it("Should still succeed ClosedContractFailure if additional properties") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "additional" := 1)
          ExpectedStructure.closedObject.$set(value)(base) shouldBe DObject("closedObject" := value)
        }
      }
      describe("Additional properties") {
        it("Should succeed if additional properties") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "additional" := 1)
          ExpectedStructure.withExpected.$set(value)(base) shouldBe DObject(
            "withExpected" ::= ("expected" := "value", "additional" := 1)
          )
        }
      }
      describe("Additional values") {
        it("Should succeed if keys and values are valid") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" := 56, "add2" := 0)
          ExpectedStructure.valueObject.$set(value)(base) shouldBe DObject(
            "valueObject" ::= ("expected" := "value", "add1" := 56, "add2" := 0)
          )
        }
        it("Should still succeed for invalid value types") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" := "value", "add2" := 0)
          ExpectedStructure.valueObject.$set(value)(base) shouldBe DObject("valueObject" := value)
        }
        it("Should still succeed for invalid key types") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add10" := 12, "add2" := 0)
          ExpectedStructure.valueObject.$set(value)(base) shouldBe DObject("valueObject" := value)
        }
      }
      describe("Additional objects") {
        it("Should succeed if keys and values are valid") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2))
          ExpectedStructure.objects.$set(value)(base) shouldBe DObject(
            "objects" ::= ("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2))
          )
        }
        it("Should still succeed for invalid value types") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" := "value", "add2" ::= ("default" := 2))
          ExpectedStructure.objects.$set(value)(base) shouldBe DObject("objects" := value)
        }
        it("Should still succeed for invalid contract values") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" ::= ("default" := "value"), "add2" ::= ("default" := 2))
          ExpectedStructure.objects.$set(value)(base) shouldBe DObject("objects" := value)
        }
        it("Should still succeed for invalid key types") {
          val base  = DObject.empty
          val value =
            DObject("expected" := "value", "add10" ::= ("default" := "value1"), "add2" ::= ("default" := "value2"))
          ExpectedStructure.objects.$set(value)(base) shouldBe DObject("objects" := value)
        }
        it("Should not apply contract defaults") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" ::= ("value" := 123))
          ExpectedStructure.objects.$set(value)(base) shouldBe DObject(
            "objects" ::= ("expected" := "value", "add1" ::= ("value" := 123))
          )
        }
      }
    }
    describe("$maybeSet") {
      describe("No path") {
        it("Should make no changes if no value set") {
          val base = DObject.empty
          ExpectedStructure.empty.$maybeSet(None)(base) shouldBe base
        }
        it("Should clear out the object if setting an object to empty") {
          val base = DObject("empty" ::= ("field" := "value"))
          ExpectedStructure.empty.$set(DObject.empty)(base) shouldBe DObject.empty
        }
        it("Should set a valid object") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "default" := 5, "maybe" := "value")
          ExpectedStructure.withExpected.$set(value)(base) shouldBe DObject("withExpected" := value)
        }
        it("Should still succeed if object doesn't contain expected properties") {
          val base  = DObject.empty
          val value = DObject("default" := 4)
          ExpectedStructure.withExpected.$set(value)(base) shouldBe DObject("withExpected" := value)
        }
        it("Should still succeed if object contains incorrect types") {
          val base  = DObject.empty
          val value = DObject("default" := false, "maybe" := 123)
          ExpectedStructure.withoutExpected.$set(value)(base) shouldBe DObject("withoutExpected" := value)
        }
        it("Should keep default value even if it matches the default property value") {
          val base  = DObject.empty
          val value = DObject("default" := 1)
          ExpectedStructure.withoutExpected.$set(value)(base) shouldBe DObject("withoutExpected" := value)
        }
        it("Should not remove empty child object property values") {
          val base  = DObject.empty
          val value = DObject("maybe" := DObject.empty)
          ExpectedStructure.expectedMaybeParent.$set(value)(base) shouldBe DObject("expectedMaybeParent" := value)
        }
        it("Should not remove empty other object values") {
          val base   = DObject.empty
          val value  = DObject("maybe" := DObject.empty, "additional" := DObject.empty)
          ExpectedStructure.expectedMaybeParent.$set(value)(base) shouldBe DObject(
            "expectedMaybeParent" ::= ("maybe" := DObject.empty, "additional" := DObject.empty)
          )
          val value2 = DObject("maybe" := DObject("additional" := DObject.empty))
          ExpectedStructure.expectedMaybeParent.$set(value2)(base) shouldBe DObject(
            "expectedMaybeParent" ::= ("maybe" ::= ("additional" := DObject.empty))
          )
        }
      }
      describe("In Expected Path") {
        it("Should create parent object if not present when setting") {
          val base  = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base) shouldBe DObject(
            "expectedParent" ::= ("expected" := value)
          )
        }
        it("Should replace parent object if not correct type when setting") {
          val base  = DObject("expectedParent" := 1)
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base) shouldBe DObject(
            "expectedParent" ::= ("expected" := value)
          )
        }
        it("Should merge in with existing properties when setting if present") {
          val base  = DObject("expectedParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base) shouldBe DObject(
            "expectedParent" ::= ("additional" := "value", "expected" := value)
          )
        }
      }
      describe("In Maybe Path") {
        it("Should create parent object if not present when setting") {
          val base  = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base) shouldBe DObject(
            "maybeParent" ::= ("expected" := value)
          )
        }
        it("Should replace parent object if not correct type when setting") {
          val base  = DObject("maybeParent" := 1)
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base) shouldBe DObject(
            "maybeParent" ::= ("expected" := value)
          )
        }
        it("Should merge in with existing properties when setting if present") {
          val base  = DObject("maybeParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base) shouldBe DObject(
            "maybeParent" ::= ("additional" := "value", "expected" := value)
          )
        }
      }
    }
    describe("additionalProperties") {
      describe("$get") {
        it("Should return Some value if found for key") {
          val base = DObject("empty" ::= ("additional1" := "value"))
          ExpectedStructure.empty.$get("additional1")(base).value shouldBe Some(Data("value"))
        }
        it("Should return None if not found for key") {
          val base = DObject("empty" ::= ("additional2" := "value"))
          ExpectedStructure.empty.$get("additional1")(base).value shouldBe None
        }
        it("Should fail with ContractFieldFailure if key is contract property") {
          val base = DObject("withExpected" ::= ("expected" := "value"))
          ExpectedStructure.withExpected.$get("default")(base).left.value should contain(
            ContractFieldFailure(ExpectedStructure.withExpected, "default")
          )
        }
        it("Should return None if expected object not found") {
          val base = DObject.empty
          ExpectedStructure.empty.$get("additional1")(base).value shouldBe None
        }
        it("Should return IncorrectType if expected object wrong type") {
          val base = DObject("empty" := 1)
          ExpectedStructure.empty.$get("additional1")(base).left.value should contain(
            IncorrectTypeFailure(ExpectedStructure.empty, 1)
          )
        }
      }
      describe("$put") {
        it("Should add value to object if not exists") {
          val base = DObject("empty" ::= ("additional1" := "value"))
          ExpectedStructure.empty.$put("additional2" := false)(base).value shouldBe DObject(
            "empty" ::= ("additional1" := "value", "additional2" := false)
          )
        }
        it("Should replace dynamic data on the object") {
          val base = DObject("empty" ::= ("additional1" := "value", "additional2" := "value2"))
          ExpectedStructure.empty.$put("additional2" := false)(base).value shouldBe DObject(
            "empty" ::= ("additional1" := "value", "additional2" := false)
          )
        }
        it("Should create path to object if empty") {
          val base = DObject.empty
          ExpectedStructure.empty.$put("additional1" := false)(base).value shouldBe DObject(
            "empty" ::= ("additional1" := false)
          )
        }
        it("Should fail with ContractFieldFailure if key is contract property") {
          val base = DObject("withExpected" ::= ("expected" := "value"))
          ExpectedStructure.withExpected.$put("default" := 563)(base).left.value should contain(
            ContractFieldFailure(ExpectedStructure.withExpected, "default")
          )
        }
        it("Should replace invalid expected object if wrong type") {
          val base = DObject("empty" := true)
          ExpectedStructure.empty.$put("additional1" := false)(base).value shouldBe DObject(
            "empty" ::= ("additional1" := false)
          )
        }
      }
    }
  }

  describe("Maybe Object lens") {
    trait MaybeContract extends SubContract {
      val empty = new \\? with Open

      val withExpected = new \\? with Open {
        val expected = \[String]
        val default  = \![Int](1)
        val maybe    = \?[String]
      }

      val expectedParent = new \\ with Open {
        val maybe                = new \\? with Open {
          val expected = \[String]
          val default  = \![Int](1)
          val maybe    = \?[String]
        }
        val maybeWithoutExpected = new \\? with Open {
          val default = \![Int](1)
          val maybe   = \?[String]
        }
      }
      val maybeParent    = new \\? with Open {
        val expected = new \\ with Open {
          val expected = \[String]
          val default  = \![Int](1)
          val maybe    = \?[String]
        }
      }
      val closedObject   = new \\ {
        val expected = \[String]
        val maybe    = \?[Int]
      }
      val valueObject    = new \\\[Length4String, Long]()() {
        val expected = \[String]
        val maybe    = \?[Int]
      }
      val objects        = new \\\[Length4String, DObject](Simple)() {
        val expected = \[String]
        val maybe    = \?[Int]
      }
    }

    object MaybeStructure extends Contract with MaybeContract

    describe("$verify") {
      describe("No Path") {
        it("Should contain IncorrectTypeFailure if not an object") {
          val base = DObject("empty" := 1)
          MaybeStructure.empty.$verify(base) should contain(IncorrectTypeFailure(MaybeStructure.empty, 1))
        }
        it("Should return empty list if not present") {
          val base = DObject.empty
          MaybeStructure.withExpected.$verify(base) shouldBe empty
        }
        it("Should return empty list if all expected present and all properties correct type") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := 2, "maybe" := "value"))
          MaybeStructure.withExpected.$verify(base) shouldBe empty
        }
        it("Should return ExpectedFailure for expected properties if present but missing properties") {
          val base = DObject("withExpected" ::= ("default" := 2, "maybe" := "value"))
          MaybeStructure.withExpected.$verify(base) should contain(ExpectedFailure(MaybeStructure.withExpected.expected))
        }
        it("Should return IncorrectTypeFailure for any properties with incorrect types") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := "four", "maybe" := false))
          MaybeStructure.withExpected.$verify(base) should contain allOf (
            IncorrectTypeFailure(MaybeStructure.withExpected.default, "four"),
            IncorrectTypeFailure(MaybeStructure.withExpected.maybe, false)
          )
        }
      }
      describe("In Expected Path") {
        it("Should return empty list expected properties if parent not present") {
          val base = DObject.empty
          MaybeStructure.expectedParent.maybe.$verify(base) shouldBe empty
        }
      }
      describe("In Maybe Path") {
        it("Should return empty list if parent not present regardless of requirements of Expected properties") {
          val base = DObject.empty
          MaybeStructure.maybeParent.expected.$verify(base) shouldBe empty
        }
      }
      describe("Closed for additional properties") {
        it("Should return empty list if no additional properties") {
          val base = DObject("closedObject" ::= ("expected" := "value"))
          MaybeStructure.closedObject.$verify(base) shouldBe empty
        }
        it("Should return ClosedContractFailure if additional properties") {
          val base = DObject("closedObject" ::= ("expected" := "value", "additional" := 1))
          MaybeStructure.closedObject.$verify(base) should contain(
            ClosedContractFailure(MaybeStructure.closedObject, "additional")
          )
        }
        it("Should return ClosedContractFailure if additional properties and nested") {
          val base = DObject("closedObject" ::= ("expected" := "value", "additional" := 1))
          MaybeStructure.closedObject.$verify(base) should contain(
            ClosedContractFailure(MaybeStructure.closedObject, "additional")
          )
        }
      }
      describe("Additional properties") {
        it("Should return empty list if additional properties") {
          val base = DObject("withExpected" ::= ("expected" := "value", "additional" := 1))
          MaybeStructure.withExpected.$verify(base) shouldBe empty
        }
        it("Should return empty list if additional properties and nested") {
          val base = DObject("withExpected" ::= ("expected" := "value", "additional" := 1))
          MaybeStructure.withExpected.$verify(base) shouldBe empty
        }
      }
      describe("Additional values") {
        it("Should return empty list for valid additional values") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add1" := 56, "add2" := 0))
          MaybeStructure.valueObject.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure for invalid value types") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add1" := "value", "add2" := 0))
          MaybeStructure.valueObject.$verify(base) should contain(
            IncorrectTypeFailure(MaybeStructure, MaybeStructure.valueObject._path \ "add1", longCodec, "value")
          )
        }
        it("Should return IncorrectKeyTypeFailure for invalid key types") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add10" := 12, "add2" := 0))
          MaybeStructure.valueObject.$verify(base) should contain(
            IncorrectKeyTypeFailure(
              MaybeStructure,
              MaybeStructure.valueObject._path,
              Length4String.fixedLength4StringCodec,
              "add10"
            )
          )
        }
      }
      describe("Additional objects") {
        it("Should return empty list for valid additional values") {
          val base =
            DObject("objects" ::= ("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2)))
          MaybeStructure.objects.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure for invalid value types") {
          val base = DObject("objects" ::= ("expected" := "value", "add1" := "value", "add2" ::= ("default" := 2)))
          MaybeStructure.objects.$verify(base) should contain(
            IncorrectTypeFailure(
              MaybeStructure,
              MaybeStructure.objects._path \ "add1",
              MaybeStructure.objects._additionalValueCodec,
              "value"
            )
          )
        }
        it("Should return IncorrectTypeFailure for invalid contract values") {
          val base =
            DObject("objects" ::= ("expected" := "value", "add1" ::= ("default" := "value"), "add2" ::= ("default" := 2)))
          MaybeStructure.objects.$verify(base) should contain(
            IncorrectTypeFailure(MaybeStructure, MaybeStructure.objects._path \ "add1" \ "default", intCodec, "value")
          )
        }
        it("Should return IncorrectKeyTypeFailure for invalid key types") {
          val base = DObject(
            "objects" ::= ("expected" := "value", "add10" ::= ("default" := "value1"), "add2" ::= ("default" := "value2"))
          )
          MaybeStructure.objects.$verify(base) should contain(
            IncorrectKeyTypeFailure(
              MaybeStructure,
              MaybeStructure.objects._path,
              Length4String.fixedLength4StringCodec,
              "add10"
            )
          )
        }
      }
    }
    describe("$get") {
      describe("No Path") {
        it("Should contain IncorrectTypeFailure if not an object") {
          val base = DObject("empty" := 1)
          MaybeStructure.empty.$get(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.empty, 1))
        }
        it("Should return None if not present and no expected properties") {
          val base = DObject.empty
          MaybeStructure.empty.$get(base).value shouldBe None
        }
        it("Should return object if all expected present and all properties correct type") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := 2, "maybe" := "value"))
          MaybeStructure.withExpected.$get(base).value shouldBe Some(
            DObject("expected" := "value", "default" := 2, "maybe" := "value")
          )
        }
        it("Should return None for expected properties if not present and object not set") {
          val base = DObject.empty
          MaybeStructure.withExpected.$get(base).value shouldBe None
        }
        it("Should return ExpectedFailure for expected properties if present but missing properties") {
          val base = DObject("withExpected" ::= ("default" := 2, "maybe" := "value"))
          MaybeStructure.withExpected.$get(base).left.value should contain(
            ExpectedFailure(MaybeStructure.withExpected.expected)
          )
        }
        it("Should not provide default values if not set") {
          val base = DObject("withExpected" ::= ("expected" := "value", "maybe" := "value"))
          MaybeStructure.withExpected.$get(base).value shouldBe Some(DObject("expected" := "value", "maybe" := "value"))
        }
        it("Should return IncorrectTypeFailure for any properties with incorrect types") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := "four", "maybe" := false))
          MaybeStructure.withExpected.$get(base).left.value should contain allOf (
            IncorrectTypeFailure(MaybeStructure.withExpected.default, "four"),
            IncorrectTypeFailure(MaybeStructure.withExpected.maybe, false)
          )
        }
      }
      describe("In Expected Path") {
        it("Should return None if empty even with expected properties") {
          val base = DObject.empty
          MaybeStructure.expectedParent.maybe.$get(base).value shouldBe None
        }
      }
      describe("In Maybe Path") {
        it("Should return None if parent not present regardless of presents of Expected properties") {
          val base = DObject.empty
          MaybeStructure.maybeParent.expected.$get(base).value shouldBe None
        }
      }
      describe("Closed for additional properties") {
        it("Should return object if no additional properties") {
          val base = DObject("closedObject" ::= ("expected" := "value"))
          MaybeStructure.closedObject.$get(base).value shouldBe DObject("expected" := "value")
        }
        it("Should fail with ClosedContractFailure if additional properties") {
          val base = DObject("closedObject" ::= ("expected" := "value", "additional" := 1))
          MaybeStructure.closedObject.$get(base).left.value should contain(
            ClosedContractFailure(MaybeStructure.closedObject, "additional")
          )
        }
      }
      describe("Additional properties") {
        it("Should return object if additional properties") {
          val base = DObject("withExpected" ::= ("expected" := "value", "additional" := 1))
          MaybeStructure.withExpected.$get(base).value shouldBe Some(DObject("expected" := "value", "additional" := 1))
        }
        it("Should return object if additional properties and nested") {
          val base = DObject("withExpected" ::= ("expected" := "value", "additional" := 1))
          MaybeStructure.withExpected.$get(base).value shouldBe Some(DObject("expected" := "value", "additional" := 1))
        }
      }
      describe("Additional values") {
        it("Should return obj for valid additional values") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add1" := 56, "add2" := 0))
          MaybeStructure.valueObject.$get(base).value shouldBe DObject("expected" := "value", "add1" := 56, "add2" := 0)
        }
        it("Should fail with IncorrectTypeFailure for invalid value types") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add1" := "value", "add2" := 0))
          MaybeStructure.valueObject.$get(base).left.value should contain(
            IncorrectTypeFailure(MaybeStructure, MaybeStructure.valueObject._path \ "add1", longCodec, "value")
          )
        }
        it("Should return IncorrectKeyTypeFailure for invalid key types") {
          val base = DObject("valueObject" ::= ("expected" := "value", "add10" := 12, "add2" := 0))
          MaybeStructure.valueObject.$get(base).left.value should contain(
            IncorrectKeyTypeFailure(
              MaybeStructure,
              MaybeStructure.valueObject._path,
              Length4String.fixedLength4StringCodec,
              "add10"
            )
          )
        }
      }
      describe("Additional objects") {
        it("Should return object for valid additional values") {
          val base =
            DObject("objects" ::= ("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2)))
          MaybeStructure.objects.$get(base).value shouldBe
            DObject("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2))
        }
        it("Should return IncorrectTypeFailure for invalid value types") {
          val base = DObject("objects" ::= ("expected" := "value", "add1" := "value", "add2" ::= ("default" := 2)))
          MaybeStructure.objects.$get(base).left.value should contain(
            IncorrectTypeFailure(
              MaybeStructure,
              MaybeStructure.objects._path \ "add1",
              MaybeStructure.objects._additionalValueCodec,
              "value"
            )
          )
        }
        it("Should return IncorrectTypeFailure for invalid contract values") {
          val base =
            DObject("objects" ::= ("expected" := "value", "add1" ::= ("default" := "value"), "add2" ::= ("default" := 2)))
          MaybeStructure.objects.$get(base).left.value should contain(
            IncorrectTypeFailure(MaybeStructure, MaybeStructure.objects._path \ "add1" \ "default", intCodec, "value")
          )
        }
        it("Should return IncorrectKeyTypeFailure for invalid key types") {
          val base = DObject(
            "objects" ::= ("expected" := "value", "add10" ::= ("default" := "value1"), "add2" ::= ("default" := "value2"))
          )
          MaybeStructure.objects.$get(base).left.value should contain(
            IncorrectKeyTypeFailure(
              MaybeStructure,
              MaybeStructure.objects._path,
              Length4String.fixedLength4StringCodec,
              "add10"
            )
          )
        }
        it("Should apply contract defaults") {
          val base = DObject("objects" ::= ("expected" := "value", "add1" ::= ("value" := 123)))
          MaybeStructure.objects.$get(base).value shouldBe
            DObject("expected" := "value", "add1" ::= ("value" := 123, "default" := 999))
        }
      }
    }
    describe("$set") {
      describe("No path") {
        it("Should make no changes if setting an empty object property to empty") {
          val base = DObject.empty
          MaybeStructure.empty.$set(DObject.empty)(base) shouldBe base
        }
        it("Should clear out the object if setting an object to empty") {
          val base = DObject("empty" ::= ("field" := "value"))
          MaybeStructure.empty.$set(DObject.empty)(base) shouldBe DObject.empty
        }
        it("Should set a valid object") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "default" := 5, "maybe" := "value")
          MaybeStructure.withExpected.$set(value)(base) shouldBe DObject("withExpected" := value)
        }
        it("Should still succeed if object doesn't contain expected properties") {
          val base  = DObject.empty
          val value = DObject("default" := 4)
          MaybeStructure.withExpected.$set(value)(base) shouldBe DObject("withExpected" := value)
        }
        it("Should still succeed if object contains incorrect types") {
          val base  = DObject.empty
          val value = DObject("default" := false, "maybe" := 123)
          MaybeStructure.withExpected.$set(value)(base) shouldBe DObject("withExpected" := value)
        }
        it("Should keep default value even if it matches the default property value") {
          val base  = DObject.empty
          val value = DObject("default" := 1, "expected" := "value")
          MaybeStructure.withExpected.$set(value)(base) shouldBe DObject("withExpected" := value)
        }
        it("Should not removed empty child object property values") {
          val base  = DObject.empty
          val value = DObject("maybeWithoutExpected" := DObject.empty)
          MaybeStructure.expectedParent.$set(value)(base) shouldBe DObject("expectedParent" := value)
        }
        it("Should not remove empty other object values") {
          val base   = DObject.empty
          val value  = DObject("maybeWithoutExpected" := DObject.empty, "additional" := DObject.empty)
          MaybeStructure.expectedParent.$set(value)(base) shouldBe DObject(
            "expectedParent" ::= ("maybeWithoutExpected" := DObject.empty, "additional" := DObject.empty)
          )
          val value2 = DObject("maybeWithoutExpected" := DObject("additional" := DObject.empty))
          MaybeStructure.expectedParent.$set(value2)(base) shouldBe DObject(
            "expectedParent" ::= ("maybeWithoutExpected" ::= ("additional" := DObject.empty))
          )
        }
      }
      describe("In Expected Path") {
        it("Should create parent object if not present when setting") {
          val base  = DObject.empty
          val value = DObject("expected" := "value")
          MaybeStructure.expectedParent.maybe.$set(value)(base) shouldBe DObject("expectedParent" ::= ("maybe" := value))
        }
        it("Should replace parent object if not correct type when setting") {
          val base  = DObject("expectedParent" := 1)
          val value = DObject("expected" := "value")
          MaybeStructure.expectedParent.maybe.$set(value)(base) shouldBe DObject("expectedParent" ::= ("maybe" := value))
        }
        it("Should merge in with existing properties when setting if present") {
          val base  = DObject("expectedParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          MaybeStructure.expectedParent.maybe.$set(value)(base) shouldBe DObject(
            "expectedParent" ::= ("additional" := "value", "maybe" := value)
          )
        }
      }
      describe("In Maybe Path") {
        it("Should create parent object if not present when setting") {
          val base  = DObject.empty
          val value = DObject("expected" := "value")
          MaybeStructure.maybeParent.expected.$set(value)(base) shouldBe DObject("maybeParent" ::= ("expected" := value))
        }
        it("Should replace parent object if not correct type when setting") {
          val base  = DObject("maybeParent" := 1)
          val value = DObject("expected" := "value")
          MaybeStructure.maybeParent.expected.$set(value)(base) shouldBe DObject("maybeParent" ::= ("expected" := value))
        }
        it("Should merge in with existing properties when setting if present") {
          val base  = DObject("maybeParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          MaybeStructure.maybeParent.expected.$set(value)(base) shouldBe DObject(
            "maybeParent" ::= ("additional" := "value", "expected" := value)
          )
        }
      }
      describe("Closed for additional properties") {
        it("Should succeed if no additional properties") {
          val base  = DObject.empty
          val value = DObject("expected" := "value")
          MaybeStructure.closedObject.$set(value)(base) shouldBe DObject("closedObject" ::= ("expected" := "value"))
        }
        it("Should still succeed with ClosedContractFailure if additional properties") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "additional" := 1)
          MaybeStructure.closedObject.$set(value)(base) shouldBe DObject("closedObject" := value)
        }
      }
      describe("Additional properties") {
        it("Should succeed if additional properties") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "additional" := 1)
          MaybeStructure.withExpected.$set(value)(base) shouldBe DObject(
            "withExpected" ::= ("expected" := "value", "additional" := 1)
          )
        }
      }
      describe("Additional values") {
        it("Should succeed if keys and values are valid") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" := 56, "add2" := 0)
          MaybeStructure.valueObject.$set(value)(base) shouldBe DObject(
            "valueObject" ::= ("expected" := "value", "add1" := 56, "add2" := 0)
          )
        }
        it("Should still succeed for invalid value types") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" := "value", "add2" := 0)
          MaybeStructure.valueObject.$set(value)(base) shouldBe DObject("valueObject" := value)
        }
        it("Should still succeed for invalid key types") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add10" := 12, "add2" := 0)
          MaybeStructure.valueObject.$set(value)(base) shouldBe DObject("valueObject" := value)
        }
      }
      describe("Additional objects") {
        it("Should succeed if keys and values are valid") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2))
          MaybeStructure.objects.$set(value)(base) shouldBe DObject(
            "objects" ::= ("expected" := "value", "add1" ::= ("default" := 1), "add2" ::= ("default" := 2))
          )
        }
        it("Should still succeed for invalid value types") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" := "value", "add2" ::= ("default" := 2))
          MaybeStructure.objects.$set(value)(base) shouldBe DObject("objects" := value)
        }
        it("Should still succeed for invalid contract values") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" ::= ("default" := "value"), "add2" ::= ("default" := 2))
          MaybeStructure.objects.$set(value)(base) shouldBe DObject("objects" := value)
        }
        it("Should still succeed for invalid key types") {
          val base  = DObject.empty
          val value =
            DObject("expected" := "value", "add10" ::= ("default" := "value1"), "add2" ::= ("default" := "value2"))
          MaybeStructure.objects.$set(value)(base) shouldBe DObject("objects" := value)
        }
        it("Should not apply contract defaults") {
          val base  = DObject.empty
          val value = DObject("expected" := "value", "add1" ::= ("value" := 123))
          MaybeStructure.objects.$set(value)(base) shouldBe DObject(
            "objects" ::= ("expected" := "value", "add1" ::= ("value" := 123))
          )
        }
      }
    }
  }

}
