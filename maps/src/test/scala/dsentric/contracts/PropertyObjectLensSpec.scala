package dsentric.contracts

import dsentric.failure.{ClosedContractFailure, ExpectedFailure, IncorrectTypeFailure}
import dsentric.{DObject, Data, Dsentric, Path, PessimisticCodecs}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

@deprecated("Update when EitherValues is updated and undeprecate.", "")
class PropertyObjectLensSpec extends AnyFunSpec with Matchers with EitherValues {
  import Dsentric._
  import PessimisticCodecs._
  import dsentric.Implicits._

  describe("Expected Object lens") {
    trait ExpectedContract extends SubContract {
      val empty = new \\ with AdditionalProperties

      val withoutExpected = new \\ with AdditionalProperties {
        val default = \![Int](1)
        val maybe = \?[String]
      }

      val withExpected = new \\ with AdditionalProperties {
        val expected = \[String]
        val default = \![Int](1)
        val maybe = \?[String]
      }

      val expectedParent = new \\ with AdditionalProperties {
        val expected = new \\ with AdditionalProperties {
          val expected = \[String]
          val default = \![Int](1)
          val maybe = \?[String]
        }
      }
      val expectedMaybeParent = new \\ with AdditionalProperties {
        val maybe = new \\? with AdditionalProperties {
          val default = \![Int](1)
          val maybe = \?[String]
        }
      }
      val maybeParent = new \\? with AdditionalProperties {
        val expected = new \\ with AdditionalProperties {
          val expected = \[String]
          val default = \![Int](1)
          val maybe = \?[String]
        }
      }
    }

    object ExpectedStructure extends Contract with ExpectedContract

    object EmptyExpectedStructure extends Contract with ExpectedContract with EmptyOnIncorrectType

    object ClosedExpectedObject extends Contract {
      val expectedObject = new  \\ {
        val expected = \[String]
        val maybe = \?[Int]
      }
    }
    object OpenExpectedObject extends Contract  {
      val expectedObject = new  \\ with AdditionalProperties {
        val expected = \[String]
        val maybe = \?[Int]
      }
    }
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
          ExpectedStructure.withExpected.$verify(base) should contain (ExpectedFailure(ExpectedStructure.withExpected.expected))
        }
        it("Should return ExpectedFailure for expected properties if present but missing properties") {
          val base = DObject("withExpected" ::= ("default" := 2, "maybe" := "value"))
          ExpectedStructure.withExpected.$verify(base) should contain (ExpectedFailure(ExpectedStructure.withExpected.expected))
        }
        it("Should return IncorrectTypeFailure for any properties with incorrect types") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := "four", "maybe" := false))
          ExpectedStructure.withExpected.$verify(base) should contain allOf (
            IncorrectTypeFailure(ExpectedStructure.withExpected.default, "four"),
            IncorrectTypeFailure(ExpectedStructure.withExpected.maybe, false)
          )
        }
        describe("With EmptyOnIncorrectType") {
          it("Should returns IncorrectTypeFailure if incorrect type") {
            val base = DObject("empty" := 1)
            EmptyExpectedStructure.empty.$verify(base) should contain (IncorrectTypeFailure(EmptyExpectedStructure.empty, 1))
          }
        }
      }
      describe("In Expected Path") {
        it("Should return ExpectedFailure for expected properties if parent not present") {
          val base = DObject.empty
          ExpectedStructure.expectedParent.expected.$verify(base) should contain (ExpectedFailure(ExpectedStructure.expectedParent.expected.expected))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectTypeFailure for parent if parent is incorrect type") {
            val base = DObject("expectedParent" := 1)
            EmptyExpectedStructure.expectedParent.expected.$verify(base) should contain (IncorrectTypeFailure(EmptyExpectedStructure.expectedParent, 1))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return empty list if parent not present regardless of requirements of Expected properties") {
          val base = DObject.empty
          ExpectedStructure.maybeParent.expected.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectTypeFailure for parent if parent is incorrect type") {
            val base = DObject("maybeParent" := 1)
            EmptyExpectedStructure.maybeParent.expected.$verify(base) should contain (IncorrectTypeFailure(EmptyExpectedStructure.maybeParent, 1))
          }
        }
      }
      describe("Closed for additional properties") {
        it("Should return empty list if no additional properties") {
          val base = DObject("expectedObject" ::= ("expected" := "value"))
          ClosedExpectedObject.expectedObject.$verify(base) shouldBe empty
        }
        it("Should return ClosedContractFailure if additional properties") {
          val base = DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
          ClosedExpectedObject.expectedObject.$verify(base) should contain (ClosedContractFailure(ClosedExpectedObject.expectedObject, "additional"))
        }
        it("Should return ClosedContractFailure if additional properties and nested") {
          val base = DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
          ClosedExpectedObject.$verify(base) should contain (ClosedContractFailure(ClosedExpectedObject.expectedObject, "additional"))
        }
      }
      describe("Additional properties") {
        it("Should return empty list if additional properties") {
          val base = DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
          OpenExpectedObject.expectedObject.$verify(base) shouldBe empty
        }
        it("Should return empty list if additional properties and nested") {
          val base = DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
          OpenExpectedObject.$verify(base) shouldBe empty
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
          ExpectedStructure.empty.$get(base).right.value shouldBe Some(DObject.empty)
        }
        it("Should return object if all expected present and all properties correct type") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := 2, "maybe" := "value"))
          ExpectedStructure.withExpected.$get(base).right.value shouldBe Some(DObject("expected" := "value", "default" := 2, "maybe" := "value"))
        }
        it("Should return ExpectedFailure for expected properties if not present") {
          val base = DObject.empty
          ExpectedStructure.withExpected.$get(base).left.value should contain (ExpectedFailure(ExpectedStructure.withExpected.expected))
        }
        it("Should return ExpectedFailure for expected properties if present but missing properties") {
          val base = DObject("withExpected" ::= ("default" := 2, "maybe" := "value"))
          ExpectedStructure.withExpected.$get(base).left.value should contain (ExpectedFailure(ExpectedStructure.withExpected.expected))
        }
        it("Should provide default values if not set") {
          val base = DObject("withExpected" ::= ("expected" := "value", "maybe" := "value"))
          ExpectedStructure.withExpected.$get(base).right.value shouldBe Some(DObject("expected" := "value", "default" := 1, "maybe" := "value"))
        }
        it("Should return IncorrectTypeFailure for any properties with incorrect types") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := "four", "maybe" := false))
          ExpectedStructure.withExpected.$get(base).left.value should contain allOf (
            IncorrectTypeFailure(ExpectedStructure.withExpected.default, "four"),
            IncorrectTypeFailure(ExpectedStructure.withExpected.maybe, false)
          )
        }
        describe("With EmptyOnIncorrectType") {
          it("Should returns empty object and no expected or default properties") {
            val base = DObject("empty" := 1)
            EmptyExpectedStructure.empty.$get(base).right.value shouldBe Some(DObject.empty)
          }
          it("Should returns object with defaults if incorrect type") {
            val base = DObject("withoutExpected" := 1)
            EmptyExpectedStructure.withoutExpected.$get(base).right.value shouldBe Some(DObject("default" := 1))
          }
          it("Should contain ExpectedFailure for expected properties if incorrect type") {
            val base = DObject("withExpected" := 1)
            EmptyExpectedStructure.withExpected.$get(base).left.value should contain (ExpectedFailure(EmptyExpectedStructure.withExpected.expected))
          }
          it("Should return object excluding maybe if maybe has incorrect type") {
            val base = DObject("withoutExpected" ::= ("default" := 5, "maybe" := 123))
            EmptyExpectedStructure.withoutExpected.$get(base).right.value shouldBe Some(DObject("default" := 5))
          }
          it("Should return object with default value applied if default has incorrect type") {
            val base = DObject("withoutExpected" ::= ("default" := true))
            EmptyExpectedStructure.withoutExpected.$get(base).right.value shouldBe Some(DObject("default" := 1))
          }
          it("Should contain ExpectedFailure for any expected properties of the wrong type") {
            val base = DObject("withExpected" ::= ("expected" := 123))
            EmptyExpectedStructure.withExpected.$get(base).left.value should contain (ExpectedFailure(EmptyExpectedStructure.withExpected.expected))
          }
        }
      }
      describe("In Expected Path") {
        it("Should return ExpectedFailure for expected properties if parent not present") {
          val base = DObject.empty
          ExpectedStructure.expectedParent.expected.$get(base).left.value should contain (ExpectedFailure(ExpectedStructure.expectedParent.expected.expected))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return ExpectedFailure for expected properties if parent is incorrect type") {
            val base = DObject("expectedParent" := 1)
            EmptyExpectedStructure.expectedParent.expected.$get(base).left.value should contain (ExpectedFailure(EmptyExpectedStructure.expectedParent.expected.expected))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return empty list if parent not present regardless of presents of Expected properties") {
          val base = DObject.empty
          ExpectedStructure.maybeParent.expected.$get(base).right.value shouldBe None
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return empty list if parent is incorrect type") {
            val base = DObject("maybeParent" := 1)
            EmptyExpectedStructure.maybeParent.expected.$get(base).right.value shouldBe None
          }
        }
      }
      describe("Closed for additional properties") {
        it("Should return object if no additional properties") {
          val base = DObject("expectedObject" ::= ("expected" := "value"))
          ClosedExpectedObject.expectedObject.$get(base).right.value shouldBe Some(DObject("expected" := "value"))
        }
        it("Should fail with ClosedContractFailure if additional properties") {
          val base = DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
          ClosedExpectedObject.expectedObject.$get(base).left.value should contain (ClosedContractFailure(ClosedExpectedObject.expectedObject, "additional"))
        }
        it("Should fail with ClosedContractFailure if additional properties and nested") {
          val base = DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
          ClosedExpectedObject.$get(base).left.value should contain (ClosedContractFailure(ClosedExpectedObject.expectedObject, "additional"))
        }
      }
      describe("Additional properties") {
        it("Should return object if additional properties") {
          val base = DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
          OpenExpectedObject.expectedObject.$get(base).right.value shouldBe Some(DObject("expected" := "value", "additional" := 1))
        }
        it("Should return object if additional properties and nested") {
          val base = DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
          val t = OpenExpectedObject.$get(base)
          t.right.value shouldBe Some(base)
        }
      }
    }
    describe("$set") {
      describe("No path") {
        it("Should make no changes if setting an empty object property to empty") {
          val base = DObject.empty
          ExpectedStructure.empty.$set(DObject.empty)(base).right.value shouldBe base
        }
        it("Should clear out the object if setting an object to empty") {
          val base = DObject("empty" ::= ("field" := "value"))
          ExpectedStructure.empty.$set(DObject.empty)(base).right.value shouldBe DObject.empty
        }
        it("Should set a valid object") {
          val base = DObject.empty
          val value = DObject("expected" := "value", "default" := 5, "maybe" := "value")
          ExpectedStructure.withExpected.$set(value)(base).right.value shouldBe DObject("withExpected" := value)
        }
        it("Should fail with ExpectedFailure if object doesn't contain expected properties") {
          val base = DObject.empty
          val value = DObject("default" := 4)
          ExpectedStructure.withExpected.$set(value)(base).left.value should contain (ExpectedFailure(ExpectedStructure.withExpected.expected))
        }
        it("Should fail with IncorrectTypeFailure if object contains incorrect types") {
          val base = DObject.empty
          val value = DObject("default" := false, "maybe" := 123)
          ExpectedStructure.withoutExpected.$set(value)(base).left.value should contain allOf(
            IncorrectTypeFailure(ExpectedStructure.withoutExpected.default, false),
            IncorrectTypeFailure(ExpectedStructure.withoutExpected.maybe, 123)
          )
        }
        it("Should keep default value even if it matches the default property value") {
          val base = DObject.empty
          val value = DObject("default" := 1)
          ExpectedStructure.withoutExpected.$set(value)(base).right.value shouldBe DObject("withoutExpected" := value)
        }
        it("Should removed empty child object property values if verified") {
          val base = DObject.empty
          val value = DObject("maybe" := DObject.empty)
          ExpectedStructure.expectedMaybeParent.$set(value)(base).right.value shouldBe base
        }
        it("Should not remove empty other object values if verified") {
          val base = DObject.empty
          val value = DObject("maybe" := DObject.empty, "additional" := DObject.empty)
          ExpectedStructure.expectedMaybeParent.$set(value)(base).right.value shouldBe DObject("expectedMaybeParent" ::= ("additional" := DObject.empty))
          val value2 = DObject("maybe" := DObject("additional" := DObject.empty))
          ExpectedStructure.expectedMaybeParent.$set(value2)(base).right.value shouldBe DObject("expectedMaybeParent" ::= ("maybe" ::= ("additional" := DObject.empty)))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with IncorrectTypeFailure as EmptyOnIncorrectType is irrelevant") {
            val base = DObject.empty
            val value = DObject("default" := false, "maybe" := 123)
            EmptyExpectedStructure.withoutExpected.$set(value)(base).left.value should contain allOf(
              IncorrectTypeFailure(EmptyExpectedStructure.withoutExpected.default, false),
              IncorrectTypeFailure(EmptyExpectedStructure.withoutExpected.maybe, 123)
            )
          }
        }
      }
      describe("In Expected Path") {
        it("Should create parent object if not present when setting") {
          val base = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("expected" := value))
        }
        it("Should replace parent object if not correct type when setting") {
          val base = DObject("expectedParent" := 1)
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("expected" := value))
        }
        it("Should merge in with existing properties when setting if present") {
          val base = DObject("expectedParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("additional" := "value", "expected" := value))
        }
      }
      describe("In Maybe Path") {
        it("Should create parent object if not present when setting") {
          val base = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("expected" := value))
        }
        it("Should replace parent object if not correct type when setting") {
          val base = DObject("maybeParent" := 1)
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("expected" := value))
        }
        it("Should merge in with existing properties when setting if present") {
          val base = DObject("maybeParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("additional" := "value", "expected" := value))
        }
      }
      describe("Closed for additional properties") {
        it("Should succeed if no additional properties") {
          val base = DObject.empty
          val value = DObject("expected" := "value")
          ClosedExpectedObject.expectedObject.$set(value)(base).right.value shouldBe DObject("expectedObject" ::= ("expected" := "value"))
        }
        it("Should fail with ClosedContractFailure if additional properties") {
          val base = DObject.empty
          val value = DObject("expected" := "value", "additional" := 1)
          ClosedExpectedObject.expectedObject.$set(value)(base).left.value should contain (ClosedContractFailure(ClosedExpectedObject.expectedObject, "additional"))
        }
      }
      describe("Additional properties") {
        it("Should succeed if additional properties") {
          val base = DObject.empty
          val value = DObject("expected" := "value", "additional" := 1)
          OpenExpectedObject.expectedObject.$set(value)(base).right.value shouldBe DObject("expectedObject" ::= ("expected" := "value", "additional" := 1))
        }
      }
    }
    describe("$maybeSet") {
      describe("No path") {
        it("Should make no changes if no value set") {
          val base = DObject.empty
          ExpectedStructure.empty.$maybeSet(None)(base).right.value shouldBe base
        }
        it("Should clear out the object if setting an object to empty") {
          val base = DObject("empty" ::= ("field" := "value"))
          ExpectedStructure.empty.$set(DObject.empty)(base).right.value shouldBe DObject.empty
        }
        it("Should set a valid object") {
          val base = DObject.empty
          val value = DObject("expected" := "value", "default" := 5, "maybe" := "value")
          ExpectedStructure.withExpected.$set(value)(base).right.value shouldBe DObject("withExpected" := value)
        }
        it("Should fail with ExpectedFailure if object doesn't contain expected properties") {
          val base = DObject.empty
          val value = DObject("default" := 4)
          ExpectedStructure.withExpected.$set(value)(base).left.value should contain (ExpectedFailure(ExpectedStructure.withExpected.expected))
        }
        it("Should fail with IncorrectTypeFailure if object contains incorrect types") {
          val base = DObject.empty
          val value = DObject("default" := false, "maybe" := 123)
          ExpectedStructure.withoutExpected.$set(value)(base).left.value should contain allOf(
            IncorrectTypeFailure(ExpectedStructure.withoutExpected.default, false),
            IncorrectTypeFailure(ExpectedStructure.withoutExpected.maybe, 123)
          )
        }
        it("Should keep default value even if it matches the default property value") {
          val base = DObject.empty
          val value = DObject("default" := 1)
          ExpectedStructure.withoutExpected.$set(value)(base).right.value shouldBe DObject("withoutExpected" := value)
        }
        it("Should removed empty child object property values if verified") {
          val base = DObject.empty
          val value = DObject("maybe" := DObject.empty)
          ExpectedStructure.expectedMaybeParent.$set(value)(base).right.value shouldBe base
        }
        it("Should not remove empty other object values if verified") {
          val base = DObject.empty
          val value = DObject("maybe" := DObject.empty, "additional" := DObject.empty)
          ExpectedStructure.expectedMaybeParent.$set(value)(base).right.value shouldBe DObject("expectedMaybeParent" ::= ("additional" := DObject.empty))
          val value2 = DObject("maybe" := DObject("additional" := DObject.empty))
          ExpectedStructure.expectedMaybeParent.$set(value2)(base).right.value shouldBe DObject("expectedMaybeParent" ::= ("maybe" ::= ("additional" := DObject.empty)))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with IncorrectTypeFailure as EmptyOnIncorrectType is irrelevant") {
            val base = DObject.empty
            val value = DObject("default" := false, "maybe" := 123)
            EmptyExpectedStructure.withoutExpected.$set(value)(base).left.value should contain allOf(
              IncorrectTypeFailure(EmptyExpectedStructure.withoutExpected.default, false),
              IncorrectTypeFailure(EmptyExpectedStructure.withoutExpected.maybe, 123)
            )
          }
        }
      }
      describe("In Expected Path") {
        it("Should create parent object if not present when setting") {
          val base = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("expected" := value))
        }
        it("Should replace parent object if not correct type when setting") {
          val base = DObject("expectedParent" := 1)
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("expected" := value))
        }
        it("Should merge in with existing properties when setting if present") {
          val base = DObject("expectedParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          ExpectedStructure.expectedParent.expected.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("additional" := "value", "expected" := value))
        }
      }
      describe("In Maybe Path") {
        it("Should create parent object if not present when setting") {
          val base = DObject.empty
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("expected" := value))
        }
        it("Should replace parent object if not correct type when setting") {
          val base = DObject("maybeParent" := 1)
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("expected" := value))
        }
        it("Should merge in with existing properties when setting if present") {
          val base = DObject("maybeParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          ExpectedStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("additional" := "value", "expected" := value))
        }
      }
    }
  }

  describe("Maybe Object lens") {
    trait MaybeContract extends SubContract {
      val empty = new \\? with AdditionalProperties

      val withExpected = new \\? with AdditionalProperties {
        val expected = \[String]
        val default = \![Int](1)
        val maybe = \?[String]
      }

      val expectedParent = new \\ with AdditionalProperties {
        val maybe = new \\? with AdditionalProperties {
          val expected = \[String]
          val default = \![Int](1)
          val maybe = \?[String]
        }
        val maybeWithoutExpected = new \\? with AdditionalProperties {
          val default = \![Int](1)
          val maybe = \?[String]
        }
      }
      val maybeParent = new \\? with AdditionalProperties {
        val expected = new \\ with AdditionalProperties {
          val expected = \[String]
          val default = \![Int](1)
          val maybe = \?[String]
        }
      }
    }

    object MaybeStructure extends Contract with MaybeContract

    object EmptyMaybeStructure extends Contract with MaybeContract with EmptyOnIncorrectType

    object ClosedMaybeObject extends Contract {
      val maybeObject = new  \\? {
        val expected = \[String]
        val maybe = \?[Int]
      }
    }

    object OpenMaybeObject extends Contract {
      val maybeObject = new  \\? with AdditionalProperties{
        val expected = \[String]
        val maybe = \?[Int]
      }
    }

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
          MaybeStructure.withExpected.$verify(base) should contain (ExpectedFailure(MaybeStructure.withExpected.expected))
        }
        it("Should return IncorrectTypeFailure for any properties with incorrect types") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := "four", "maybe" := false))
          MaybeStructure.withExpected.$verify(base) should contain allOf (
            IncorrectTypeFailure(MaybeStructure.withExpected.default, "four"),
            IncorrectTypeFailure(MaybeStructure.withExpected.maybe, false)
          )
        }
        describe("With EmptyOnIncorrectType") {
          it("Should returns IncorrectTypeFailure if incorrect type") {
            val base = DObject("empty" := 1)
            EmptyMaybeStructure.empty.$verify(base) should contain (IncorrectTypeFailure(EmptyMaybeStructure.empty, 1))
          }
        }
      }
      describe("In Expected Path") {
        it("Should return empty list expected properties if parent not present") {
          val base = DObject.empty
          MaybeStructure.expectedParent.maybe.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectTypeFailure for parent if parent is incorrect type") {
            val base = DObject("expectedParent" := 1)
            EmptyMaybeStructure.expectedParent.maybe.$verify(base) should contain (IncorrectTypeFailure(EmptyMaybeStructure.expectedParent, 1))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return empty list if parent not present regardless of requirements of Expected properties") {
          val base = DObject.empty
          MaybeStructure.maybeParent.expected.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectTypeFailure for parent if parent is incorrect type") {
            val base = DObject("maybeParent" := 1)
            EmptyMaybeStructure.maybeParent.expected.$verify(base) should contain (IncorrectTypeFailure(EmptyMaybeStructure.maybeParent, 1))
          }
        }
      }
      describe("Closed for additional properties") {
        it("Should return empty list if no additional properties") {
          val base = DObject("maybeObject" ::= ("expected" := "value"))
          ClosedMaybeObject.maybeObject.$verify(base) shouldBe empty
        }
        it("Should return ClosedContractFailure if additional properties") {
          val base = DObject("maybeObject" ::= ("expected" := "value", "additional" := 1))
          ClosedMaybeObject.maybeObject.$verify(base) should contain (ClosedContractFailure(ClosedMaybeObject.maybeObject, "additional"))
        }
        it("Should return ClosedContractFailure if additional properties and nested") {
          val base = DObject("maybeObject" ::= ("expected" := "value", "additional" := 1))
          ClosedMaybeObject.$verify(base) should contain (ClosedContractFailure(ClosedMaybeObject.maybeObject, "additional"))
        }
      }
      describe("Additional properties") {
        it("Should return empty list if additional properties") {
          val base = DObject("maybeObject" ::= ("expected" := "value", "additional" := 1))
          OpenMaybeObject.maybeObject.$verify(base) shouldBe empty
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
          MaybeStructure.empty.$get(base).right.value shouldBe None
        }
        it("Should return object if all expected present and all properties correct type") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := 2, "maybe" := "value"))
          MaybeStructure.withExpected.$get(base).right.value shouldBe Some(DObject("expected" := "value", "default" := 2, "maybe" := "value"))
        }
        it("Should return None for expected properties if not present and object not set") {
          val base = DObject.empty
          MaybeStructure.withExpected.$get(base).right.value shouldBe None
        }
        it("Should return ExpectedFailure for expected properties if present but missing properties") {
          val base = DObject("withExpected" ::= ("default" := 2, "maybe" := "value"))
          MaybeStructure.withExpected.$get(base).left.value should contain (ExpectedFailure(MaybeStructure.withExpected.expected))
        }
        it("Should provide default values if not set") {
          val base = DObject("withExpected" ::= ("expected" := "value", "maybe" := "value"))
          MaybeStructure.withExpected.$get(base).right.value shouldBe Some(DObject("expected" := "value", "default" := 1, "maybe" := "value"))
        }
        it("Should return IncorrectTypeFailure for any properties with incorrect types") {
          val base = DObject("withExpected" ::= ("expected" := "value", "default" := "four", "maybe" := false))
          MaybeStructure.withExpected.$get(base).left.value should contain allOf (
            IncorrectTypeFailure(MaybeStructure.withExpected.default, "four"),
            IncorrectTypeFailure(MaybeStructure.withExpected.maybe, false)
          )
        }
        describe("With EmptyOnIncorrectType") {
          it("Should contain ExpectedFailure for expected properties if incorrect type") {
            val base = DObject("withExpected" := 1)
            EmptyMaybeStructure.withExpected.$get(base).right.value shouldBe None
          }
          it("Should return object excluding maybe if maybe has incorrect type") {
            val base = DObject("withExpected" ::= ("default" := 5, "maybe" := 123, "expected" := "value"))
            EmptyMaybeStructure.withExpected.$get(base).right.value shouldBe Some(DObject("default" := 5, "expected" := "value"))
          }
          it("Should return object with default value applied if default has incorrect type") {
            val base = DObject("withExpected" ::= ("default" := true, "expected" := "value"))
            EmptyMaybeStructure.withExpected.$get(base).right.value shouldBe Some(DObject("default" := 1, "expected" := "value"))
          }
          it("Should contain ExpectedFailure for any expected properties of the wrong type") {
            val base = DObject("withExpected" ::= ("expected" := 123))
            EmptyMaybeStructure.withExpected.$get(base).left.value should contain (ExpectedFailure(EmptyMaybeStructure.withExpected.expected))
          }
        }
      }
      describe("In Expected Path") {
        it("Should return None if empty even with expected properties") {
          val base = DObject.empty
          MaybeStructure.expectedParent.maybe.$get(base).right.value shouldBe None
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return None for maybe parent is incorrect type") {
            val base = DObject("expectedParent" := 1)
            EmptyMaybeStructure.expectedParent.maybe.$get(base).right.value shouldBe None
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return None if parent not present regardless of presents of Expected properties") {
          val base = DObject.empty
          MaybeStructure.maybeParent.expected.$get(base).right.value shouldBe None
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return None if parent is incorrect type") {
            val base = DObject("maybeParent" := 1)
            EmptyMaybeStructure.maybeParent.expected.$get(base).right.value shouldBe None
          }
        }
      }
      describe("Closed for additional properties") {
        it("Should return object if no additional properties") {
          val base = DObject("maybeObject" ::= ("expected" := "value"))
          ClosedMaybeObject.maybeObject.$get(base).right.value shouldBe Some(DObject("expected" := "value"))
        }
        it("Should fail with ClosedContractFailure if additional properties") {
          val base = DObject("maybeObject" ::= ("expected" := "value", "additional" := 1))
          ClosedMaybeObject.maybeObject.$get(base).left.value should contain (ClosedContractFailure(ClosedMaybeObject.maybeObject, "additional"))
        }
        it("Should fail with ClosedContractFailure if additional properties and nested") {
          val base = DObject("maybeObject" ::= ("expected" := "value", "additional" := 1))
          ClosedMaybeObject.$get(base).left.value should contain (ClosedContractFailure(ClosedMaybeObject.maybeObject, "additional"))
        }
      }
      describe("Additional properties") {
        it("Should return object if additional properties") {
          val base = DObject("maybeObject" ::= ("expected" := "value", "additional" := 1))
          OpenMaybeObject.maybeObject.$get(base).right.value shouldBe Some(DObject("expected" := "value", "additional" := 1))
        }
        it("Should return object if additional properties and nested") {
          val base = DObject("maybeObject" ::= ("expected" := "value", "additional" := 1))
          OpenMaybeObject.$get(base).right.value shouldBe base
        }
      }
    }
    describe("$set") {
      describe("No path") {
        it("Should make no changes if setting an empty object property to empty") {
          val base = DObject.empty
          MaybeStructure.empty.$set(DObject.empty)(base).right.value shouldBe base
        }
        it("Should clear out the object if setting an object to empty") {
          val base = DObject("empty" ::= ("field" := "value"))
          MaybeStructure.empty.$set(DObject.empty)(base).right.value shouldBe DObject.empty
        }
        it("Should set a valid object") {
          val base = DObject.empty
          val value = DObject("expected" := "value", "default" := 5, "maybe" := "value")
          MaybeStructure.withExpected.$set(value)(base).right.value shouldBe DObject("withExpected" := value)
        }
        it("Should fail with ExpectedFailure if object doesn't contain expected properties") {
          val base = DObject.empty
          val value = DObject("default" := 4)
          MaybeStructure.withExpected.$set(value)(base).left.value should contain (ExpectedFailure(MaybeStructure.withExpected.expected))
        }
        it("Should fail with IncorrectTypeFailure if object contains incorrect types") {
          val base = DObject.empty
          val value = DObject("default" := false, "maybe" := 123)
          MaybeStructure.withExpected.$set(value)(base).left.value should contain allOf(
            IncorrectTypeFailure(MaybeStructure.withExpected.default, false),
            IncorrectTypeFailure(MaybeStructure.withExpected.maybe, 123)
          )
        }
        it("Should keep default value even if it matches the default property value") {
          val base = DObject.empty
          val value = DObject("default" := 1, "expected" := "value")
          MaybeStructure.withExpected.$set(value)(base).right.value shouldBe DObject("withExpected" := value)
        }
        it("Should removed empty child object property values if verified") {
          val base = DObject.empty
          val value = DObject("maybeWithoutExpected" := DObject.empty)
          MaybeStructure.expectedParent.$set(value)(base).right.value shouldBe base
        }
        it("Should not remove empty other object values if verified") {
          val base = DObject.empty
          val value = DObject("maybeWithoutExpected" := DObject.empty, "additional" := DObject.empty)
          MaybeStructure.expectedParent.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("additional" := DObject.empty))
          val value2 = DObject("maybeWithoutExpected" := DObject("additional" := DObject.empty))
          MaybeStructure.expectedParent.$set(value2)(base).right.value shouldBe DObject("expectedParent" ::= ("maybeWithoutExpected" ::= ("additional" := DObject.empty)))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with IncorrectTypeFailure as EmptyOnIncorrectType is irrelevant") {
            val base = DObject.empty
            val value = DObject("default" := false, "maybe" := 123)
            EmptyMaybeStructure.withExpected.$set(value)(base).left.value should contain allOf(
              IncorrectTypeFailure(EmptyMaybeStructure.withExpected.default, false),
              IncorrectTypeFailure(EmptyMaybeStructure.withExpected.maybe, 123)
            )
          }
        }
      }
      describe("In Expected Path") {
        it("Should create parent object if not present when setting") {
          val base = DObject.empty
          val value = DObject("expected" := "value")
          MaybeStructure.expectedParent.maybe.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("maybe" := value))
        }
        it("Should replace parent object if not correct type when setting") {
          val base = DObject("expectedParent" := 1)
          val value = DObject("expected" := "value")
          MaybeStructure.expectedParent.maybe.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("maybe" := value))
        }
        it("Should merge in with existing properties when setting if present") {
          val base = DObject("expectedParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          MaybeStructure.expectedParent.maybe.$set(value)(base).right.value shouldBe DObject("expectedParent" ::= ("additional" := "value", "maybe" := value))
        }
      }
      describe("In Maybe Path") {
        it("Should create parent object if not present when setting") {
          val base = DObject.empty
          val value = DObject("expected" := "value")
          MaybeStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("expected" := value))
        }
        it("Should replace parent object if not correct type when setting") {
          val base = DObject("maybeParent" := 1)
          val value = DObject("expected" := "value")
          MaybeStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("expected" := value))
        }
        it("Should merge in with existing properties when setting if present") {
          val base = DObject("maybeParent" ::= ("additional" := "value"))
          val value = DObject("expected" := "value")
          MaybeStructure.maybeParent.expected.$set(value)(base).right.value shouldBe DObject("maybeParent" ::= ("additional" := "value", "expected" := value))
        }
      }
      describe("Closed for additional properties") {
        it("Should succeed if no additional properties") {
          val base = DObject.empty
          val value = DObject("expected" := "value")
          ClosedMaybeObject.maybeObject.$set(value)(base).right.value shouldBe DObject("maybeObject" ::= ("expected" := "value"))
        }
        it("Should fail with ClosedContractFailure if additional properties") {
          val base = DObject.empty
          val value = DObject("maybe" := "value", "additional" := 1)
          ClosedMaybeObject.maybeObject.$set(value)(base).left.value should contain (ClosedContractFailure(ClosedMaybeObject.maybeObject, "additional"))
        }
      }
      describe("Additional properties") {
        it("Should succeed if additional properties") {
          val base = DObject.empty
          val value = DObject("maybe" := "value", "additional" := 1)
          OpenMaybeObject.maybeObject.$set(value)(base).right.value shouldBe DObject("maybeObject" ::= ("expected" := "value", "additional" := 1))
        }
      }
    }
  }

  describe("Objects Lens") {

    object ExpectedObjects extends Contract {
      val property = \[String]
    }
    object MaybeObjects extends Contract {
      val property = \?[String]
    }
    object DefaultObjects extends Contract {
      val property = \![String]("default")
    }

    object Objects extends Contract {
      val expectedObjects = \::[DObject](ExpectedObjects)

      val maybeObjects = \::[DObject](MaybeObjects)

      val defaultObjects = \::[DObject](DefaultObjects)
    }

    describe("$get") {
      it("Should return empty vector if not found") {
        val base = DObject.empty
        Objects.expectedObjects.$get(base).right.value shouldBe Vector.empty
      }
      it("Should return empty vector if empty vector") {
        val base = DObject("expectedObjects" := Vector.empty[DObject])
        Objects.expectedObjects.$get(base).right.value shouldBe Vector.empty
      }
      it("Should fail if not a vector") {
        val base = DObject("expectedObjects" := "fail")
        Objects.expectedObjects.$get(base).left.value should contain(IncorrectTypeFailure(Objects.expectedObjects, "fail"))
      }
      it("Should succeed if all objects succeed") {
        val expecteds = Vector(DObject("property" := "test"), DObject("property" := "test2", "additional" := 123))
        val base = DObject("expectedObjects" := expecteds)
        Objects.expectedObjects.$get(base).right.value shouldBe expecteds
      }
      it("Should fail if any object fails") {
        val expecteds = Vector(DObject("property" := "test"), DObject("property" := false), DObject("property" := "test2", "additional" := 123))
        val base = DObject("expectedObjects" := expecteds)
        Objects.expectedObjects.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObjects.property, false).rebase(Objects, Path("expectedObjects", 1)))
        val expecteds2 = Vector(DObject("property" := "test"), DObject.empty, DObject("property" := "test2", "additional" := 123))
        val base2 = DObject("expectedObjects" := expecteds2)
        Objects.expectedObjects.$get(base2).left.value should contain(ExpectedFailure(ExpectedObjects.property).rebase(Objects, Path("expectedObjects", 1)))
      }
      it("Should return with empty object if maybe") {
        val maybes = Vector(DObject("property" := "test"), DObject("property" := "test2", "additional" := 123), DObject.empty)
        val base = DObject("maybeObjects" := maybes)
        Objects.maybeObjects.$get(base).right.value shouldBe maybes
      }

      it("Should include any defaults") {
        val defaults = Vector(DObject.empty, DObject("property" := "test"), DObject("property" := "test2", "additional" := 123))
        val base = DObject("defaultObjects" := defaults)
        Objects.defaultObjects.$get(base).right.value shouldBe Vector(DObject("property" := "default"), DObject("property" := "test"), DObject("property" := "test2", "additional" := 123))
      }
    }
    describe("$clear") {
      it("Should clear even if invalid objects") {
        val expecteds = Vector(DObject("property" := "test"), DObject("property" := false), DObject("property" := "test2", "additional" := 123))
        val base = DObject("expectedObjects" := expecteds)
        Objects.expectedObjects.$clear(base) shouldBe DObject.empty
      }
    }
    describe("$append") {
      it("Should succeed on empty") {
        val base = DObject.empty
        Objects.expectedObjects.$append(DObject("property" := "test"))(base).right.value shouldBe DObject("expectedObjects" := Vector(DObject("property" := "test")))
        Objects.maybeObjects.$append(DObject.empty)(base).right.value shouldBe DObject("maybeObjects" := Vector(DObject.empty))
        Objects.defaultObjects.$append(DObject.empty)(base).right.value shouldBe DObject("defaultObjects" := Vector(DObject.empty))
      }
      it("Should Fail if append if appended object is wrong") {
        val base = DObject.empty
        Objects.expectedObjects.$append(DObject("property" := 123))(base).left.value should contain(IncorrectTypeFailure(ExpectedObjects.property, 123))
      }
      it("Should fail if append if field being appended to is of the wrong type") {
        val base = DObject("expectedObjects" := 1234)
        Objects.expectedObjects.$append(DObject("property" := "value"))(base).left.value should contain(IncorrectTypeFailure(Objects.expectedObjects, 1234))
      }
      it("Should succeed if append if objects being appended to are wrong") {
        val base = DObject("expectedObjects" := Vector(DObject.empty))
        Objects.expectedObjects.$append(DObject("property" := "value"))(base).right.value shouldBe DObject("expectedObjects" := Vector(DObject.empty, DObject("property" := "value")))
      }
      it("Should append expected") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "test1")))
        Objects.expectedObjects.$append(DObject("property" := "test2"))(base).right.value shouldBe DObject("expectedObjects" := Vector(DObject("property" := "test1"), DObject("property" := "test2")))
      }
      it("Should append default") {
        val base = DObject("defaultObjects" := Vector(DObject("property" := "test1")))
        Objects.defaultObjects.$append(DObject.empty)(base).right.value shouldBe DObject("defaultObjects" := Vector(DObject("property" := "test1"), DObject.empty))
      }
    }
    describe("$set") {
      it("Should set empty") {
        val base = DObject.empty
        Objects.expectedObjects.$set(Vector.empty)(base).right.value shouldBe DObject.empty
      }
      it("Should fail if any set objects are invalid") {
        val base = DObject.empty
        val array = Vector(ExpectedObjects.$create(_.property.$set("one")), DObject.empty, ExpectedObjects.$create(_.property.$set("two")))
        Objects.expectedObjects.$set(array)(base).left.value should contain (ExpectedFailure(ExpectedObjects.property).rebase(ExpectedObjects, Path(1)))
      }
      it("Should include any empty objects if allowed") {
        val maybeBase = DObject.empty
        val maybeArray = Vector(MaybeObjects.$create(_.property.$set("one")), DObject.empty, MaybeObjects.$create(_.property.$set("two")))
        Objects.maybeObjects.$set(maybeArray)(maybeBase).right.value shouldBe DObject("maybeObjects" := maybeArray)
      }
      it("Should replace existing collection") {
        val defaultBase = DObject.empty
        val defaultArray = Vector(MaybeObjects.$create(_.property.$set("one")), DObject.empty, MaybeObjects.$create(_.property.$set("two")))
        Objects.defaultObjects.$set(defaultArray)(defaultBase).right.value shouldBe DObject("defaultObjects" := defaultArray)
      }
      it("Should replace existing wrong collection") {
        val base = DObject("expectedObjects" := Vector(123, DObject("property" := "one"), "blah"))
        val array = Vector(ExpectedObjects.$create(_.property.$set("one")), ExpectedObjects.$create(_.property.$set("two")))
        Objects.expectedObjects.$set(array)(base).right.value shouldBe DObject("expectedObjects" := array)
      }
    }
    describe("$map") {
      it("Should succeed if objects are empty") {
        val base = DObject.empty
        Objects.expectedObjects.$map(ExpectedObjects.property.$set("value2"))(base).right.value shouldBe base
      }
      it("Should succeed if objects are valid") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "value"), DObject("property" := "value2")))
        Objects.expectedObjects.$map(ExpectedObjects.property.$modify(s => "_" + s))(base).right.value shouldBe DObject("expectedObjects" := Vector(DObject("property" := "_value"), DObject("property" := "_value2")))
      }
      it("Should fail if any object is invalid") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "value"), DObject("property" := "value2"), DObject.empty))
        Objects.expectedObjects.$map(ExpectedObjects.property.$modify(s => "_" + s))(base).left.value should contain(ExpectedFailure(ExpectedObjects.property).rebase(Objects, Objects.expectedObjects._path \ 2))
      }
      it("Should provide any default values") {
        val base = DObject("defaultObjects" := Vector(DObject("property" := "value1"), DObject("property" := "value2"), DObject.empty))
        Objects.defaultObjects.$map(DefaultObjects.property.$modify(s => "_" + s))(base).right.value shouldBe DObject("defaultObjects" := Vector(DObject("property" := "_value1"), DObject("property" := "_value2"), DObject("property" := "_default")))
      }
    }
  }

  describe("Objects Lens with EmptyOnIncorrectTypes") {

    object EmptyExpectedObjects extends Contract with EmptyOnIncorrectType {
      val property = \[String]
    }
    object EmptyMaybeObjects extends Contract with EmptyOnIncorrectType {
      val property = \?[String]
    }
    object EmptyDefaultObjects extends Contract with EmptyOnIncorrectType {
      val property = \![String]("default")
    }

    object EmptyObjects extends Contract with EmptyOnIncorrectType {
      val maybeObjects = \::[DObject](EmptyMaybeObjects)

      val defaultObjects = \::[DObject](EmptyDefaultObjects)

      val expectedObjects = \::[DObject](EmptyExpectedObjects)
    }

    describe("$get") {
      it("Should return empty if not a vector") {
        val base = DObject("maybeObjects" := 123)
        EmptyObjects.maybeObjects.$get(base).right.value shouldBe Vector.empty
      }
      it("Should ignore a failed element") {
        val base = DObject("maybeObjects" := Vector(DObject("property" := "value"), Data(123)))
        EmptyObjects.maybeObjects.$get(base).right.value shouldBe Vector(DObject("property" := "value"))
      }

      it("Should return with empty object if maybe incorrect type") {
        val base = DObject("maybeObjects" := Vector(DObject("property" := 123)))
        EmptyObjects.maybeObjects.$get(base).right.value shouldBe Vector(DObject.empty)
      }

      it("Should include any defaults if default incorrect type") {
        val base = DObject("defaultObjects" := Vector(DObject("property" := 123)))
        EmptyObjects.defaultObjects.$get(base).right.value shouldBe Vector(DObject("property" := "default"))
      }
    }
    describe("$append") {

      it("Should fail if append if appended object has any optional incorrect types") {
        val base = DObject("maybeObjects" := Vector(DObject("property" := "one")))
        EmptyObjects.maybeObjects.$append(DObject("property" := 123))(base).left.value should contain (IncorrectTypeFailure(EmptyMaybeObjects.property, 123))
      }
      it("Should fail on append if appended object has any expected incorrect types") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "one")))
        EmptyObjects.expectedObjects.$append(DObject("property" := 123))(base).left.value should contain (IncorrectTypeFailure(EmptyExpectedObjects.property, 123))
      }
      it("Should succeed if current collection has any optional incorrect types") {
        val base = DObject("maybeObjects" := Vector(DObject("property" := 123)))
        EmptyObjects.maybeObjects.$append(DObject("property" := "string"))(base).right.value shouldBe DObject("maybeObjects" := Vector(DObject("property" := 123), DObject("property" := "string")))
      }
      it("Should succeed if current is incorrect type") {
        val base = DObject("maybeObjects" := false)
        EmptyObjects.maybeObjects.$append(DObject("property" := "string"))(base).right.value shouldBe DObject("maybeObjects" := Vector(DObject("property" := "string")))
      }
    }
    describe("$set") {
      it("Should fail if any set objects have any optional incorrect types") {
        val base = DObject.empty
        val array = Vector(DObject("property" := "value"), DObject.empty, DObject("property" := 123))
        EmptyObjects.maybeObjects.$set(array)(base).left.value should contain (IncorrectTypeFailure(EmptyMaybeObjects.property, 123).rebase(EmptyMaybeObjects, Path(2)))
      }
    }
    describe("$map") {
      it("Should ignore element if that object is invalid") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "value"), DObject("property" := "value2"), DObject.empty))
        val r = EmptyObjects.expectedObjects.$map(EmptyExpectedObjects.property.$modify(s => "_" + s))(base).right.value
        r shouldBe DObject("expectedObjects" := Vector(DObject("property" := "_value"), DObject("property" := "_value2"), DObject.empty))
      }
      it("Should ignore if array not valid array") {
        val base = DObject("expectedObjects" := 123)
        val r = EmptyObjects.expectedObjects.$map(EmptyExpectedObjects.property.$modify(s => "_" + s))(base).right.value
        r shouldBe base
      }
    }
  }

  describe("MapObjectsPropertyLens") {

    object Expected extends Contract {
      val property = \[String]
    }
    object Maybe extends Contract {
      val property = \?[Int]
    }
    object Default extends Contract {
      val property = \![Boolean](false)
    }

    object MapObjects extends Contract {
      val expectedMap = \->[String, DObject](Expected)
      val maybeMap = \->[String, DObject](Maybe)
      val defaultMap = \->[String, DObject](Default)
    }

    describe("$get for map") {
      it("should return empty map if no value") {
        val base = DObject.empty
        MapObjects.expectedMap.$get(base).right.value shouldBe Map.empty

        val base2 = DObject("defaultMap" := DObject.empty)
        MapObjects.defaultMap.$get(base2).right.value shouldBe Map.empty
      }
      it("Should return map") {
        val base = DObject("maybeMap" := Map("first" -> DObject("property" := 123), "second" -> DObject("property" := 456)))
        MapObjects.maybeMap.$get(base).right.value shouldBe Map("first" -> DObject("property" := 123), "second" -> DObject("property" := 456))
      }
      it("Should fail if map not correct type") {
        val base = DObject("expectedMap" := 123)
        MapObjects.expectedMap.$get(base).left.value should contain (IncorrectTypeFailure(MapObjects.expectedMap, 123))
      }
      it("Should fail if any object fails") {
        val base = DObject("maybeMap" := Map("first" -> DObject("property" := false), "second" -> DObject("property" := 456)))
        MapObjects.maybeMap.$get(base).left.value should contain (IncorrectTypeFailure(Maybe.property, false).rebase(MapObjects, MapObjects.maybeMap._path \ "first"))
      }
      it("Should provide any default values") {
        val base = DObject("defaultMap" := Map("first" -> DObject("additional" := 23)))
        MapObjects.defaultMap.$get(base).right.value shouldBe Map("first" -> DObject("property" := false, "additional" := 23))
      }
    }
    describe("$get for individual object") {
      it("Should return None if no map object") {
        val base = DObject.empty
        MapObjects.expectedMap.$get("first")(base).right.value shouldBe None
      }
      it("Should return None if no key entry") {
        val base = DObject("maybeMap" := DObject("first" := DObject("property" := 456)))
        MapObjects.maybeMap.$get("second")(base).right.value shouldBe None
      }
      it("Should return object if found") {
        val base = DObject("expectedMap" := DObject("first" := DObject("property" := "value1"), "second" := DObject("property" := "value2")))
        MapObjects.expectedMap.$get("second")(base).right.value shouldBe Some(DObject("property" := "value2"))
      }
      it("Should return failure if object fails") {
        val base = DObject("expectedMap" := DObject("first" := DObject.empty, "second" := DObject("property" := "value2")))
        MapObjects.expectedMap.$get("first")(base).left.value should contain (ExpectedFailure(Expected.property).rebase(MapObjects, MapObjects.expectedMap._path \ "first"))
      }
      it("Should return object with defaults") {
        val base = DObject("defaultMap" := DObject("first" := DObject.empty, "second" := DObject("property" := true)))
        MapObjects.defaultMap.$get("first")(base).right.value shouldBe Some(DObject("property" := false))
      }
    }
    describe("$exists") {
      it("Should return false if empty") {
        val base = DObject.empty
        MapObjects.expectedMap.$exists("first")(base).right.value shouldBe false
      }
      it("Should return true if key present") {
        val base = DObject("maybeMap" := DObject("first" := DObject("property" := 456), "second" := DObject("property" := 456)))
        MapObjects.maybeMap.$exists("second")(base).right.value shouldBe true
      }
      it("Should return true even if object referenced fails") {
        val base = DObject("defaultMap" := DObject("first" := DObject("property" := false), "second" := DObject("property" := 456)))
        MapObjects.defaultMap.$exists("second")(base).right.value shouldBe true
      }
    }
    describe("$set") {
      //Setting empty could remove?
      it("Should clear if set empty object") {
        val base = DObject.empty
        MapObjects.expectedMap.$set(Map.empty)(base).right.value shouldBe DObject.empty
      }
      it("Should set correct map values") {
        val base = DObject.empty
        val set = Map("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value2"))
        MapObjects.expectedMap.$set(set)(base).right.value shouldBe DObject("expectedMap" := set)
      }
      it("Should fail if any objects being set fail") {
        val base = DObject.empty
        val set = Map("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 1234))
        MapObjects.expectedMap.$set(set)(base).left.value should contain (IncorrectTypeFailure(Expected.property, 1234).rebase(Expected, Path("second")))
      }
      it("Should replace map if map is incorrect type") {
        val base = DObject("maybeMap" := DObject("first" := DObject("property" := 456), "second" := DObject("property" := false)))
        val set = Map("three" -> DObject("property" := 423))
        MapObjects.maybeMap.$set(set)(base).right.value shouldBe DObject("maybeMap" := DObject("three" := DObject("property" := 423)))
      }
      it("Should replace map with failed objects") {
        val base = DObject("maybeMap" := "fail")
        val set = Map("three" -> DObject("property" := 423))
        MapObjects.maybeMap.$set(set)(base).right.value shouldBe DObject("maybeMap" := DObject("three" := DObject("property" := 423)))
      }
      it("Should include any empty objects if allowed") {
        val base = DObject.empty
        val set = Map("first" -> DObject("property" := 2), "second" -> DObject("property" := 3), "third" -> DObject.empty)
        MapObjects.maybeMap.$set(set)(base).right.value shouldBe DObject("maybeMap" := DObject("first" := DObject("property" := 2), "second" := DObject("property" := 3), "third" := DObject.empty) )
      }
    }
    describe("$clear") {
      it("Should clear if empty") {
        val base = DObject.empty
        MapObjects.defaultMap.$clear(base) shouldBe DObject.empty
      }
      it("Should clear if object map present") {
        val base = DObject("maybeMap" := DObject("first" := DObject("property" := "value1")), "defaultMap" :=Map("first" -> DObject("property" := false)) )
        MapObjects.defaultMap.$clear(base) shouldBe DObject("maybeMap" := DObject("first" := DObject("property" := "value1")))
      }
      it("Should clear if maps type is invalid") {
        val base = DObject("maybeMap" := false, "defaultMap" :=Map("first" -> DObject("property" := false)))
        MapObjects.maybeMap.$clear(base) shouldBe DObject("defaultMap" := DObject("first" := DObject("property" := false)))
      }
    }
    describe("$remove") {
      it("Should return if object empty") {
        val base = DObject.empty
        MapObjects.defaultMap.$remove("first")(base).right.value shouldBe DObject.empty
      }
      it("Should return of key not found") {
        val base = DObject("expectedMap" := DObject("first" := DObject("property" := "value1"), "second" := DObject("property" := "value2")))
        MapObjects.expectedMap.$remove("third")(base).right.value shouldBe base
      }
      it("Should remove key object if found") {
        val base = DObject("expectedMap" := DObject("first" := DObject("property" := "value1"), "second" := DObject("property" := "value2")))
        MapObjects.expectedMap.$remove("second")(base).right.value shouldBe DObject("expectedMap" := DObject("first" := DObject("property" := "value1")))
      }
      it("Should remove key object even if object fails") {
        val base = DObject("defaultMap" := DObject("first" := DObject("property" := false), "second" := DObject("property" := 456)))
        MapObjects.defaultMap.$remove("first")(base).right.value shouldBe DObject("defaultMap" := DObject("second" := DObject("property" := 456)))
      }
      it("Should fail if maps not correct type") {
        val base = DObject("expectedMap" := 123)
        MapObjects.expectedMap.$remove("first")(base).left.value should contain (IncorrectTypeFailure(MapObjects.expectedMap, 123))
      }
      it("Should clear field if last element removed") {

      }
    }
    describe("$add") {
      it("Should create single element map if no map object") {
        val base = DObject.empty
        val r = MapObjects.defaultMap.$add("first" -> Default.$create(_.property.$set(false)))(base).right.value
        r shouldBe DObject("defaultMap" ::= "first" -> DObject("property" := false))
      }
      it("Should fail if map not correct type") {
        val base = DObject("expectedMap" := 123)
        val r = MapObjects.expectedMap.$add("first" -> Expected.$create(_.property.$set("value")))(base).left.value
        r should contain (IncorrectTypeFailure(MapObjects.expectedMap, 123))
      }
      it("Should add if map exists") {
        val base = DObject("expectedMap" := Map("first" -> Expected.$create(_.property.$set("value1"))))
        val r = MapObjects.expectedMap.$add("second" -> Expected.$create(_.property.$set("value2")))(base).right.value
        r shouldBe DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value2")))
      }
      it("Should add if map objects not all valid") {
        val base = DObject("maybeMap" := Map("first" -> Expected.$create(_.property.$set("value1"))))
        val r = MapObjects.maybeMap.$add("second" -> Maybe.$create(_.property.$set(456)))(base).right.value
        r shouldBe DObject("maybeMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 456)))
      }
      it("Should fail if added object fails") {
        val base = DObject.empty
        val r = MapObjects.defaultMap.$add("first" ::= ("property" := 123))(base).left.value
        r should contain (IncorrectTypeFailure(Default.property, 123))
      }
      it("Should replace object if object already exists") {
        val base = DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value2")))
        val r = MapObjects.expectedMap.$add("second" -> Expected.$create(_.property.$set("value3")))(base).right.value
        r shouldBe DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value3")))
      }
    }
    describe("$map") {
      it("Should succeed if objects are empty") {
        val base = DObject.empty
        val r = MapObjects.expectedMap.$map(Expected.property.$modify(_ + "1"))(base).right.value
        r shouldBe DObject.empty
      }
      it("Should succeed if objects are valid") {
        val base = DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value2")))
        val r = MapObjects.expectedMap.$map(Expected.property.$modify(_ + "*"))(base).right.value
        r shouldBe DObject("expectedMap" ::= ("first" -> DObject("property" := "value1*"), "second" -> DObject("property" := "value2*")))
      }
      it("Should fail if any object is invalid") {
        val base = DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 123)))
        val r = MapObjects.expectedMap.$map(Expected.property.$modify(_ + "*"))(base).left.value
        r should contain (IncorrectTypeFailure(Expected.property, 123).rebase(MapObjects, MapObjects.expectedMap._path \ "second"))
      }
      it("Should provide any default values") {
        val base = DObject("defaultMap" ::= ("first" -> DObject("property" := true), "second" -> DObject.empty))
        val r = MapObjects.defaultMap.$map(a => a + ("property2" := 123))(base).right.value
        r shouldBe DObject("defaultMap" ::= ("first" -> DObject("property" := true, "property2" := 123), "second" -> DObject("property" := false, "property2" := 123)))
      }
    }
  }

  describe("MapObjectsPropertyLens with EmptyOnIncorrectType") {

    object EmptyExpected extends Contract with EmptyOnIncorrectType {
      val property = \[String]
    }
    object EmptyMaybe extends Contract with EmptyOnIncorrectType {
      val property = \?[Int]
    }
    object EmptyDefault extends Contract with EmptyOnIncorrectType {
      val property = \![Boolean](false)
    }

    object EmptyMapObjects extends Contract with EmptyOnIncorrectType {
      val expectedMap = \->[String, DObject](EmptyExpected)
      val maybeMap = \->[String, DObject](EmptyMaybe)
      val defaultMap = \->[String, DObject](EmptyDefault)
    }

    describe("$get for map") {
      it("Should return Empty if map not correct type") {
        val base = DObject("expectedMap" := 123)
        EmptyMapObjects.expectedMap.$get(base).right.value shouldBe Map.empty
      }
      it("Should drop key value if the object fails") {
        val base = DObject("maybeMap" := Map("first" -> DObject("property" := 321), "second" := 345))
        EmptyMapObjects.maybeMap.$get(base).right.value shouldBe Map("first" -> DObject("property" := 321))
      }
    }
    describe("$get for individual object") {
      it("Should empty failure if object fails") {
        val base = DObject("expectedMap" := DObject("first" := DObject.empty, "second" := DObject("property" := "value2")))
        EmptyMapObjects.expectedMap.$get("first")(base).right.value shouldBe None
        val base2 = DObject("maybeMap" := DObject("first" := DObject("property" := "value"), "second" := 123))
        EmptyMapObjects.expectedMap.$get("second")(base2).right.value shouldBe None
      }
    }
    describe("$set") {
      it("Should fail if any objects being set fail") {
        val base = DObject.empty
        val set = Map("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 1234))
        EmptyMapObjects.expectedMap.$set(set)(base).left.value should contain (IncorrectTypeFailure(EmptyExpected.property, 1234).rebase(EmptyExpected, Path("second")))
      }

    }
    describe("$remove") {
      it("Should ignore if maps not correct type") {
        val base = DObject("expectedMap" := 123)
        EmptyMapObjects.expectedMap.$remove("first")(base).right.value shouldBe base
      }
    }
    describe("$add") {
      it("Should create map of one object if map not correct type") {
        val base = DObject("expectedMap" := 123)
        val r = EmptyMapObjects.expectedMap.$add("first" -> EmptyExpected.$create(_.property.$set("value")))(base).right.value
        r shouldBe DObject("expectedMap" := Map("first" ::= ("property" := "value")))
      }
      it("Should fail if added object fails") {
        val base = DObject.empty
        val r = EmptyMapObjects.defaultMap.$add("first" ::= ("property" := 123))(base).left.value
        r should contain (IncorrectTypeFailure(EmptyDefault.property, 123))
      }
    }
    describe("$map") {
      it("Should ignore any object that are invalid") {
        val base = DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 123)))
        val r = EmptyMapObjects.expectedMap.$map(EmptyExpected.property.$modify(_ + "*"))(base).right.value
        r shouldBe DObject("expectedMap" ::= ("first" -> DObject("property" := "value1*"), "second" -> DObject("property" := 123)))
      }
      it("Should ignore if map value is not a valid map") {
        val base = DObject("expectedMap" := 1234)
        val r = EmptyMapObjects.expectedMap.$map(EmptyExpected.property.$modify(_ + "*"))(base).right.value
        r shouldBe base
      }
    }
  }
}