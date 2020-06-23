package dsentric.contracts

import dsentric._
import dsentric.failure.{ExpectedFailure, IncorrectTypeFailure}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

@deprecated("Update when EitherValues is updated and undeprecate.", "")
class PropertyLensSpec extends AnyFunSpec with Matchers with EitherValues {

  import Dsentric._
  import PessimisticCodecs._
  import dsentric.Implicits._
  

  object ExpectedStructure extends Contract {
    val copy = \[String]
    val maybeCopied = \?[String]
    val defaultCopied = \![String]("default")
    val nulled = \[DNullable[Int]]
    val field = \[String]
    val expected = new \\ {
      val field = \[String]
      val default = \![String]("default")
    }
    val maybe = new \\? {
      val field = \[String]
      val default = \![String]("default")
    }
  }

  object EmptyExpectedStructure extends Contract with EmptyOnIncorrectType {
    val field = \[String]
    val copy = \[String]
    val defaultCopied = \![String]("default")
    val maybeCopied = \?[String]
    val expected = new \\ {
      val field = \[String]
      val default = \![String]("default")
    }
    val maybe = new \\? {
      val field = \[String]
      val default = \![String]("default")
    }
  }

  describe("Expected Lens") {
    describe("$verify") {
      describe("No Path") {
        it("Should return empty list if valid value and valid values in path") {
          val base = DObject("field" := "value")
          ExpectedStructure.field.$verify(base) shouldBe Nil
        }
        it("Should return IncorrectTypeFailure failure if wrong type") {
          val base = DObject("field" := 1234)
          ExpectedStructure.field.$verify(base) should contain(IncorrectTypeFailure(ExpectedStructure.field, 1234))
        }
        it("Should return IncorrectTypeFailure failure if null value") {
          val base = DObject("field" := DNull)
          ExpectedStructure.field.$verify(base) should contain(IncorrectTypeFailure(ExpectedStructure.field, DNull))
        }
        it("Should return ExpectedFailure if value not found") {
          val base = DObject.empty
          ExpectedStructure.field.$verify(base) should contain(ExpectedFailure(ExpectedStructure.field))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectTypeFailure if incorrect type") {
            val base = DObject("field" := "value", "copy" := 1234)
            EmptyExpectedStructure.copy.$verify(base) should contain(IncorrectTypeFailure(EmptyExpectedStructure.copy, 1234))
          }
        }
      }
      describe("In Expected Path") {
        it("Should return empty if nested value has correct type") {
          val base = DObject("expected" ::= ("field" := "value"))
          ExpectedStructure.expected.field.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          ExpectedStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(ExpectedStructure.expected.field, 1234))
        }
        it("Should return IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          ExpectedStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(ExpectedStructure.expected, false))
        }
        it("Should return ExpectedFailure for the property if value not found") {
          val base = DObject("expected" := DObject.empty)
          ExpectedStructure.expected.field.$verify(base) should contain(ExpectedFailure(ExpectedStructure.expected.field))
        }
        it("Should return ExpectedFailure for the parent if the Expected parent object is not found") {
          val base = DObject.empty
          ExpectedStructure.expected.field.$verify(base) should contain(ExpectedFailure(ExpectedStructure.expected.field))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectType for the parent if the parent is the wrong type") {
            val base = DObject("expected" := 1)
            EmptyExpectedStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(EmptyExpectedStructure.expected, 1))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return empty if nested value has correct type") {
          val base = DObject("maybe" ::= ("field" := "value"))
          ExpectedStructure.maybe.field.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure if wrong type for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          ExpectedStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(ExpectedStructure.maybe.field, 1234))
        }
        it("Should return IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          ExpectedStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(ExpectedStructure.maybe, false))
        }
        it("Should return ExpectedFailure for the property if value not found") {
          val base = DObject("maybe" := DObject.empty)
          ExpectedStructure.maybe.field.$verify(base) should contain(ExpectedFailure(ExpectedStructure.maybe.field))
        }
        it("Should return empty list for the property if the Maybe parent object is not found") {
          val base = DObject.empty
          ExpectedStructure.maybe.field.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectType for the parent if the parent is the wrong type") {
            val base = DObject("maybe" := 1)
            EmptyExpectedStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(EmptyExpectedStructure.maybe, 1))
          }
        }
      }

    }
    describe("$get") {
      describe("No Path") {
        it("Should fail with ExpectedFailure if empty value") {
          ExpectedStructure.field.$get(DObject.empty).left.value should contain(ExpectedFailure(ExpectedStructure.field))
        }
        it("Should fail with IncorrectTypeFailure if null value") {
          ExpectedStructure.field.$get(DObject("field" := DNull)).left.value should contain(IncorrectTypeFailure(ExpectedStructure.field, DNull))
        }
        it("Should fail with IncorrectTypeFailure if value is of the wrong type") {
          ExpectedStructure.field.$get(DObject("field" := false)).left.value should contain(IncorrectTypeFailure(ExpectedStructure.field, false))
        }
        it("Should return value if set with correct type") {
          ExpectedStructure.field.$get(DObject("field" := "test")).right.value shouldBe Some("test")
        }
        it("Should return Type failure if nullable empty") {
          ExpectedStructure.nulled.$get(DObject.empty).left.value should contain(ExpectedFailure(ExpectedStructure.nulled))
        }
        it("Should fail with IncorrectType if nullable wrong type") {
          ExpectedStructure.nulled.$get(DObject("nulled" := "wrong")).left.value should contain(IncorrectTypeFailure(ExpectedStructure.nulled, "wrong"))
        }
        it("Should return null if nullable null") {
          ExpectedStructure.nulled.$get(DObject("nulled" := DNull)).right.value shouldBe Some(DNull)
        }
        it("Should return DSome Value if nullable set") {
          ExpectedStructure.nulled.$get(DObject("nulled" := 123)).right.value shouldBe Some(DSome(123))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure if null value") {
            EmptyExpectedStructure.field.$get(DObject("field" := DNull)).left.value should contain(ExpectedFailure(EmptyExpectedStructure.field))
          }
          it("Should fail with ExpectedFailure if wrong type") {
            EmptyExpectedStructure.field.$get(DObject("field" := false)).left.value should contain(ExpectedFailure(EmptyExpectedStructure.field))
          }
        }
      }
      describe("In Expected Path") {
        it("Should return value if exists with correct type") {
          val base = DObject("expected" ::= ("field" := "value"))
          ExpectedStructure.expected.field.$get(base).right.value shouldBe Some("value")
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          ExpectedStructure.expected.field.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.expected.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          ExpectedStructure.expected.field.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.expected, false))
        }
        it("Should fail with ExpectedFailure for the property if value not found") {
          val base = DObject("expected" := DObject.empty)
          ExpectedStructure.expected.field.$get(base).left.value should contain(ExpectedFailure(ExpectedStructure.expected.field))
        }
        it("Should fail with ExpectedFailure for the property if the Expected parent object is not found") {
          val base = DObject.empty
          ExpectedStructure.expected.field.$get(base).left.value should contain(ExpectedFailure(ExpectedStructure.expected.field))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure for the property if the parent is the wrong type and typeBehaviour is empty") {
            val base = DObject("expected" := 1)
            EmptyExpectedStructure.expected.field.$get(base).left.value should contain(ExpectedFailure(EmptyExpectedStructure.expected.field))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return value if exists with correct type") {
          val base = DObject("maybe" ::= ("field" := "value"))
          ExpectedStructure.maybe.field.$get(base).right.value shouldBe Some("value")
        }
        it("Should fail with IncorrectTypeFailure if wrong type in for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          ExpectedStructure.maybe.field.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.maybe.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          ExpectedStructure.maybe.field.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.maybe, false))
        }
        it("Should fail with ExpectedFailure for the property if value not found") {
          val base = DObject("maybe" := DObject.empty)
          ExpectedStructure.maybe.field.$get(base).left.value should contain(ExpectedFailure(ExpectedStructure.maybe.field))
        }
        it("Should return None for the property if the Maybe parent object is not found") {
          val base = DObject.empty
          ExpectedStructure.maybe.field.$get(base).right.value shouldBe None
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return Nonefor the property if the parent is the wrong type and typeBehaviour is empty") {
            val base = DObject("maybe" := 1)
            EmptyExpectedStructure.maybe.field.$get(base).right.value shouldBe None
          }
        }
      }
    }
    describe("$set") {
      describe("No Path") {
        it("should set an empty field") {
          val base = DObject.empty
          ExpectedStructure.field.$set("test")(base) should be(DObject("field" := "test"))
        }
        it("Should replace an existing fields value") {
          val base = DObject("field" := "test")
          ExpectedStructure.field.$set("test2")(base) should be(DObject("field" := "test2"))
        }
        it("Should replace a set field of the wrong type") {
          val base = DObject("field" := false)
          ExpectedStructure.field.$set("test")(base) should be(DObject("field" := "test"))
        }
        it("Should set null value if nullable") {
          val base = DObject.empty
          ExpectedStructure.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set value if nullable") {
          val base = DObject.empty
          ExpectedStructure.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set value if nullable over a null value") {
          val base = DObject("nulled" := DNull)
          ExpectedStructure.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set null over value ") {
          val base = DObject("nulled" := 123)
          ExpectedStructure.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
        }
      }
      describe("In Expected Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          ExpectedStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("expected" ::= ("field" := "value"))
          ExpectedStructure.expected.field.$set("value2")(base) shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("expected" := 1)
          ExpectedStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("expected" ::= ("field2" := 23))
          ExpectedStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value", "field2" := 23))
        }
      }
      describe("In Maybe Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          ExpectedStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("maybe" ::= ("field" := "value"))
          ExpectedStructure.maybe.field.$set("value2")(base) shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("maybe" := 1)
          ExpectedStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("maybe" ::= ("field2" := 23))
          ExpectedStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value", "field2" := 23))
        }
      }

    }
    describe("$maybeSet") {
      describe("No Path") {
        it("Should not change nothing if not set") {
          val base = DObject.empty
          ExpectedStructure.field.$maybeSet(None)(base) shouldBe DObject.empty
        }
        it("Should not alter a value if not set") {
          val base = DObject("field" := "test")
          ExpectedStructure.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not alter a value if not set and value of wrong type") {
          val base = DObject("field" := 123)
          ExpectedStructure.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should alter a value if empty") {
          val base = DObject.empty
          ExpectedStructure.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
        }
        it("Should alter a value if set") {
          val base = DObject("field" := "test")
          ExpectedStructure.field.$maybeSet(Some("test2"))(base) shouldBe DObject("field" := "test2")
        }
        it("Should alter if value of wrong type") {
          val base = DObject("field" := 123)
          ExpectedStructure.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
        }
        it("Should set null value if nullable") {
          val base = DObject.empty
          ExpectedStructure.nulled.$maybeSet(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set value if nullable") {
          val base = DObject.empty
          ExpectedStructure.nulled.$maybeSet(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should not set nullable if empty") {
          val base = DObject.empty
          ExpectedStructure.nulled.$maybeSet(None)(base) shouldBe base
        }
      }
      describe("In Expected Path") {
        it("Should not create the nested field if internal is not set") {
          val base = DObject.empty
          ExpectedStructure.expected.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          ExpectedStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should create the nested field if internal is altered") {
          val base = DObject("expected" ::= ("field" := "value"))
          ExpectedStructure.expected.field.$maybeSet(Some("value2"))(base) shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should replace the nested field if it is of wrong type") {
          val base = DObject("expected" := 1)
          ExpectedStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should not replace the nested field if it is of wrong type and the value is empty") {
          val base = DObject("expected" := 1)
          ExpectedStructure.expected.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("expected" ::= ("field2" := 23))
          ExpectedStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value", "field2" := 23))
        }
      }
      describe("In Maybe Path") {
        it("Should not create the nested field if internal is not set") {
          val base = DObject.empty
          ExpectedStructure.maybe.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          ExpectedStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should create the nested field if internal is altered") {
          val base = DObject("maybe" ::= ("field" := "value"))
          ExpectedStructure.maybe.field.$maybeSet(Some("value2"))(base) shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should replace the nested field if it is of wrong type") {
          val base = DObject("maybe" := 1)
          ExpectedStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should not replace the nested field if it is of wrong type and the value is empty") {
          val base = DObject("maybe" := 1)
          ExpectedStructure.maybe.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("maybe" ::= ("field2" := 23))
          ExpectedStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value", "field2" := 23))
        }
      }

    }
    describe("$modify") {
      describe("No Path") {
        it("Should fail with  with ExpectedFailure when modifying on not found") {
          val base = DObject.empty
          ExpectedStructure.field.$modify(_ + "2")(base).left.value should contain(ExpectedFailure(ExpectedStructure.field))
        }
        it("Should fail with IncorrectTypeFailure when modifying on incorrect type") {
          val base = DObject("field" := 123)
          ExpectedStructure.field.$modify(_ + "2")(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.field, 123))
        }
        it("Should fail with IncorrectTypeFailure when modifying on unexpected null type") {
          val base = DObject("field" := DNull)
          ExpectedStructure.field.$modify(_ + "2")(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.field, DNull))
        }
        it("Should modify a set value") {
          val base = DObject("field" := "value")
          ExpectedStructure.field.$modify(_ + "2")(base).right.value shouldBe DObject("field" := "value2")
        }
        it("Should modify a null value") {
          val base = DObject("nulled" := DNull)
          ExpectedStructure.nulled.$modify { case DNull => DSome(0); case DSome(x) => DSome(x + 1) }(base).right.value shouldBe DObject("nulled" := 0)
          val base2 = DObject("nulled" := 123)
          ExpectedStructure.nulled.$modify { case DNull => DSome(0); case DSome(x) => DSome(x + 1) }(base2).right.value shouldBe DObject("nulled" := 124)
          val base3 = DObject.empty
          ExpectedStructure.nulled.$modify { case DNull => DSome(0); case DSome(x) => DSome(x + 1) }(base3).left.value should contain(ExpectedFailure(ExpectedStructure.nulled))
          val base4 = DObject("nulled" := "wrong")
          ExpectedStructure.nulled.$modify { case DNull => DSome(0); case DSome(x) => DSome(x + 1) }(base4).left.value should contain(IncorrectTypeFailure(ExpectedStructure.nulled, "wrong"))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure with modifying on empty") {
            val base = DObject.empty
            EmptyExpectedStructure.field.$modify(_ + "2")(base).left.value should contain(ExpectedFailure(EmptyExpectedStructure.field))
          }
          it("Should fail with ExpectedFailure when modifying on incorrect type") {
            val base = DObject("field" := 123)
            EmptyExpectedStructure.field.$modify(_ + "2")(base).left.value should contain(ExpectedFailure(EmptyExpectedStructure.field))
          }
          it("Should fail with ExpectedFailure when modifying unexpected null type") {
            val base = DObject("field" := DNull)
            EmptyExpectedStructure.field.$modify(_ + "2")(base).left.value should contain(ExpectedFailure(EmptyExpectedStructure.field))
          }
        }
      }
      describe("In Expected Path") {
        it("Should modify a nested value") {
          val base = DObject("expected" ::= ("field" := "value"))
          ExpectedStructure.expected.field.$modify(_ + "2")(base).right.value shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          ExpectedStructure.expected.field.$modify(_ + "2")(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.expected.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          ExpectedStructure.expected.field.$modify(_ + "2")(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.expected, false))
        }
        it("Should fail with ExpectedFailure if nested value not found") {
          val base = DObject("expected" ::= ("field2" := "value"))
          ExpectedStructure.expected.field.$modify(_ + "2")(base).left.value should contain(ExpectedFailure(ExpectedStructure.expected.field))
        }
        it("Should fail with ExpectedFailure for property if parent is not found") {
          val base = DObject.empty
          ExpectedStructure.expected.field.$modify(_ + "2")(base).left.value should contain(ExpectedFailure(ExpectedStructure.expected.field))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure for property if parent is wrong type") {
            val base = DObject("expected" := false)
            EmptyExpectedStructure.expected.field.$modify(_ + "2")(base).left.value should contain(ExpectedFailure(EmptyExpectedStructure.expected.field))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should modify a nested value") {
          val base = DObject("maybe" ::= ("field" := "value"))
          ExpectedStructure.maybe.field.$modify(_ + "2")(base).right.value shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          ExpectedStructure.maybe.field.$modify(_ + "2")(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.maybe.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          ExpectedStructure.maybe.field.$modify(_ + "2")(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.maybe, false))
        }
        it("Should fail with ExpectedFailure if nested value not found") {
          val base = DObject("maybe" ::= ("field2" := "value"))
          ExpectedStructure.maybe.field.$modify(_ + "2")(base).left.value should contain(ExpectedFailure(ExpectedStructure.maybe.field))
        }
        it("Should do nothing for property if parent is not found") {
          val base = DObject.empty
          ExpectedStructure.maybe.field.$modify(_ + "2")(base).right.value shouldBe base
        }
        describe("With EmptyOnIncorrectType") {
          it("Should do nothing if parent is wrong type and typeBehaviour is empty") {
            val base = DObject("maybe" := false)
            EmptyExpectedStructure.maybe.field.$modify(_ + "2")(base).right.value shouldBe base
          }
        }
      }
    }
    describe("$copy") {
      describe("No Path") {
        it("Should fail with ExpectedFailure if copying from an empty expected property") {
          val base = DObject.empty
          ExpectedStructure.copy.$copy(ExpectedStructure.field)(base).left.value should contain(ExpectedFailure(ExpectedStructure.field))
        }
        it("Should do nothing if copying from an empty Maybe property") {
          val base = DObject("copy" := "leave")
          ExpectedStructure.copy.$copy(ExpectedStructure.maybeCopied)(base).right.value shouldBe base
        }
        it("Should fail with IncorrectTypeFailure if copying an incorrect type") {
          val base1 = DObject("field" := 123)
          ExpectedStructure.copy.$copy(ExpectedStructure.field)(base1).left.value should contain(IncorrectTypeFailure(ExpectedStructure.field, 123))
          val base2 = DObject("copy" := "leave", "field" := 123)
          ExpectedStructure.copy.$copy(ExpectedStructure.field)(base2).left.value should contain(IncorrectTypeFailure(ExpectedStructure.field, 123))
        }
        it("Should fail with IncorrectTypeFailure if copying an unexpected Null value") {
          val base1 = DObject("field" := DNull)
          ExpectedStructure.copy.$copy(ExpectedStructure.field)(base1).left.value should contain(IncorrectTypeFailure(ExpectedStructure.field, DNull))
          val base2 = DObject("copy" := "leave", "field" := DNull)
          ExpectedStructure.copy.$copy(ExpectedStructure.field)(base2).left.value should contain(IncorrectTypeFailure(ExpectedStructure.field, DNull))
        }
        it("Should create the property when copying to an unset property") {
          val base = DObject("field" := "copyMe")
          ExpectedStructure.copy.$copy(ExpectedStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should override value when copying to an already set property") {
          val base = DObject("field" := "copyMe", "copy" := "replaceMe")
          ExpectedStructure.copy.$copy(ExpectedStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should override value when copying to an already set property of the wrong type")  {
          val base = DObject("field" := "copyMe", "copy" := 123)
          ExpectedStructure.copy.$copy(ExpectedStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should copy a maybe field when set with a value") {
          val base = DObject("maybeCopied" := "test2")
          ExpectedStructure.copy.$copy(ExpectedStructure.maybeCopied)(base).right.value shouldBe DObject("maybeCopied" := "test2", "copy" := "test2")
        }
        it("Should copy default value when Copying an empty default property") {
          val base = DObject.empty
          ExpectedStructure.copy.$copy(ExpectedStructure.defaultCopied)(base).right.value shouldBe DObject("copy" := "default")
        }
        it("Should copy a set value from a default property if set") {
          val base = DObject("defaultCopied" := "notDefault")
          ExpectedStructure.copy.$copy(ExpectedStructure.defaultCopied)(base).right.value shouldBe DObject("defaultCopied" := "notDefault", "copy" := "notDefault")
        }
        it("Should fail with IncorrectType when Copying a default value set as the wrong type") {
          val base = DObject("defaultCopied" := 1234)
          ExpectedStructure.copy.$copy(ExpectedStructure.defaultCopied)(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.defaultCopied, 1234))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure if copying from an empty expected property") {
            val base = DObject.empty
            EmptyExpectedStructure.copy.$copy(EmptyExpectedStructure.field)(base).left.value should contain(ExpectedFailure(EmptyExpectedStructure.field))
          }
          it("Should fail with ExpectedFailure if copying from an expected property with incorrect type") {
            val base = DObject("field" := false)
            EmptyExpectedStructure.copy.$copy(EmptyExpectedStructure.field)(base).left.value should contain(ExpectedFailure(EmptyExpectedStructure.field))
          }
          it("Should do nothing if copying from a maybe field with incorrect type") {
            val base = DObject("maybeCopied" := false)
            EmptyExpectedStructure.copy.$copy(EmptyExpectedStructure.maybeCopied)(base).right.value shouldBe base
          }
          it("Copying a default value set as the wrong type should set default value") {
            val base = DObject("defaultCopied" := 1234)
            EmptyExpectedStructure.copy.$copy(EmptyExpectedStructure.defaultCopied)(base).right.value shouldBe DObject("defaultCopied" := 1234, "copy" := "default")
          }
        }
      }
      describe("In Expected Path") {
        it("Should fail with ExpectedFailure if copying from path and parent not found") {
          val base = DObject.empty
          ExpectedStructure.field.$copy(ExpectedStructure.expected.field)(base).left.value should contain(ExpectedFailure(ExpectedStructure.expected.field))
        }
        it("Should fail with ExpectedFailure if copying from path and property not found") {
          val base = DObject("expected" ::= ("field2" := 2))
          ExpectedStructure.field.$copy(ExpectedStructure.expected.field)(base).left.value should contain(ExpectedFailure(ExpectedStructure.expected.field))
        }
        it("Should fail with IncorrectType when parent is incorrect type") {
          val base = DObject("expected" := false)
          ExpectedStructure.field.$copy(ExpectedStructure.expected.field)(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.expected, false))
        }
        it("Should create path when copying") {
          val base = DObject("field" := "value")
          ExpectedStructure.expected.field.$copy(ExpectedStructure.field)(base).right.value shouldBe DObject("field" := "value", "expected" ::= ("field" := "value"))
        }
        it("Should replace path when copying if parent of incorrect type") {
          val base = DObject("field" := "value", "expected" := 1)
          ExpectedStructure.expected.field.$copy(ExpectedStructure.field)(base).right.value shouldBe DObject("field" := "value", "expected" ::= ("field" := "value"))
        }
        it("Should copy the default value in an expected path if the parent doesnt exist") {
          val base = DObject.empty
          ExpectedStructure.field.$copy(ExpectedStructure.expected.default)(base).right.value shouldBe DObject("field" := "default")
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure if copying from parent with incorrect type") {
            val base = DObject("expected" := false)
            EmptyExpectedStructure.field.$copy(EmptyExpectedStructure.expected.field)(base).left.value should contain(ExpectedFailure(EmptyExpectedStructure.expected.field))
          }
          it("Should copy the default value in an expected path if copying from parent with incorrect type") {
            val base =  DObject("expected" := false)
            EmptyExpectedStructure.field.$copy(EmptyExpectedStructure.expected.default)(base).right.value shouldBe DObject("expected" := false, "field" := "default")
          }
        }
      }
      describe("In MaybePath") {
        it("Should do nothing if copying from path and parent not found") {
          val base = DObject.empty
          ExpectedStructure.field.$copy(ExpectedStructure.maybe.field)(base).right.value shouldBe base
        }
        it("Should fail with ExpectedFailure if copying from an path and property not found") {
          val base = DObject("maybe" ::= ("field2" := 2))
          ExpectedStructure.field.$copy(ExpectedStructure.maybe.field)(base).left.value should contain(ExpectedFailure(ExpectedStructure.maybe.field))
        }
        it("Should fail with IncorrectType when parent is incorrect type") {
          val base = DObject("maybe" := false)
          ExpectedStructure.field.$copy(ExpectedStructure.maybe.field)(base).left.value should contain(IncorrectTypeFailure(ExpectedStructure.maybe, false))
        }
        it("Should create path when copying") {
          val base = DObject("field" := "value")
          ExpectedStructure.maybe.field.$copy(ExpectedStructure.field)(base).right.value shouldBe DObject("field" := "value", "maybe" ::= ("field" := "value"))
        }
        it("Should replace path when copying if parent of incorrect type") {
          val base = DObject("field" := "value", "maybe" := 1)
          ExpectedStructure.maybe.field.$copy(ExpectedStructure.field)(base).right.value shouldBe DObject("field" := "value", "maybe" ::= ("field" := "value"))
        }
        it("Should do nothing if copying the  default value in an maybe path if the parent doesnt exist") {
          val base = DObject.empty
          ExpectedStructure.field.$copy(ExpectedStructure.maybe.default)(base).right.value shouldBe DObject.empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should do nothing if copying when parent is incorrect type") {
            val base = DObject("maybe" := false)
            EmptyExpectedStructure.field.$copy(EmptyExpectedStructure.maybe.field)(base).right.value shouldBe base
          }
          it("Should do nothing if copying default from parent with incorrect type") {
            val base = DObject("expected" := false)
            EmptyExpectedStructure.field.$copy(EmptyExpectedStructure.maybe.default)(base).right.value shouldBe DObject("expected" := false)
          }
        }
      }
    }
  }
  
  object MaybeStructure extends Contract {
    val field = \?[String]
    val copy = \?[String]
    val expectedCopied = \[String]
    val defaultCopied = \![String]("default")
    val nulled = \?[DNullable[Int]]
    val expected = new \\ {
      val field = \?[String]
      val default = \![String]("default")
      val expected = \[String]
    }
    val maybe = new \\? {
      val field = \?[String]
      val default = \![String]("default")
      val expected = \[String]
    }
  }

  object EmptyMaybeStructure extends Contract with EmptyOnIncorrectType {
    val field = \?[String]
    val copy = \?[String]
    val expectedCopied = \[String]
    val defaultCopied = \![String]("default")

    val nulled = \?[DNullable[Int]]

    val expected = new \\ {
      val field = \?[String]
      val default = \![String]("default")
      val expected = \[String]
    }
    val maybe = new \\? {
      val field = \?[String]
      val default = \![String]("default")
      val expected = \[String]
    }
  }

  describe("Maybe Lens") {
    describe("$verify") {
      describe("No Path") {
        it("Should return empty if valid value and valid values in path") {
          val base = DObject("field" := "value")
          MaybeStructure.field.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure failure if wrong type") {
          val base = DObject("field" := 1234)
          MaybeStructure.field.$verify(base) should contain(IncorrectTypeFailure(MaybeStructure.field, 1234))
        }
        it("Should return IncorrectTypeFailure failure if null value") {
          val base = DObject("field" := DNull)
          MaybeStructure.field.$verify(base) should contain(IncorrectTypeFailure(MaybeStructure.field, DNull))
        }
        it("Should return empty if value not found") {
          val base = DObject.empty
          MaybeStructure.field.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectTypeFailure if incorrect type") {
            val base = DObject("field" := "value", "copy" := 1234)
            EmptyMaybeStructure.copy.$verify(base) should contain(IncorrectTypeFailure(EmptyMaybeStructure.copy, 1234))
          }
        }
      }
      describe("In Expected Path") {
        it("Should return empty if nested value has correct type") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          MaybeStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(MaybeStructure.expected.field, 1234))
        }
        it("Should return IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          MaybeStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(MaybeStructure.expected, false))
        }
        it("Should return empty for the property if value not found") {
          val base = DObject("expected" := DObject.empty)
          MaybeStructure.expected.field.$verify(base) shouldBe empty
        }
        it("Should return empty if the parent object is not found") {
          val base = DObject.empty
          MaybeStructure.expected.field.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectType for the parent if the parent is the wrong type") {
            val base = DObject("expected" := 1)
            EmptyMaybeStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(EmptyMaybeStructure.expected, 1))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return empty if nested value has correct type") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure if wrong type for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          MaybeStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(MaybeStructure.maybe.field, 1234))
        }
        it("Should return IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          MaybeStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(MaybeStructure.maybe, false))
        }
        it("Should return empty list if value not found") {
          val base = DObject("maybe" := DObject.empty)
          MaybeStructure.maybe.field.$verify(base) shouldBe empty
        }
        it("Should return empty list for the property if the Maybe parent object is not found") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectType for the parent if the parent is the wrong type") {
            val base = DObject("maybe" := 1)
            EmptyMaybeStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(EmptyMaybeStructure.maybe, 1))
          }
        }
      }
    }
    describe("$get") {
      describe("No Path") {
        it("Should return None if empty value") {
          val base = DObject.empty
          MaybeStructure.field.$get(base).right.value shouldBe None
        }
        it("Should fail with IncorrectTypeFailure if null value") {
          val base = DObject("field" := DNull)
          MaybeStructure.field.$get(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, DNull))
        }
        it("Should fail with IncorrectTypeFailure if value is of the wrong type") {
          val base = DObject("field" := false)
          MaybeStructure.field.$get(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, false))
        }
        it("Should return value if set with correct type") {
          val base = DObject("field" := "test")
          MaybeStructure.field.$get(base).right.value shouldBe Some("test")
        }
        it("Should return None if nullable empty") {
          val base = DObject.empty
          MaybeStructure.nulled.$get(base).right.value shouldBe None
        }
        it("Should fail with IncorrectType if nullable wrong type") {
          val base = DObject("nulled" := "wrong")
          MaybeStructure.nulled.$get(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.nulled, "wrong"))
        }
        it("Should return null if nullable null") {
          val base = DObject("nulled" := DNull)
          MaybeStructure.nulled.$get(base).right.value shouldBe Some(DNull)
        }
        it("Should return DSome Value if nullable set") {
          val base = DObject("nulled" := 123)
          MaybeStructure.nulled.$get(base).right.value shouldBe Some(DSome(123))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return None if unexpected null value") {
            val base = DObject("field" := DNull)
            EmptyMaybeStructure.field.$get(base).right.value shouldBe None
          }
          it("Should return None if wrong type") {
            val base = DObject("field" := false)
            EmptyMaybeStructure.field.$get(base).right.value shouldBe None
          }
        }
      }
      describe("In Expected Path") {
        it("Should return value if exists with correct type") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$get(base).right.value shouldBe Some("value")
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          MaybeStructure.expected.field.$get(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.expected.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          MaybeStructure.expected.field.$get(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.expected, false))
        }
        it("Should return None if property value not found") {
          val base = DObject("expected" := DObject.empty)
          MaybeStructure.expected.field.$get(base).right.value shouldBe None
        }
        it("Should return None if the Expected parent object is not found") {
          val base = DObject.empty
          MaybeStructure.expected.field.$get(base).right.value shouldBe None
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return None for the property if the parent is the wrong type and typeBehaviour is empty") {
            val base = DObject("expected" := 1)
            EmptyMaybeStructure.expected.field.$get(base).right.value shouldBe None
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return value if exists with correct type") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$get(base).right.value shouldBe Some("value")
        }
        it("Should fail with IncorrectTypeFailure if wrong type in for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          MaybeStructure.maybe.field.$get(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          MaybeStructure.maybe.field.$get(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe, false))
        }
        it("Should return None for the property if value not found") {
          val base = DObject("maybe" := DObject.empty)
          MaybeStructure.maybe.field.$get(base).right.value shouldBe None
        }
        it("Should return None for the property if the Maybe parent object is not found") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$get(base).right.value shouldBe None
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return None for the property if the parent is the wrong type and typeBehaviour is empty") {
            val base = DObject("maybe" := 1)
            EmptyMaybeStructure.maybe.field.$get(base).right.value shouldBe None
          }
        }
      }
    }
    describe("$getOrElse") {
      describe("No Path") {
        it("Should return orElse if empty value") {
          val base = DObject.empty
          MaybeStructure.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
        }
        it("Should fail with IncorrectTypeFailure if null value") {
          val base = DObject("field" := DNull)
          MaybeStructure.field.$getOrElse(base, "orElse").left.value should contain(IncorrectTypeFailure(MaybeStructure.field, DNull))
        }
        it("Should fail with IncorrectTypeFailure if value is of the wrong type") {
          val base = DObject("field" := false)
          MaybeStructure.field.$getOrElse(base, "orElse").left.value should contain(IncorrectTypeFailure(MaybeStructure.field, false))
        }
        it("Should return value if set with correct type") {
          val base = DObject("field" := "test")
          MaybeStructure.field.$getOrElse(base, "orElse").right.value shouldBe "test"
        }
        it("Should return orElse if nullable empty") {
          val base = DObject.empty
          MaybeStructure.nulled.$getOrElse(base, DSome(123)).right.value shouldBe DSome(123)
        }
        it("Should fail with IncorrectType if nullable wrong type") {
          val base = DObject("nulled" := "wrong")
          MaybeStructure.nulled.$getOrElse(base, DSome(123)).left.value should contain(IncorrectTypeFailure(MaybeStructure.nulled, "wrong"))
        }
        it("Should return null if nullable null") {
          val base = DObject("nulled" := DNull)
          MaybeStructure.nulled.$getOrElse(base, DSome(123)).right.value shouldBe DNull
        }
        it("Should return DSome Value if nullable set") {
          val base = DObject("nulled" := 123)
          MaybeStructure.nulled.$getOrElse(base, DSome(456)).right.value shouldBe DSome(123)
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return orElse if unexpected null value") {
            val base = DObject("field" := DNull)
            EmptyMaybeStructure.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
          }
          it("Should return orElse if wrong type") {
            val base = DObject("field" := false)
            EmptyMaybeStructure.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
          }
        }
      }
      describe("In Expected Path") {
        it("Should return value if exists with correct type") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$getOrElse(base, "orElse").right.value shouldBe "value"
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          MaybeStructure.expected.field.$getOrElse(base, "orElse").left.value should contain(IncorrectTypeFailure(MaybeStructure.expected.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          MaybeStructure.expected.field.$getOrElse(base, "orElse").left.value should contain(IncorrectTypeFailure(MaybeStructure.expected, false))
        }
        it("Should return orElse if property value not found") {
          val base = DObject("expected" := DObject.empty)
          MaybeStructure.expected.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
        }
        it("Should return orElse if the Expected parent object is not found") {
          val base = DObject.empty
          MaybeStructure.expected.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return orElse for the property if the parent is the wrong type and typeBehaviour is empty") {
            val base = DObject("expected" := 1)
            EmptyMaybeStructure.expected.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return value if exists with correct type") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$getOrElse(base, "orElse").right.value shouldBe "value"
        }
        it("Should fail with IncorrectTypeFailure if wrong type in for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          MaybeStructure.maybe.field.$getOrElse(base, "orElse").left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          MaybeStructure.maybe.field.$getOrElse(base, "orElse").left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe, false))
        }
        it("Should return orElse for the property if value not found") {
          val base = DObject("maybe" := DObject.empty)
          MaybeStructure.maybe.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
        }
        it("Should return orElse for the property if the Maybe parent object is not found") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return orElse for the property if the parent is the wrong type and typeBehaviour is empty") {
            val base = DObject("maybe" := 1)
            EmptyMaybeStructure.maybe.field.$getOrElse(base, "orElse").right.value shouldBe "orElse"
          }
        }
      }
    }
    describe("$set") {
      describe("No Path") {
        it("should set an empty field") {
          val base = DObject.empty
          MaybeStructure.field.$set("test")(base) should be(DObject("field" := "test"))
        }
        it("Should replace an existing fields value") {
          val base = DObject("field" := "test")
          MaybeStructure.field.$set("test2")(base) should be(DObject("field" := "test2"))
        }
        it("Should replace a set field of the wrong type") {
          val base = DObject("field" := false)
          MaybeStructure.field.$set("test")(base) should be(DObject("field" := "test"))
        }
        it("Should set null value if nullable") {
          val base = DObject.empty
          MaybeStructure.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set value if nullable") {
          val base = DObject.empty
          MaybeStructure.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set value if nullable over a null value") {
          val base = DObject("nulled" := DNull)
          MaybeStructure.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set null over value ") {
          val base = DObject("nulled" := 123)
          MaybeStructure.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
        }
      }
      describe("In Expected Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          MaybeStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$set("value2")(base) shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("expected" := 1)
          MaybeStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("expected" ::= ("field2" := 23))
          MaybeStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value", "field2" := 23))
        }
      }
      describe("In Maybe Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$set("value2")(base) shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("maybe" := 1)
          MaybeStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("maybe" ::= ("field2" := 23))
          MaybeStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value", "field2" := 23))
        }
      }
    }
    describe("$maybeSet") {
      describe("No Path") {
        it("Should not change nothing if not set") {
          val base = DObject.empty
          MaybeStructure.field.$maybeSet(None)(base) shouldBe DObject.empty
        }
        it("Should not alter a value if not set") {
          val base = DObject("field" := "test")
          MaybeStructure.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not alter a value if not set and value of wrong type") {
          val base = DObject("field" := 123)
          MaybeStructure.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should alter a value if empty") {
          val base = DObject.empty
          MaybeStructure.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
        }
        it("Should alter a value if set") {
          val base = DObject("field" := "test")
          MaybeStructure.field.$maybeSet(Some("test2"))(base) shouldBe DObject("field" := "test2")
        }
        it("Should alter if value of wrong type") {
          val base = DObject("field" := 123)
          MaybeStructure.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
        }
        it("Should set null value if nullable") {
          val base = DObject.empty
          MaybeStructure.nulled.$maybeSet(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set value if nullable") {
          val base = DObject.empty
          MaybeStructure.nulled.$maybeSet(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should not set nullable if empty") {
          val base = DObject.empty
          MaybeStructure.nulled.$maybeSet(None)(base) shouldBe base
        }
      }
      describe("In Expected Path") {
        it("Should not create the nested field if internal is not set") {
          val base = DObject.empty
          MaybeStructure.expected.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          MaybeStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should create the nested field if internal is altered") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$maybeSet(Some("value2"))(base) shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should replace the nested field if it is of wrong type") {
          val base = DObject("expected" := 1)
          MaybeStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should not replace the nested field if it is of wrong type and the value is empty") {
          val base = DObject("expected" := 1)
          MaybeStructure.expected.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("expected" ::= ("field2" := 23))
          MaybeStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value", "field2" := 23))
        }
      }
      describe("In Maybe Path") {
        it("Should not create the nested field if internal is not set") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should create the nested field if internal is altered") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$maybeSet(Some("value2"))(base) shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should replace the nested field if it is of wrong type") {
          val base = DObject("maybe" := 1)
          MaybeStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should not replace the nested field if it is of wrong type and the value is empty") {
          val base = DObject("maybe" := 1)
          MaybeStructure.maybe.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("maybe" ::= ("field2" := 23))
          MaybeStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value", "field2" := 23))
        }
      }
    }
    describe("$drop") {
      describe("No path") {
        it("Should do nothing if dropped field not present") {
          val base = DObject("leave" := 123)
          MaybeStructure.field.$drop(base) shouldBe base
        }
        it("Should drop value if present") {
          val base = DObject("field" := "value")
          MaybeStructure.field.$drop(base) shouldBe DObject.empty
        }
        it("Should drop value if wrong type") {
          val base = DObject("field" := 123)
          MaybeStructure.field.$drop(base) shouldBe DObject.empty
        }
        it("Should drop value if nulled") {
          val base = DObject("nulled" := DNull, "leave" := 123)
          MaybeStructure.nulled.$drop(base) shouldBe DObject("leave" := 123)
        }
      }
      describe("In Expected Path") {
        it("Should drop value in path") {
          val base = DObject("expected" ::= ("field" := "value", "field2" := "value2"))
          MaybeStructure.expected.field.$drop(base) shouldBe DObject("expected" ::= ("field2" := "value2"))
        }
        it("Should clear path if drop is only element in object") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$drop(base) shouldBe DObject.empty
        }
      }
      describe("In Maybe Path") {
        it("Should drop value in path") {
          val base = DObject("maybe" ::= ("field" := "value", "field2" := "value2"))
          MaybeStructure.maybe.field.$drop(base) shouldBe DObject("maybe" ::= ("field2" := "value2"))
        }
        it("Should clear path if drop is only element in object") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$drop(base) shouldBe DObject.empty
        }
      }
    }
    describe("$setOrDrop") {
      describe("No path") {
        it("Should set an empty field") {
          val base = DObject.empty
          MaybeStructure.field.$setOrDrop(Some("test"))(base) should be(DObject("field" := "test"))
        }
        it("Should replace a set field") {
          val base = DObject("field" := "test")
          MaybeStructure.field.$setOrDrop(Some("test2"))(base) should be(DObject("field" := "test2"))
        }
        it("Should replace a set field of the wrong type") {
          val base = DObject("field" := false)
          MaybeStructure.field.$setOrDrop(Some("test"))(base) should be(DObject("field" := "test"))
        }
        it("Should set null value if nullable") {
          val base = DObject.empty
          MaybeStructure.nulled.$setOrDrop(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set value if nullable") {
          val base = DObject.empty
          MaybeStructure.nulled.$setOrDrop(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set value if nullable over a null value") {
          val base = DObject("nulled" := DNull)
          MaybeStructure.nulled.$setOrDrop(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set null over value ") {
          val base = DObject("nulled" := 123)
          MaybeStructure.nulled.$setOrDrop(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should not change if dropped field not present") {
          val base = DObject("leave" := 123)
          MaybeStructure.field.$setOrDrop(None)(base) shouldBe base
        }
        it("Should drop value if present") {
          val base = DObject("field" := "value")
          MaybeStructure.field.$setOrDrop(None)(base) shouldBe DObject.empty
        }
        it("Should drop value if wrong type") {
          val base = DObject("field" := 123)
          MaybeStructure.field.$setOrDrop(None)(base) shouldBe DObject.empty
        }
        it("Should drop value if nulled") {
          val base = DObject("nulled" := DNull, "leave" := 123)
          MaybeStructure.nulled.$setOrDrop(None)(base) shouldBe DObject("leave" := 123)
        }
      }
      describe("In Expected Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          MaybeStructure.expected.field.$setOrDrop(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$setOrDrop(Some("value2"))(base) shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("expected" := 1)
          MaybeStructure.expected.field.$setOrDrop(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("expected" ::= ("field2" := 23))
          MaybeStructure.expected.field.$setOrDrop(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value", "field2" := 23))
        }
        it("Should drop value in path") {
          val base = DObject("expected" ::= ("field" := "value", "field2" := "value2"))
          MaybeStructure.expected.field.$setOrDrop(None)(base) shouldBe DObject("expected" ::= ("field2" := "value2"))
        }
        it("Should clear path if drop is only element in object") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$setOrDrop(None)(base) shouldBe DObject.empty
        }
      }
      describe("In Maybe Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$setOrDrop(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$setOrDrop(Some("value2"))(base) shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("maybe" := 1)
          MaybeStructure.maybe.field.$setOrDrop(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("maybe" ::= ("field2" := 23))
          MaybeStructure.maybe.field.$setOrDrop(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value", "field2" := 23))
        }
        it("Should drop value in path") {
          val base = DObject("maybe" ::= ("field" := "value", "field2" := "value2"))
          MaybeStructure.maybe.field.$setOrDrop(None)(base) shouldBe DObject("maybe" ::= ("field2" := "value2"))
        }
        it("Should clear path if drop is only element in object") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$setOrDrop(None)(base) shouldBe DObject.empty
        }
      }
    }
    describe("$modify") {
      val pf:Function[Option[String], String] = {
        case None => "wasEmpty"
        case Some(t) => t + " found"
      }
      describe("No Path") {
        it("Should modify empty value") {
          val base = DObject.empty
          MaybeStructure.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
        }
        it("Should modify a set value") {
          val base = DObject("field" := "value")
          MaybeStructure.field.$modify(pf)(base).right.value shouldBe DObject("field" := "value found")
        }
        it("Should fail with IncorrectTypeFailure if wrong type") {
          val base = DObject("field" := 123)
          MaybeStructure.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, 123))
        }
        it("Should fail with IncorrectTypeFailure if incorrect null type") {
          val base = DObject("field" := DNull)
          MaybeStructure.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, DNull))
        }
        it("Should modify a null value") {
          val pfN: Function[Option[DNullable[Int]], DNullable[Int]] = {
            case None => DNull
            case Some(DNull) => DSome(0)
            case Some(DSome(x)) => DSome(x + 1)
          }
          val base = DObject("nulled" := DNull)
          MaybeStructure.nulled.$modify(pfN)(base).right.value shouldBe DObject("nulled" := 0)
          val base2 = DObject("nulled" := 123)
          MaybeStructure.nulled.$modify(pfN)(base2).right.value shouldBe DObject("nulled" := 124)
          val base3 = DObject.empty
          MaybeStructure.nulled.$modify(pfN)(base3).right.value shouldBe DObject("nulled" := DNull)
        }
        describe("With EmptyOnIncorrectType") {
          it("Should treat as empty when null is incorrect type") {
            val base = DObject("field" := DNull)
            EmptyMaybeStructure.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
          }
          it("Should treat as empty a wrong type ") {
            val base = DObject("field" := 123)
            EmptyMaybeStructure.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
          }
        }
      }
      describe("In Expected Path") {
        it("Should modify a nested value") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "value found"))
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          MaybeStructure.expected.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.expected.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          MaybeStructure.expected.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.expected, false))
        }
        it("Should modify structure if nested value not found") {
          val base = DObject("expected" ::= ("field2" := "value"))
          MaybeStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field2" := "value", "field" := "wasEmpty"))
        }
        it("Should create structure if parent not found") {
          val base = DObject.empty
          MaybeStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "wasEmpty"))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should treat as empty if nested property is wrong type") {
            val base = DObject("expected" ::= ("field" := 1234))
            EmptyMaybeStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "wasEmpty"))
          }
          it("Should treat as empty if parent is wrong type and create structure") {
            val base = DObject("expected" := 123.23)
            EmptyMaybeStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "wasEmpty"))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should modify a nested value") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$modify(pf)(base).right.value shouldBe DObject("maybe" ::= ("field" := "value found"))
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          MaybeStructure.maybe.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          MaybeStructure.maybe.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe, false))
        }
        it("Should modify structure if nested value not found") {
          val base = DObject("maybe" ::= ("field2" := "value"))
          MaybeStructure.maybe.field.$modify(pf)(base).right.value shouldBe DObject("maybe" ::= ("field2" := "value", "field" := "wasEmpty"))
        }
        it("Should do nothing if parent not found") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$modify(pf)(base).right.value shouldBe base
        }
        describe("With EmptyOnIncorrectType") {
          it("Should treat as empty if nested property is wrong type") {
            val base = DObject("maybe" ::= ("field" := 1234))
            EmptyMaybeStructure.maybe.field.$modify(pf)(base).right.value shouldBe DObject("maybe" ::= ("field" := "wasEmpty"))
          }
          it("Should treat as empty if parent is wrong type and do nothing") {
            val base = DObject("maybe" := 123.23)
            EmptyMaybeStructure.maybe.field.$modify(pf)(base).right.value shouldBe base
          }
        }
      }
    }
    describe("$modifyOrDrop") {
      val pf: Function[Option[String], Option[String]] = {
        case None => Some("wasEmpty")
        case Some("drop") => None
        case Some(t) => Some(t + " found")
      }
      describe("No path") {
        it("Should modify empty value") {
          val base = DObject.empty
          MaybeStructure.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
        }
        it("Should modify a set value") {
          val base = DObject("field" := "value")
          MaybeStructure.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("field" := "value found")
        }
        it("Should drop on a corresponding value") {
          val base = DObject("field" := "drop")
          MaybeStructure.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject.empty
        }
        it("Should fail with IncorrectTypeFailure on a wrong type") {
          val base = DObject("field" := 123)
          MaybeStructure.field.$modifyOrDrop(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, 123))
        }
        it("Should fail with IncorrectTypeFailure on a wrong null type") {
          val base = DObject("field" := DNull)
          MaybeStructure.field.$modifyOrDrop(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, DNull))
        }
        it("Should modify a null value") {
          val pfN: Function[Option[DNullable[Int]], Option[DNullable[Int]]] = {
            case None => Some(DNull)
            case Some(DNull) => Some(DSome(0))
            case Some(DSome(0)) => None
            case Some(DSome(x)) => Some(DSome(x + 1))
          }

          val base = DObject("nulled" := DNull)
          MaybeStructure.nulled.$modifyOrDrop(pfN)(base).right.value shouldBe DObject("nulled" := 0)
          val base2 = DObject("nulled" := 123)
          MaybeStructure.nulled.$modifyOrDrop(pfN)(base2).right.value shouldBe DObject("nulled" := 124)
          val base3 = DObject.empty
          MaybeStructure.nulled.$modifyOrDrop(pfN)(base3).right.value shouldBe DObject("nulled" := DNull)
          val base4 = DObject("nulled" := 0)
          MaybeStructure.nulled.$modifyOrDrop(pfN)(base4).right.value shouldBe DObject.empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should treat as empty a wrong type") {
            val base = DObject("field" := 123)
            EmptyMaybeStructure.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
          }
          it("Should treat as empty a wrong null") {
            val base = DObject("field" := DNull)
            EmptyMaybeStructure.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
          }
        }
      }
      describe("In Expected Path") {
        it("Should modify a nested value") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "value found"))
        }
        it("Should drop a nested value") {
          val base = DObject("expected" ::= ("field" := "drop", "field2" := "value"))
          MaybeStructure.expected.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("expected" ::= ("field2" := "value"))
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          MaybeStructure.expected.field.$modifyOrDrop(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.expected.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          MaybeStructure.expected.field.$modifyOrDrop(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.expected, false))
        }
        it("Should modify structure if nested value not found") {
          val base = DObject("expected" ::= ("field2" := "value"))
          MaybeStructure.expected.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("expected" ::= ("field2" := "value", "field" := "wasEmpty"))
        }
        it("Should create structure if parent not found") {
          val base = DObject.empty
          MaybeStructure.expected.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "wasEmpty"))
        }
        it("Should remove structure if dropped") {
          val base = DObject("expected" ::= ("field" := "drop"))
          MaybeStructure.expected.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject.empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should treat as empty if nested property is wrong type") {
            val base = DObject("expected" ::= ("field" := 1234))
            EmptyMaybeStructure.expected.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "wasEmpty"))
          }
          it("Should treat as empty if parent is wrong type and create structure") {
            val base = DObject("expected" := 123.23)
            EmptyMaybeStructure.expected.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "wasEmpty"))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should modify a nested value") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("maybe" ::= ("field" := "value found"))
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          MaybeStructure.maybe.field.$modifyOrDrop(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          MaybeStructure.maybe.field.$modifyOrDrop(pf)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe, false))
        }
        it("Should modify structure if nested value not found") {
          val base = DObject("maybe" ::= ("field2" := "value"))
          MaybeStructure.maybe.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("maybe" ::= ("field2" := "value", "field" := "wasEmpty"))
        }
        it("Should do nothing if parent not found") {
          val base = DObject.empty
          MaybeStructure.maybe.field.$modifyOrDrop(pf)(base).right.value shouldBe base
        }
        it("Should remove structure if dropped") {
          val base = DObject("maybe" ::= ("field" := "drop"))
          MaybeStructure.maybe.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject.empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should treat as empty if nested property is wrong type") {
            val base = DObject("maybe" ::= ("field" := 1234))
            EmptyMaybeStructure.maybe.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("maybe" ::= ("field" := "wasEmpty"))
          }
          it("Should treat as empty if parent is wrong type and do nothing") {
            val base = DObject("maybe" := 123.23)
            EmptyMaybeStructure.maybe.field.$modifyOrDrop(pf)(base).right.value shouldBe base
          }
        }
      }
    }
    describe("$copy") {
      describe("No Path") {
        it("Should copy maybe empty to empty") {
          val base = DObject.empty
          MaybeStructure.copy.$copy(MaybeStructure.field)(base).right.value shouldBe base
        }
        it("Copying an empty value to a set value will set value to empty") {
          val base = DObject("copy" := "leave")
          MaybeStructure.copy.$copy(MaybeStructure.field)(base).right.value shouldBe DObject.empty
        }
        it("Should fail with IncorrectTypeFailure if copying an incorrect type") {
          val base1 = DObject("field" := 123)
          MaybeStructure.copy.$copy(MaybeStructure.field)(base1).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, 123))
          val base2 = DObject("copy" := "leave", "field" := 123)
          MaybeStructure.copy.$copy(MaybeStructure.field)(base2).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, 123))
        }
        it("Should fail with IncorrectTypeFailure if copying an unexpected Null value") {
          val base1 = DObject("field" := DNull)
          MaybeStructure.copy.$copy(MaybeStructure.field)(base1).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, DNull))
          val base2 = DObject("copy" := "leave", "field" := DNull)
          MaybeStructure.copy.$copy(MaybeStructure.field)(base2).left.value should contain(IncorrectTypeFailure(MaybeStructure.field, DNull))
        }
        it("Should create the property when copying to an unset property") {
          val base = DObject("field" := "copyMe")
          MaybeStructure.copy.$copy(MaybeStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should override value when copying to an already set property") {
          val base = DObject("field" := "copyMe", "copy" := "replaceMe")
          MaybeStructure.copy.$copy(MaybeStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should override value when copying to an already set property of the wrong type") {
          val base = DObject("field" := "copyMe", "copy" := 123)
          MaybeStructure.copy.$copy(MaybeStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should Copy an expected value") {
          val base = DObject("expectedCopied" := "test2")
          MaybeStructure.copy.$copy(MaybeStructure.expectedCopied)(base).right.value shouldBe DObject("expectedCopied" := "test2", "copy" := "test2")
        }
        it("Should fail with ExpectedFailure if copying empty expected value") {
          val base = DObject.empty
          MaybeStructure.copy.$copy(MaybeStructure.expectedCopied)(base).left.value should contain(ExpectedFailure(MaybeStructure.expectedCopied))
        }
        it("Should fail with IncorrectTypeFailure if copying an expected value with incorrect type") {
          val base = DObject("expectedCopied" := 123)
          MaybeStructure.copy.$copy(MaybeStructure.expectedCopied)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.expectedCopied, 123))
        }
        it("Should copy a default value when default value is not set") {
          val base = DObject.empty
          MaybeStructure.copy.$copy(MaybeStructure.defaultCopied)(base).right.value shouldBe DObject("copy" := "default")
        }
        it("Should copying a set default value when default value is set") {
          val base = DObject("defaultCopied" := "notDefault")
          MaybeStructure.copy.$copy(MaybeStructure.defaultCopied)(base).right.value shouldBe DObject("defaultCopied" := "notDefault", "copy" := "notDefault")
        }
        it("Should fail with IncorrectTypeFailure if copying a default value set as the wrong type") {
          val base = DObject("defaultCopied" := 1234)
          MaybeStructure.copy.$copy(MaybeStructure.defaultCopied)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.defaultCopied, 1234))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure if copying from an empty expected property") {
            val base = DObject.empty
            EmptyMaybeStructure.copy.$copy(EmptyMaybeStructure.expectedCopied)(base).left.value should contain(ExpectedFailure(EmptyMaybeStructure.expectedCopied))
          }
          it("Should fail with ExpectedFailure if copying from an expected property with incorrect type") {
            val base = DObject("expectedCopied" := false)
            EmptyMaybeStructure.copy.$copy(EmptyMaybeStructure.expectedCopied)(base).left.value should contain(ExpectedFailure(EmptyMaybeStructure.expectedCopied))
          }
          it("Should replace with empty if copying from a maybe field with incorrect type") {
            val base = DObject("copy":= "value", "maybeCopied" := false)
            EmptyMaybeStructure.copy.$copy(EmptyExpectedStructure.maybeCopied)(base).right.value shouldBe DObject("maybeCopied" := false)
          }
          it("Copying a default value set as the wrong type should set default value") {
            val base = DObject("defaultCopied" := 1234)
            EmptyMaybeStructure.copy.$copy(EmptyMaybeStructure.defaultCopied)(base).right.value shouldBe DObject("defaultCopied" := 1234, "copy" := "default")
          }
        }
      }
      describe("In Expected Path") {
        it("Should apply empty if copying from maybe field in Expected path and parent is empty") {
          val base = DObject("field" := "value")
          MaybeStructure.field.$copy(MaybeStructure.expected.field)(base).right.value shouldBe DObject.empty
        }
        it("Should fail with ExpectedFailure if copying from Expected path and expected property not found") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.field.$copy(MaybeStructure.expected.expected)(base).left.value should contain(ExpectedFailure(MaybeStructure.expected.expected))
        }
        it("Should fail with IncorrectType when parent is incorrect type") {
          val base = DObject("expected" := false)
          MaybeStructure.field.$copy(MaybeStructure.expected.field)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.expected, false))
        }
        it("Should create path when copying") {
          val base = DObject("field" := "value")
          MaybeStructure.expected.field.$copy(MaybeStructure.field)(base).right.value shouldBe DObject("field" := "value", "expected" ::= ("field" := "value"))
        }
        it("Should remove path if copying removes final property") {
          val base = DObject("expected" ::= ("field" := "value"))
          MaybeStructure.expected.field.$copy(MaybeStructure.field)(base).right.value shouldBe DObject.empty
        }
        it("Should replace path when copying if parent of incorrect type") {
          val base = DObject("field" := "value", "expected" := 1)
          MaybeStructure.expected.field.$copy(MaybeStructure.field)(base).right.value shouldBe DObject("field" := "value", "expected" ::= ("field" := "value"))
        }
        it("Should copy the default value in an expected path if the parent doesnt exist") {
          val base = DObject.empty
          MaybeStructure.field.$copy(MaybeStructure.expected.default)(base).right.value shouldBe DObject("field" := "default")
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure if copying expected from parent with incorrect type") {
            val base = DObject("expected" := false)
            EmptyMaybeStructure.field.$copy(EmptyMaybeStructure.expected.expected)(base).left.value should contain(ExpectedFailure(EmptyMaybeStructure.expected.expected))
          }
          it("Should copy empty if copying maybe from parent with incorrect type") {
            val base = DObject("field" := "value", "expected" := false)
            EmptyMaybeStructure.field.$copy(EmptyMaybeStructure.expected.field)(base).right.value shouldBe DObject("expected" := false)
          }
          it("Should copy the default value in an expected path if copying from parent with incorrect type") {
            val base =  DObject("expected" := false)
            EmptyMaybeStructure.field.$copy(EmptyMaybeStructure.expected.default)(base).right.value shouldBe DObject("expected" := false, "field" := "default")
          }
        }
      }
      describe("In MaybePath") {
        it("Should do nothing if copying from path and parent not found") {
          val base = DObject.empty
          MaybeStructure.field.$copy(MaybeStructure.maybe.field)(base).right.value shouldBe base
        }
        it("Should fail with ExpectedFailure if copying from an path and expected property not found") {
          val base = DObject("maybe" ::= ("field2" := 2))
          MaybeStructure.field.$copy(MaybeStructure.maybe.expected)(base).left.value should contain(ExpectedFailure(MaybeStructure.maybe.expected))
        }
        it("Should do nothing if copying Expected where parent doesnt exist") {
          val base = DObject.empty
          MaybeStructure.field.$copy(MaybeStructure.maybe.expected)(base).right.value shouldBe base
        }
        it("Should fail with IncorrectType when parent is incorrect type") {
          val base = DObject("maybe" := false)
          MaybeStructure.field.$copy(MaybeStructure.maybe.field)(base).left.value should contain(IncorrectTypeFailure(MaybeStructure.maybe, false))
        }
        it("Should create path when copying") {
          val base = DObject("field" := "value")
          MaybeStructure.maybe.field.$copy(MaybeStructure.field)(base).right.value shouldBe DObject("field" := "value", "maybe" ::= ("field" := "value"))
        }
        it("Should remove path if copying removes final property") {
          val base = DObject("maybe" ::= ("field" := "value"))
          MaybeStructure.maybe.field.$copy(MaybeStructure.field)(base).right.value shouldBe DObject.empty
        }
        it("Should replace path when copying if parent of incorrect type") {
          val base = DObject("field" := "value", "maybe" := 1)
          MaybeStructure.maybe.field.$copy(MaybeStructure.field)(base).right.value shouldBe DObject("field" := "value", "maybe" ::= ("field" := "value"))
        }
        it("Should do nothing if copying the default value in an maybe path if the parent doesnt exist") {
          val base = DObject.empty
          MaybeStructure.field.$copy(MaybeStructure.maybe.default)(base).right.value shouldBe DObject.empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should do nothing if copying when parent is incorrect type") {
            val base = DObject("field" := "value", "maybe" := false)
            EmptyMaybeStructure.field.$copy(EmptyExpectedStructure.maybe.field)(base).right.value shouldBe base
          }
          it("Should do nothing if copying default from parent with incorrect type") {
            val base = DObject("expected" := false)
            EmptyMaybeStructure.field.$copy(EmptyMaybeStructure.maybe.default)(base).right.value shouldBe DObject("expected" := false)
          }
        }
      }
    }
  }

  object DefaultStructure extends Contract {
    val field = \![String]("defaultValue")
    val copy = \![String]("default")
    val maybeCopied = \?[String]
    val expectedCopied = \[String]
    val nulled = \![DNullable[Int]](DSome(23))

    val expected = new \\ {
      val maybe = \?[String]
      val field = \![String]("default")
      val expected = \[String]
    }
    val maybe = new \\? {
      val maybe = \?[String]
      val field = \![String]("default")
      val expected = \[String]
    }
  }

  object EmptyDefaultStructure extends Contract with EmptyOnIncorrectType {
    val field = \![String]("defaultValue")
    val copy = \![String]("default")
    val expectedCopied = \[String]
    val maybeCopied = \?[String]
    val nulled = \?[DNullable[Int]]
    val expected = new \\ {
      val maybe = \?[String]
      val field = \![String]("default")
      val expected = \[String]
    }
    val maybe = new \\? {
      val maybe = \?[String]
      val field = \![String]("default")
      val expected = \[String]
    }
  }

  describe("default lens") {
    describe("$verify") {
      describe("No Path") {
        it("Should return empty if valid value and valid values in path") {
          val base = DObject("field" := "value")
          DefaultStructure.field.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure failure if wrong type") {
          val base = DObject("field" := 1234)
          DefaultStructure.field.$verify(base) should contain(IncorrectTypeFailure(DefaultStructure.field, 1234))
        }
        it("Should return IncorrectTypeFailure failure if null value") {
          val base = DObject("field" := DNull)
          DefaultStructure.field.$verify(base) should contain(IncorrectTypeFailure(DefaultStructure.field, DNull))
        }
        it("Should return empty if value not found") {
          val base = DObject.empty
          DefaultStructure.field.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectTypeFailure if incorrect type") {
            val base = DObject("field" := "value", "copy" := 1234)
            EmptyDefaultStructure.copy.$verify(base) should contain(IncorrectTypeFailure(EmptyDefaultStructure.copy, 1234))
          }
        }
      }
      describe("In Expected Path") {
        it("Should return empty if nested value has correct type") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          DefaultStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(DefaultStructure.expected.field, 1234))
        }
        it("Should return IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          DefaultStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(DefaultStructure.expected, false))
        }
        it("Should return empty for the property if value not found") {
          val base = DObject("expected" := DObject.empty)
          DefaultStructure.expected.field.$verify(base) shouldBe empty
        }
        it("Should return empty if the parent object is not found") {
          val base = DObject.empty
          DefaultStructure.expected.field.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectType for the parent if the parent is the wrong type") {
            val base = DObject("expected" := 1)
            EmptyDefaultStructure.expected.field.$verify(base) should contain(IncorrectTypeFailure(EmptyDefaultStructure.expected, 1))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return empty if nested value has correct type") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$verify(base) shouldBe empty
        }
        it("Should return IncorrectTypeFailure if wrong type for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          DefaultStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(DefaultStructure.maybe.field, 1234))
        }
        it("Should return IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          DefaultStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(DefaultStructure.maybe, false))
        }
        it("Should return empty list if value not found") {
          val base = DObject("maybe" := DObject.empty)
          DefaultStructure.maybe.field.$verify(base) shouldBe empty
        }
        it("Should return empty list for the property if the Maybe parent object is not found") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$verify(base) shouldBe empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return IncorrectType for the parent if the parent is the wrong type") {
            val base = DObject("maybe" := 1)
            EmptyDefaultStructure.maybe.field.$verify(base) should contain(IncorrectTypeFailure(EmptyDefaultStructure.maybe, 1))
          }
        }
      }
    }
    describe("$get") {
      describe("No path") {
        it("Should return default if empty value") {
          DefaultStructure.field.$get(DObject.empty).right.value shouldBe Some("defaultValue")
        }
        it("Should fail with IncorrectTypeFailure if unexpected null value") {
          DefaultStructure.field.$get(DObject("field" := DNull)).left.value should contain(IncorrectTypeFailure(DefaultStructure.field, DNull))
        }
        it("Should fail with IncorrectTypeFailure if value is of the wrong type") {
          DefaultStructure.field.$get(DObject("field" := false)).left.value should contain(IncorrectTypeFailure(DefaultStructure.field, false))
        }
        it("Should return value if set") {
          DefaultStructure.field.$get(DObject("field" := "test")).right.value shouldBe Some("test")
        }
        it("Should return default if nullable empty") {
          DefaultStructure.nulled.$get(DObject.empty).right.value shouldBe Some(DSome(23))
        }
        it("Should fail with IncorrectTypeFailure if nullable wrong type") {
          DefaultStructure.nulled.$get(DObject("nulled" := "wrong")).left.value should contain(IncorrectTypeFailure(DefaultStructure.nulled, "wrong"))
        }
        it("Should return null if nullable null") {
          DefaultStructure.nulled.$get(DObject("nulled" := DNull)).right.value shouldBe Some(DNull)
        }
        it("Should return DSome Value if nullable set") {
          DefaultStructure.nulled.$get(DObject("nulled" := 123)).right.value shouldBe Some(DSome(123))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return default if unexpected null value") {
            val base = DObject("field" := DNull)
            EmptyDefaultStructure.field.$get(base).right.value shouldBe Some("defaultValue")
          }
          it("Should return default if wrong type") {
            val base = DObject("field" := false)
            EmptyDefaultStructure.field.$get(base).right.value shouldBe Some("defaultValue")
          }
        }
      }
      describe("In Expected Path") {
        it("Should return value if exists with correct type") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$get(base).right.value shouldBe Some("value")
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          DefaultStructure.expected.field.$get(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.expected.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          DefaultStructure.expected.field.$get(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.expected, false))
        }
        it("Should return default if property value not found") {
          val base = DObject("expected" := DObject.empty)
          DefaultStructure.expected.field.$get(base).right.value shouldBe Some("default")
        }
        it("Should return default if the Expected parent object is not found") {
          val base = DObject.empty
          DefaultStructure.expected.field.$get(base).right.value shouldBe Some("default")
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return default for the property if the parent is the wrong type and typeBehaviour is empty") {
            val base = DObject("expected" := 1)
            EmptyDefaultStructure.expected.field.$get(base).right.value shouldBe Some("default")
          }
        }
      }
      describe("In Maybe Path") {
        it("Should return value if exists with correct type") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$get(base).right.value shouldBe Some("value")
        }
        it("Should fail with IncorrectTypeFailure if wrong type in for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          DefaultStructure.maybe.field.$get(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.maybe.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          DefaultStructure.maybe.field.$get(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.maybe, false))
        }
        it("Should return default for the property if value not found") {
          val base = DObject("maybe" := DObject.empty)
          DefaultStructure.maybe.field.$get(base).right.value shouldBe Some("default")
        }
        it("Should return None for the property if the Maybe parent object is not found") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$get(base).right.value shouldBe None
        }
        describe("With EmptyOnIncorrectType") {
          it("Should return None for the property if the parent is the wrong type and typeBehaviour is empty") {
            val base = DObject("maybe" := 1)
            EmptyDefaultStructure.maybe.field.$get(base).right.value shouldBe None
          }
        }
      }
    }
    describe("$set") {
      describe("No Path") {
        it("should set an empty field") {
          val base = DObject.empty
          DefaultStructure.field.$set("test")(base) should be(DObject("field" := "test"))
        }
        it("Should replace an existing fields value") {
          val base = DObject("field" := "test")
          DefaultStructure.field.$set("test2")(base) should be(DObject("field" := "test2"))
        }
        it("Should replace a set field of the wrong type") {
          val base = DObject("field" := false)
          DefaultStructure.field.$set("test")(base) should be(DObject("field" := "test"))
        }
        it("Should set null value if nullable") {
          val base = DObject.empty
          DefaultStructure.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set value if nullable") {
          val base = DObject.empty
          DefaultStructure.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set value if nullable over a null value") {
          val base = DObject("nulled" := DNull)
          DefaultStructure.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set null over value ") {
          val base = DObject("nulled" := 123)
          DefaultStructure.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set the default value if set") {
          val base = DObject.empty
          DefaultStructure.field.$set(DefaultStructure.field._default)(base) shouldBe DObject("field" := "defaultValue")
        }
      }
      describe("In Expected Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          DefaultStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$set("value2")(base) shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("expected" := 1)
          DefaultStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("expected" ::= ("field2" := 23))
          DefaultStructure.expected.field.$set("value")(base) shouldBe DObject("expected" ::= ("field" := "value", "field2" := 23))
        }
        it("Should set the default value if set") {
          val base = DObject.empty
          DefaultStructure.expected.field.$set(DefaultStructure.expected.field._default)(base) shouldBe DObject("expected" ::= ("field" := "default"))
        }
      }
      describe("In Maybe Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$set("value2")(base) shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("maybe" := 1)
          DefaultStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("maybe" ::= ("field2" := 23))
          DefaultStructure.maybe.field.$set("value")(base) shouldBe DObject("maybe" ::= ("field" := "value", "field2" := 23))
        }
        it("Should set the default value if set") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$set(DefaultStructure.maybe.field._default)(base) shouldBe DObject("maybe" ::= ("field" := "default"))
        }
      }
    }
    describe("$maybeSet") {
      describe("No Path") {
        it("Should not change nothing if not set") {
          val base = DObject.empty
          DefaultStructure.field.$maybeSet(None)(base) shouldBe DObject.empty
        }
        it("Should not alter a value if not set") {
          val base = DObject("field" := "test")
          DefaultStructure.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not alter a value if not set and value of wrong type") {
          val base = DObject("field" := 123)
          DefaultStructure.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should alter a value if empty") {
          val base = DObject.empty
          DefaultStructure.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
        }
        it("Should alter a value if set") {
          val base = DObject("field" := "test")
          DefaultStructure.field.$maybeSet(Some("test2"))(base) shouldBe DObject("field" := "test2")
        }
        it("Should alter if value of wrong type") {
          val base = DObject("field" := 123)
          DefaultStructure.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
        }
        it("Should set null value if nullable") {
          val base = DObject.empty
          DefaultStructure.nulled.$maybeSet(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set value if nullable") {
          val base = DObject.empty
          DefaultStructure.nulled.$maybeSet(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should not set nullable if empty") {
          val base = DObject.empty
          DefaultStructure.nulled.$maybeSet(None)(base) shouldBe base
        }
        it("Should set the default value if set") {
          val base = DObject.empty
          DefaultStructure.field.$maybeSet(Some(DefaultStructure.field._default))(base) shouldBe DObject("field" := "defaultValue")
        }
      }
      describe("In Expected Path") {
        it("Should not create the nested field if internal is not set") {
          val base = DObject.empty
          DefaultStructure.expected.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          DefaultStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should create the nested field if internal is altered") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$maybeSet(Some("value2"))(base) shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should replace the nested field if it is of wrong type") {
          val base = DObject("expected" := 1)
          DefaultStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should not replace the nested field if it is of wrong type and the value is empty") {
          val base = DObject("expected" := 1)
          DefaultStructure.expected.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("expected" ::= ("field2" := 23))
          DefaultStructure.expected.field.$maybeSet(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value", "field2" := 23))
        }
        it("Should set the default value if set") {
          val base = DObject.empty
          DefaultStructure.expected.field.$maybeSet(Some(DefaultStructure.expected.field._default))(base) shouldBe DObject("expected" ::= ("field" := "default"))
        }
      }
      describe("In Maybe Path") {
        it("Should not create the nested field if internal is not set") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should create the nested field if internal is altered") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$maybeSet(Some("value2"))(base) shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should replace the nested field if it is of wrong type") {
          val base = DObject("maybe" := 1)
          DefaultStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should not replace the nested field if it is of wrong type and the value is empty") {
          val base = DObject("maybe" := 1)
          DefaultStructure.maybe.field.$maybeSet(None)(base) shouldBe base
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("maybe" ::= ("field2" := 23))
          DefaultStructure.maybe.field.$maybeSet(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value", "field2" := 23))
        }
        it("Should set the default value if set") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$maybeSet(Some(DefaultStructure.maybe.field._default))(base) shouldBe DObject("maybe" ::= ("field" := "default"))
        }
      }
    }
    describe("$restore") {
      describe("No path") {
        it("Should do nothing if dropped field not present") {
          val base = DObject("leave" := 123)
          DefaultStructure.field.$restore(base) shouldBe base
        }
        it("Should drop value if present") {
          val base = DObject("field" := "value")
          DefaultStructure.field.$restore(base) shouldBe DObject.empty
        }
        it("Should drop value if wrong type") {
          val base = DObject("field" := 123)
          DefaultStructure.field.$restore(base) shouldBe DObject.empty
        }
        it("Should drop value if nulled") {
          val base = DObject("nulled" := DNull, "leave" := 123)
          DefaultStructure.nulled.$restore(base) shouldBe DObject("leave" := 123)
        }
      }
      describe("In Expected Path") {
        it("Should drop value in path") {
          val base = DObject("expected" ::= ("field" := "value", "field2" := "value2"))
          DefaultStructure.expected.field.$restore(base) shouldBe DObject("expected" ::= ("field2" := "value2"))
        }
        it("Should clear path if drop is only element in object") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$restore(base) shouldBe DObject.empty
        }
      }
      describe("In Maybe Path") {
        it("Should drop value in path") {
          val base = DObject("maybe" ::= ("field" := "value", "field2" := "value2"))
          DefaultStructure.maybe.field.$restore(base) shouldBe DObject("maybe" ::= ("field2" := "value2"))
        }
        it("Should clear path if drop is only element in object") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$restore(base) shouldBe DObject.empty
        }
      }
    }
    describe("$setOrRestore") {
      describe("No path") {
        it("Should set an empty field") {
          val base = DObject.empty
          DefaultStructure.field.$setOrRestore(Some("test"))(base) should be(DObject("field" := "test"))
        }
        it("Should replace a set field") {
          val base = DObject("field" := "test")
          DefaultStructure.field.$setOrRestore(Some("test2"))(base) should be(DObject("field" := "test2"))
        }
        it("Should replace a set field of the wrong type") {
          val base = DObject("field" := false)
          DefaultStructure.field.$setOrRestore(Some("test"))(base) should be(DObject("field" := "test"))
        }
        it("Should set null value if nullable") {
          val base = DObject.empty
          DefaultStructure.nulled.$setOrRestore(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should set value if nullable") {
          val base = DObject.empty
          DefaultStructure.nulled.$setOrRestore(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set value if nullable over a null value") {
          val base = DObject("nulled" := DNull)
          DefaultStructure.nulled.$setOrRestore(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
        }
        it("Should set null over value ") {
          val base = DObject("nulled" := 123)
          DefaultStructure.nulled.$setOrRestore(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
        }
        it("Should not change if dropped field not present") {
          val base = DObject("leave" := 123)
          DefaultStructure.field.$setOrRestore(None)(base) shouldBe base
        }
        it("Should drop value if present") {
          val base = DObject("field" := "value")
          DefaultStructure.field.$setOrRestore(None)(base) shouldBe DObject.empty
        }
        it("Should drop value if wrong type") {
          val base = DObject("field" := 123)
          DefaultStructure.field.$setOrRestore(None)(base) shouldBe DObject.empty
        }
        it("Should drop value if nulled") {
          val base = DObject("nulled" := DNull, "leave" := 123)
          DefaultStructure.nulled.$setOrRestore(None)(base) shouldBe DObject("leave" := 123)
        }
      }
      describe("In Expected Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          DefaultStructure.expected.field.$setOrRestore(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$setOrRestore(Some("value2"))(base) shouldBe DObject("expected" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("expected" := 1)
          DefaultStructure.expected.field.$setOrRestore(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("expected" ::= ("field2" := 23))
          DefaultStructure.expected.field.$setOrRestore(Some("value"))(base) shouldBe DObject("expected" ::= ("field" := "value", "field2" := 23))
        }
        it("Should drop value in path") {
          val base = DObject("expected" ::= ("field" := "value", "field2" := "value2"))
          DefaultStructure.expected.field.$setOrRestore(None)(base) shouldBe DObject("expected" ::= ("field2" := "value2"))
        }
        it("Should clear path if drop is only element in object") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$setOrRestore(None)(base) shouldBe DObject.empty
        }
      }
      describe("In Maybe Path") {
        it("Should create the nested path structure if internal is set on empty object") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$setOrRestore(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should replace the nested field if internal is altered") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$setOrRestore(Some("value2"))(base) shouldBe DObject("maybe" ::= ("field" := "value2"))
        }
        it("Should replace the parent object if it is of wrong type") {
          val base = DObject("maybe" := 1)
          DefaultStructure.maybe.field.$setOrRestore(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value"))
        }
        it("Should not effect sibling nested fields when being set or replaced") {
          val base = DObject("maybe" ::= ("field2" := 23))
          DefaultStructure.maybe.field.$setOrRestore(Some("value"))(base) shouldBe DObject("maybe" ::= ("field" := "value", "field2" := 23))
        }
        it("Should drop value in path") {
          val base = DObject("maybe" ::= ("field" := "value", "field2" := "value2"))
          DefaultStructure.maybe.field.$setOrRestore(None)(base) shouldBe DObject("maybe" ::= ("field2" := "value2"))
        }
        it("Should clear path if drop is only element in object") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$setOrRestore(None)(base) shouldBe DObject.empty
        }
      }
    }
    describe("$modify") {
      val pf: Function[String, String] = {
        case "defaultValue" => "wasEmpty"
        case t => t + " found"
      }
      describe("No path") {
        it("Should modify default value if empty") {
          val base = DObject.empty
          DefaultStructure.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
        }
        it("Should modify a set value") {
          val base = DObject("field" := "value")
          DefaultStructure.field.$modify(pf)(base).right.value shouldBe DObject("field" := "value found")
        }
        it("Should remain set when modifying to defaultValue") {
          val base = DObject("field" := "value")
          DefaultStructure.field.$modify(_ => "defaultValue")(base).right.value shouldBe DObject("field" := "defaultValue")
        }
        it("Should fail with IncorrectTypeFailure when type is wrong") {
          val base = DObject("field" := 123)
          DefaultStructure.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.field, 123))
        }
        it("Should fail with IncorrectTypeFailure when type is null") {
          val base = DObject("field" := DNull)
          DefaultStructure.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.field, DNull))
        }
        it("Should modify a null value") {
          val pfN: Function[DNullable[Int], DNullable[Int]] = {
            case DNull => DSome(0)
            case DSome(x) => DSome(x + 1)
          }
          val base = DObject("nulled" := DNull)
          DefaultStructure.nulled.$modify(pfN)(base).right.value shouldBe DObject("nulled" := 0)
          val base2 = DObject("nulled" := 123)
          DefaultStructure.nulled.$modify(pfN)(base2).right.value shouldBe DObject("nulled" := 124)
          val base3 = DObject.empty
          DefaultStructure.nulled.$modify(pfN)(base3).right.value shouldBe DObject("nulled" := 24)
        }
        describe("With EmptyOnIncorrectType") {
          it("Should modify default when null and is incorrect type") {
            val base = DObject("field" := DNull)
            EmptyDefaultStructure.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
          }
          it("Should modify default when value is wrong type ") {
            val base = DObject("field" := 123)
            EmptyDefaultStructure.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
          }
        }
      }
      describe("In Expected Path") {
        it("Should modify a nested value") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "value found"))
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("expected" ::= ("field" := 1234))
          DefaultStructure.expected.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.expected.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("expected" := false)
          DefaultStructure.expected.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.expected, false))
        }
        it("Should modify structure if nested value not found") {
          val base = DObject("expected" ::= ("field2" := "value"))
          DefaultStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field2" := "value", "field" := "default found"))
        }
        it("Should create structure if parent not found") {
          val base = DObject.empty
          DefaultStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "default found"))
        }
        describe("With EmptyOnIncorrectType") {
          it("Should treat as empty if nested property is wrong type") {
            val base = DObject("expected" ::= ("field" := 1234))
            EmptyDefaultStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "default found"))
          }
          it("Should treat as empty if parent is wrong type and create structure") {
            val base = DObject("expected" := 123.23)
            EmptyDefaultStructure.expected.field.$modify(pf)(base).right.value shouldBe DObject("expected" ::= ("field" := "default found"))
          }
        }
      }
      describe("In Maybe Path") {
        it("Should modify a nested value") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$modify(pf)(base).right.value shouldBe DObject("maybe" ::= ("field" := "value found"))
        }
        it("Should fail with IncorrectTypeFailure if wrong type for property") {
          val base = DObject("maybe" ::= ("field" := 1234))
          DefaultStructure.maybe.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.maybe.field, 1234))
        }
        it("Should fail with IncorrectTypeFailure for the parent if parent is wrong type") {
          val base = DObject("maybe" := false)
          DefaultStructure.maybe.field.$modify(pf)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.maybe, false))
        }
        it("Should modify structure if nested value not found") {
          val base = DObject("maybe" ::= ("field2" := "value"))
          DefaultStructure.maybe.field.$modify(pf)(base).right.value shouldBe DObject("maybe" ::= ("field2" := "value", "field" := "default found"))
        }
        it("Should do nothing if parent not found") {
          val base = DObject.empty
          DefaultStructure.maybe.field.$modify(pf)(base).right.value shouldBe base
        }
        describe("With EmptyOnIncorrectType") {
          it("Should treat as empty if nested property is wrong type") {
            val base = DObject("maybe" ::= ("field" := 1234))
            EmptyDefaultStructure.maybe.field.$modify(pf)(base).right.value shouldBe DObject("maybe" ::= ("field" := "default found"))
          }
          it("Should treat as empty if parent is wrong type and do nothing") {
            val base = DObject("maybe" := 123.23)
            EmptyDefaultStructure.maybe.field.$modify(pf)(base).right.value shouldBe base
          }
        }
      }
    }
    describe("$copy") {
      describe("No Path") {
        it("Should copy maybe empty to empty") {
          val base = DObject.empty
          DefaultStructure.copy.$copy(DefaultStructure.maybeCopied)(base).right.value shouldBe base
        }
        it("Copying an empty value to a set value will set value to empty") {
          val base = DObject("copy" := "leave")
          DefaultStructure.copy.$copy(DefaultStructure.maybeCopied)(base).right.value shouldBe DObject.empty
        }
        it("Should fail with IncorrectTypeFailure if copying an incorrect type") {
          val base1 = DObject("field" := 123)
          DefaultStructure.copy.$copy(DefaultStructure.field)(base1).left.value should contain(IncorrectTypeFailure(DefaultStructure.field, 123))
          val base2 = DObject("copy" := "leave", "field" := 123)
          DefaultStructure.copy.$copy(DefaultStructure.field)(base2).left.value should contain(IncorrectTypeFailure(DefaultStructure.field, 123))
        }
        it("Should fail with IncorrectTypeFailure if copying an unexpected Null value") {
          val base1 = DObject("field" := DNull)
          DefaultStructure.copy.$copy(DefaultStructure.field)(base1).left.value should contain(IncorrectTypeFailure(DefaultStructure.field, DNull))
          val base2 = DObject("copy" := "leave", "field" := DNull)
          DefaultStructure.copy.$copy(DefaultStructure.field)(base2).left.value should contain(IncorrectTypeFailure(DefaultStructure.field, DNull))
        }
        it("Should create the property when copying to an unset property") {
          val base = DObject("field" := "copyMe")
          DefaultStructure.copy.$copy(DefaultStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should override value when copying to an already set property") {
          val base = DObject("field" := "copyMe", "copy" := "replaceMe")
          DefaultStructure.copy.$copy(DefaultStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should override value when copying to an already set property of the wrong type") {
          val base = DObject("field" := "copyMe", "copy" := 123)
          DefaultStructure.copy.$copy(DefaultStructure.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
        }
        it("Should Copy an expected value") {
          val base = DObject("expectedCopied" := "test2")
          DefaultStructure.copy.$copy(DefaultStructure.expectedCopied)(base).right.value shouldBe DObject("expectedCopied" := "test2", "copy" := "test2")
        }
        it("Should fail with ExpectedFailure if copying empty expected value") {
          val base = DObject.empty
          DefaultStructure.copy.$copy(DefaultStructure.expectedCopied)(base).left.value should contain(ExpectedFailure(DefaultStructure.expectedCopied))
        }
        it("Should fail with IncorrectTypeFailure if copying an expected value with incorrect type") {
          val base = DObject("expectedCopied" := 123)
          DefaultStructure.copy.$copy(DefaultStructure.expectedCopied)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.expectedCopied, 123))
        }
        it("Should copy a default value when default value is not set") {
          val base = DObject.empty
          DefaultStructure.copy.$copy(DefaultStructure.field)(base).right.value shouldBe DObject("copy" := "defaultValue")
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure if copying from an empty expected property") {
            val base = DObject.empty
            EmptyDefaultStructure.copy.$copy(EmptyDefaultStructure.expectedCopied)(base).left.value should contain(ExpectedFailure(EmptyDefaultStructure.expectedCopied))
          }
          it("Should fail with ExpectedFailure if copying from an expected property with incorrect type") {
            val base = DObject("expectedCopied" := false)
            EmptyDefaultStructure.copy.$copy(EmptyDefaultStructure.expectedCopied)(base).left.value should contain(ExpectedFailure(EmptyDefaultStructure.expectedCopied))
          }
          it("Should replace with empty if copying from a maybe field with incorrect type") {
            val base = DObject("copy":= "value", "maybeCopied" := false)
            EmptyDefaultStructure.copy.$copy(EmptyExpectedStructure.maybeCopied)(base).right.value shouldBe DObject("maybeCopied" := false)
          }
          it("Copying a default value set as the wrong type should set default value") {
            val base = DObject("field" := 1234)
            EmptyDefaultStructure.copy.$copy(EmptyDefaultStructure.field)(base).right.value shouldBe DObject("field" := 1234, "copy" := "defaultValue")
          }
        }
      }
      describe("In Expected Path") {
        it("Should apply empty if copying from maybe field in Expected path and parent is empty") {
          val base = DObject("field" := "value")
          DefaultStructure.field.$copy(DefaultStructure.expected.maybe)(base).right.value shouldBe DObject.empty
        }
        it("Should fail with ExpectedFailure if copying from Expected path and expected property not found") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.field.$copy(DefaultStructure.expected.expected)(base).left.value should contain(ExpectedFailure(DefaultStructure.expected.expected))
        }
        it("Should fail with IncorrectType when parent is incorrect type") {
          val base = DObject("expected" := false)
          DefaultStructure.field.$copy(DefaultStructure.expected.field)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.expected, false))
        }
        it("Should create path when copying") {
          val base = DObject("field" := "value")
          DefaultStructure.expected.field.$copy(DefaultStructure.field)(base).right.value shouldBe DObject("field" := "value", "expected" ::= ("field" := "value"))
        }
        it("Should remove path if copying removes final property") {
          val base = DObject("expected" ::= ("field" := "value"))
          DefaultStructure.expected.field.$copy(DefaultStructure.maybeCopied)(base).right.value shouldBe DObject.empty
        }
        it("Should replace path when copying if parent of incorrect type") {
          val base = DObject("field" := "value", "expected" := 1)
          DefaultStructure.expected.field.$copy(DefaultStructure.field)(base).right.value shouldBe DObject("field" := "value", "expected" ::= ("field" := "value"))
        }
        it("Should copy the default value in an expected path if the parent doesnt exist") {
          val base = DObject.empty
          DefaultStructure.field.$copy(DefaultStructure.expected.field)(base).right.value shouldBe DObject("field" := "default")
        }
        describe("With EmptyOnIncorrectType") {
          it("Should fail with ExpectedFailure if copying expected from parent with incorrect type") {
            val base = DObject("expected" := false)
            EmptyDefaultStructure.field.$copy(EmptyDefaultStructure.expected.expected)(base).left.value should contain(ExpectedFailure(EmptyDefaultStructure.expected.expected))
          }
          it("Should copy empty if copying maybe from parent with incorrect type") {
            val base = DObject("field" := "value", "expected" := false)
            EmptyDefaultStructure.field.$copy(EmptyDefaultStructure.expected.maybe)(base).right.value shouldBe DObject("expected" := false)
          }
          it("Should copy the default value in an expected path if copying from parent with incorrect type") {
            val base =  DObject("expected" := false)
            EmptyDefaultStructure.field.$copy(EmptyDefaultStructure.expected.field)(base).right.value shouldBe DObject("expected" := false, "field" := "default")
          }
        }
      }
      describe("In MaybePath") {
        it("Should do nothing if copying from path and parent not found") {
          val base = DObject.empty
          DefaultStructure.field.$copy(DefaultStructure.maybe.field)(base).right.value shouldBe base
        }
        it("Should fail with ExpectedFailure if copying from an path and expected property not found") {
          val base = DObject("maybe" ::= ("field2" := 2))
          DefaultStructure.field.$copy(DefaultStructure.maybe.expected)(base).left.value should contain(ExpectedFailure(DefaultStructure.maybe.expected))
        }
        it("Should do nothing if copying Expected where parent doesnt exist") {
          val base = DObject.empty
          DefaultStructure.field.$copy(DefaultStructure.maybe.expected)(base).right.value shouldBe base
        }
        it("Should fail with IncorrectType when parent is incorrect type") {
          val base = DObject("maybe" := false)
          DefaultStructure.field.$copy(DefaultStructure.maybe.field)(base).left.value should contain(IncorrectTypeFailure(DefaultStructure.maybe, false))
        }
        it("Should create path when copying") {
          val base = DObject("field" := "value")
          DefaultStructure.maybe.field.$copy(DefaultStructure.field)(base).right.value shouldBe DObject("field" := "value", "maybe" ::= ("field" := "value"))
        }
        it("Should remove path if copying removes final property") {
          val base = DObject("maybe" ::= ("field" := "value"))
          DefaultStructure.maybe.field.$copy(DefaultStructure.maybeCopied)(base).right.value shouldBe DObject.empty
        }
        it("Should replace path when copying if parent of incorrect type") {
          val base = DObject("field" := "value", "maybe" := 1)
          DefaultStructure.maybe.field.$copy(DefaultStructure.field)(base).right.value shouldBe DObject("field" := "value", "maybe" ::= ("field" := "value"))
        }
        it("Should do nothing if copying the default value in an maybe path if the parent doesnt exist") {
          val base = DObject.empty
          DefaultStructure.field.$copy(DefaultStructure.maybe.field)(base).right.value shouldBe DObject.empty
        }
        describe("With EmptyOnIncorrectType") {
          it("Should do nothing if copying when parent is incorrect type") {
            val base = DObject("field" := "value", "maybe" := false)
            EmptyDefaultStructure.field.$copy(EmptyExpectedStructure.maybe.field)(base).right.value shouldBe base
          }
          it("Should do nothing if copying default from parent with incorrect type") {
            val base = DObject("expected" := false)
            EmptyDefaultStructure.field.$copy(EmptyDefaultStructure.maybe.field)(base).right.value shouldBe DObject("expected" := false)
          }
        }
      }
    }
  }

}
