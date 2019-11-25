package dsentric.contracts

import dsentric._
import dsentric.failure.{ExpectedFailure, IncorrectTypeFailure}
import org.scalatest.{EitherValues, FunSpec, Matchers}

class PropertyLensTests extends FunSpec with Matchers with FailureMatchers with EitherValues {

  import Dsentric._
  import PessimisticCodecs._
  import dsentric.Implicits._

  object ExpectedField extends Contract {
    val field = \[String]
    val copy = \[String]
    val maybeCopied = \?[String]
    val defaultCopied = \![String]("default")
    val nulled = \[DNullable[Int]]
    val nested = new \\ {
      val nestedField = \[String]
      val nestedCopy = \[String]
    }
  }

  object EmptyExpectedField extends Contract with EmptyOnIncorrectType {
    val field = \[String]
    val copy = \[String]
    val defaultCopied = \![String]("default")
  }


  describe("expected lens") {
    describe("$set") {
      it("should set an empty field") {
        val base = DObject.empty
        ExpectedField.field.$set("test")(base) should be(DObject("field" := "test"))
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        ExpectedField.field.$set("test2")(base) should be(DObject("field" := "test2"))
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        ExpectedField.field.$set("test")(base) should be(DObject("field" := "test"))
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        ExpectedField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        ExpectedField.nested.nestedField.$set("value2")(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        ExpectedField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        ExpectedField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        ExpectedField.nested.nestedField.$set("value")(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        ExpectedField.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        ExpectedField.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set value if nullable over a null value") {
        val base = DObject("nulled" := DNull)
        ExpectedField.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set null over value ") {
        val base = DObject("nulled" := 123)
        ExpectedField.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
      }
    }
    describe("$get") {
      it("Should empty fail if empty value") {
        ExpectedField.field.$get(DObject.empty).left.value should contain (ExpectedFailure(ExpectedField.field))
      }
      it("Should incorrect type  if null value") {
        ExpectedField.field.$get(DObject("field" := DNull)).left.value should contain (IncorrectTypeFailure(ExpectedField.field, DNull))
      }
      it("Should incorrect type  if value is of the wrong type") {
        ExpectedField.field.$get(DObject("field" := false)).left.value should contain (IncorrectTypeFailure(ExpectedField.field, false))
      }
      it("Should empty if null value and EmptyOnIncorrectType") {
        EmptyExpectedField.field.$get(DObject("field" := DNull)).left.value should contain (ExpectedFailure(EmptyExpectedField.field))
      }
      it("Should empty if wrong type and EmptyOnIncorrectType") {
        EmptyExpectedField.field.$get(DObject("field" := false)).left.value should contain (ExpectedFailure(EmptyExpectedField.field))
      }
      it("Should return value if set") {
        ExpectedField.field.$get(DObject("field" := "test")).right.value shouldBe ("test")
      }
      it("Should return nested field value") {
        ExpectedField.nested.nestedField.$get(DObject("nested" ::= ("nestedField" := "value"))).right.value shouldBe "value"
      }
      it("Should incorrect type if nullable empty") {
        ExpectedField.nulled.$get(DObject.empty).left.value should contain (ExpectedFailure(ExpectedField.nulled))
      }
      it("Should incorrect type  if nullable wrong type") {
        ExpectedField.nulled.$get(DObject("nulled" := "wrong")).left.value should contain (IncorrectTypeFailure(ExpectedField.nulled, "wrong"))
      }
      it("Should return null if nullable null") {
        ExpectedField.nulled.$get(DObject("nulled" := DNull)).right.value shouldBe DNull
      }
      it("Should return DSome Value if nullable set") {
        ExpectedField.nulled.$get(DObject("nulled" := 123)).right.value shouldBe DSome(123)
      }
    }
    describe("$maybeSet") {
      it("Should not change nothing if not set") {
        val base = DObject.empty
        ExpectedField.field.$maybeSet(None)(base) shouldBe DObject.empty
      }
      it("Should not alter a value if not set") {
        val base = DObject("field" := "test")
        ExpectedField.field.$maybeSet(None)(base) shouldBe base
      }
      it("Should not alter a value if not set and value of wrong type") {
        val base = DObject("field" := 123)
        ExpectedField.field.$maybeSet(None)(base) shouldBe base
      }
      it("Should alter a value if empty") {
        val base = DObject.empty
        ExpectedField.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
      }
      it("Should alter a value if set") {
        val base = DObject("field" := "test")
        ExpectedField.field.$maybeSet(Some("test2"))(base) shouldBe DObject("field" := "test2")
      }
      it("Should alter if value of wrong type") {
        val base = DObject("field" := 123)
        ExpectedField.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
      }
      it("Should not create the nested field if internal is not set") {
        val base = DObject.empty
        ExpectedField.nested.nestedField.$maybeSet(None)(base) shouldBe base
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        ExpectedField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        ExpectedField.nested.nestedField.$maybeSet(Some("value2"))(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        ExpectedField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        ExpectedField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        ExpectedField.nested.nestedField.$maybeSet(Some("value"))(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        ExpectedField.nulled.$maybeSet(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        ExpectedField.nulled.$maybeSet(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should not set nullable if empty") {
        val base = DObject.empty
        ExpectedField.nulled.$maybeSet(None)(base) shouldBe base
      }
    }
    describe("$modify") {
      it("Should expected fail an empty value empty") {
        val base = DObject.empty
        ExpectedField.field.$modify(_ + "2")(base).left.value should contain (ExpectedFailure(ExpectedField.field))
      }
      it("Should expected fail an empty value empty with EmptyOnIncorrectType") {
        val base = DObject.empty
        EmptyExpectedField.field.$modify(_ + "2")(base).left.value should contain (ExpectedFailure(EmptyExpectedField.field))
      }
      it("Should incorrect type fail on incorrect type") {
        val base = DObject("field" := 123)
        ExpectedField.field.$modify(_ + "2")(base).left.value should contain (IncorrectTypeFailure(ExpectedField.field, 123))
      }
      it("Should incorrect type fail on null type") {
        val base = DObject("field" := DNull)
        ExpectedField.field.$modify(_ + "2")(base).left.value should contain (IncorrectTypeFailure(ExpectedField.field, DNull))
      }
      it("Should expected fail on incorrect type with EmptyOnIncorrectType") {
        val base = DObject("field" := 123)
        EmptyExpectedField.field.$modify(_ + "2")(base).left.value should contain (ExpectedFailure(EmptyExpectedField.field))
      }
      it("Should expected fail on null type  with EmptyOnIncorrectType") {
        val base = DObject("field" := DNull)
        EmptyExpectedField.field.$modify(_ + "2")(base).left.value should contain (ExpectedFailure(EmptyExpectedField.field))
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        ExpectedField.field.$modify(_ + "2")(base).right.value shouldBe DObject("field" := "value2")
      }
      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        ExpectedField.nested.nestedField.$modify(_ + "2")(base).right.value shouldBe DObject("nested" ::= ("nestedField" := "value2"))
      }
      it("Should expected fail if nested value not found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        ExpectedField.nested.nestedField.$modify(_ + "2")(base).left.value should contain (ExpectedFailure(ExpectedField.nested.nestedField))
      }
      it("Should modify a null value") {
        val base = DObject("nulled" := DNull)
        ExpectedField.nulled.$modify{case DNull => DSome(0); case DSome(x) => DSome(x + 1)}(base).right.value shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        ExpectedField.nulled.$modify{case DNull => DSome(0); case DSome(x) => DSome(x + 1)}(base2).right.value shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        ExpectedField.nulled.$modify{case DNull => DSome(0); case DSome(x) => DSome(x + 1)}(base3).left.value should contain (ExpectedFailure(ExpectedField.nulled))
        val base4 = DObject("nulled" := "wrong")
        ExpectedField.nulled.$modify{case DNull => DSome(0); case DSome(x) => DSome(x + 1)}(base4).left.value should contain (IncorrectTypeFailure(ExpectedField.nulled, "wrong"))
      }
    }
    describe("$copy") {
      it("copying expected empty is expected failure") {
        val base = DObject.empty
        ExpectedField.copy.$copy(ExpectedField.field)(base).left.value should contain (ExpectedFailure(ExpectedField.field))
      }
      it("Copying an empty value to a set value is expected failure") {
        val base = DObject("copy" := "leave")
        ExpectedField.copy.$copy(ExpectedField.field)(base).left.value should contain (ExpectedFailure(ExpectedField.field))
      }
      it("Copying a wrong type to a set value is incorrect type failure") {
        val base1 = DObject("field" := 123)
        ExpectedField.copy.$copy(ExpectedField.field)(base1).left.value should contain (IncorrectTypeFailure(ExpectedField.field, 123))
        val base2 = DObject("copy" := "leave", "field" := 123)
        ExpectedField.copy.$copy(ExpectedField.field)(base2).left.value should contain (IncorrectTypeFailure(ExpectedField.field, 123))
      }
      it("Copying a null value to a set value is incorrect type failure") {
        val base1 = DObject("field" := DNull)
        ExpectedField.copy.$copy(ExpectedField.field)(base1).left.value should contain (IncorrectTypeFailure(ExpectedField.field, DNull))
        val base2 = DObject("copy" := "leave", "field" := DNull)
        ExpectedField.copy.$copy(ExpectedField.field)(base2).left.value should contain (IncorrectTypeFailure(ExpectedField.field, DNull))
      }
      it("Copying to an empty value will copy") {
        val base = DObject("field" := "copyMe")
        ExpectedField.copy.$copy(ExpectedField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying to a set value will override") {
        val base = DObject("field" := "copyMe", "copy" := "replaceMe")
        ExpectedField.copy.$copy(ExpectedField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying over a value of the wrong type will replace") {
        val base = DObject("field" := "copyMe", "copy" := 123)
        ExpectedField.copy.$copy(ExpectedField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying into a nested value will create nested structure") {
        val base = DObject("field" := "copyMe")
        ExpectedField.nested.nestedField.$copy(ExpectedField.field)(base).right.value shouldBe DObject("field" := "copyMe", "nested" ::= ("nestedField" := "copyMe"))
      }
      it("Copying a from a nested structure into a nested structure will work") {
        val base = DObject("nested" ::= ("nestedCopy" := "copyMe"))
        ExpectedField.nested.nestedField.$copy(ExpectedField.nested.nestedCopy)(base).right.value shouldBe DObject("nested" ::= ("nestedCopy" := "copyMe", "nestedField" := "copyMe"))
      }
      it("Copying a maybe field will copy the value") {
        val base = DObject("maybeCopied" := "test2")
        ExpectedField.copy.$copy(ExpectedField.maybeCopied)(base).right.value shouldBe DObject("maybeCopied" := "test2", "copy" := "test2")
      }
      it("Copying an empty maybe field will fail with expected type") {
        val base = DObject("copy" := "test")
        ExpectedField.copy.$copy(ExpectedField.maybeCopied)(base).left.value should contain (ExpectedFailure(ExpectedField.copy))
      }
      it("Copying an empty default value should set as default") {
        val base = DObject.empty
        ExpectedField.copy.$copy(ExpectedField.defaultCopied)(base).right.value shouldBe DObject("copy" := "default")
      }
      it("Copying a set default value should set as default value") {
        val base = DObject("defaultCopied" := "notDefault")
        ExpectedField.copy.$copy(ExpectedField.defaultCopied)(base).right.value shouldBe DObject("defaultCopied" := "notDefault", "copy" := "notDefault")
      }
      it("Copying a default value set as the wrong type should fail with incorrect type") {
        val base = DObject("defaultCopied" := 1234)
        ExpectedField.copy.$copy(ExpectedField.defaultCopied)(base).left.value should contain (IncorrectTypeFailure(ExpectedField.defaultCopied, 1234))
      }
      it("Copying a default value set as the wrong type should set default value when EmptyOnIncorrectType") {
        val base = DObject("defaultCopied" := 1234)
        EmptyExpectedField.copy.$copy(EmptyExpectedField.defaultCopied)(base).right.value shouldBe DObject("defaultCopied" := 1234, "copy" := "default")
      }
    }
  }

  object MaybeField extends Contract {
    val field = \?[String]
    val copy = \?[String]
    val expectedCopied = \[String]
    val defaultCopied = \![String]("default")
    val nested = new \\? {
      val nestedField = \?[String]
      val nestedCopy = \?[String]
    }
    val nulled = \?[DNullable[Int]]
  }

  object EmptyMaybeField extends Contract with EmptyOnIncorrectType {
    val field = \?[String]
    val copy = \?[String]
    val expectedCopied = \[String]
    val defaultCopied = \![String]("default")

    val nulled = \?[DNullable[Int]]
  }

  describe("maybe lens") {
    describe("$set") {
      it("should set an empty field") {
        val base = DObject.empty
        MaybeField.field.$set("test")(base) shouldBe DObject("field" := "test")
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        MaybeField.field.$set("test2")(base) shouldBe DObject("field" := "test2")
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        MaybeField.field.$set("test")(base) shouldBe DObject("field" := "test")
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$set("value")(base) shouldBe DObject("nested" ::= ("nestedField" := "value"))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$set("value2")(base) shouldBe DObject("nested" ::= ("nestedField" := "value2"))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        MaybeField.nested.nestedField.$set("value")(base) shouldBe DObject("nested" ::= ("nestedField" := "value"))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        MaybeField.nested.nestedField.$set("value")(base) shouldBe DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        MaybeField.nested.nestedField.$set("value")(base2) shouldBe DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        MaybeField.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        MaybeField.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set value if nullable over a null value") {
        val base = DObject("nulled" := DNull)
        MaybeField.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set null over value ") {
        val base = DObject("nulled" := 123)
        MaybeField.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
      }
    }
    describe("$get") {
      it("Should return empty if empty value") {
        MaybeField.field.$get(DObject.empty).right.value shouldBe empty
      }
      it("Should incorrect type fail when null value") {
        MaybeField.field.$get(DObject("field" := DNull)).left.value should contain (IncorrectTypeFailure(MaybeField.field, DNull))
      }
      it("Should return empty if null value when EmptyOnIncorrectType") {
        EmptyMaybeField.field.$get(DObject("field" := DNull)).right.value shouldBe empty
      }
      it("Should incorrect type fail if value is of the wrong type") {
        MaybeField.field.$get(DObject("field" := false)).left.value should contain (IncorrectTypeFailure(MaybeField.field, false))
      }
      it("Should return empty  if value is of the wrong type and when EmptyOnIncorrectType") {
        EmptyMaybeField.field.$get(DObject("field" := false)).right.value shouldBe empty
      }
      it("Should return value if set") {
        MaybeField.field.$get(DObject("field" := "test")).right.value should contain ("test")
      }
      it("Should return nested field value") {
        MaybeField.nested.nestedField.$get(DObject("nested" ::= ("nestedField" := "value"))).right.value should contain ("value")
      }
      it("Should return empty if nullable empty") {
        MaybeField.nulled.$get(DObject.empty).right.value shouldBe empty
      }
      it("Should incorrect type fail  if nullable wrong type") {
        MaybeField.nulled.$get(DObject("nulled" := "wrong")).left.value should contain (IncorrectTypeFailure(MaybeField.nulled, "wrong"))
      }
      it("Should return null if nullable null") {
        MaybeField.nulled.$get(DObject("nulled" := DNull)).right.value should contain (DNull)
      }
      it("Should return DSome Value if nullable set") {
        MaybeField.nulled.$get(DObject("nulled" := 123)).right.value should contain (DSome(123))
      }
    }
    describe("$getOrElse") {
      it("Should return orElse if empty value") {
        MaybeField.field.$getOrElse(DObject.empty, "orElse").right.value shouldBe "orElse"
      }
      it("Should return orElse if value is of the wrong type when EmptyOnIncorrectType") {
        EmptyMaybeField.field.$getOrElse(DObject("field" := false), "orElse").right.value shouldBe "orElse"
      }
      it("Should incorrect type fail if value is of the wrong type") {
        MaybeField.field.$getOrElse(DObject("field" := false), "orElse").left.value should contain (IncorrectTypeFailure(MaybeField.field, false))
      }
      it("Should return orElse if null value when EmptyOnIncorrectType") {
        EmptyMaybeField.field.$getOrElse(DObject("field" := DNull), "orElse").right.value shouldBe "orElse"
      }
      it("Should incorrect type fail if value is null") {
        MaybeField.field.$getOrElse(DObject("field" := DNull), "orElse").left.value should contain (IncorrectTypeFailure(MaybeField.field, DNull))
      }
      it("Should return value if set") {
        MaybeField.field.$getOrElse(DObject("field" := "test"), "orElse").right.value shouldBe "test"
      }

      it("Should return nested field value") {
        MaybeField.nested.nestedField.$getOrElse(DObject("nested" ::= ("nestedField" := "value")), "orElse").right.value shouldBe "value"
      }
      it("Should return orElse if nullable empty") {
        MaybeField.nulled.$getOrElse(DObject.empty, DSome(123)).right.value shouldBe DSome(123)
        MaybeField.nulled.$getOrElse(DObject.empty, DNull).right.value shouldBe DNull
      }
      it("Should return orElse if nullable wrong type and EmptyOnIncorrectType") {
        EmptyMaybeField.nulled.$getOrElse(DObject("nulled" := "wrong"), DSome(123)).right.value shouldBe DSome(123)
      }
      it("Should incorrect type fail if nullable wrong type") {
        MaybeField.nulled.$getOrElse(DObject("nulled" := "wrong"), DSome(123)).left.value should contain (IncorrectTypeFailure(MaybeField.nulled, "wrong"))
      }
      it("Should return null if nullable null") {
        MaybeField.nulled.$getOrElse(DObject("nulled" := DNull), DSome(123)).right.value shouldBe DNull
      }
      it("Should return DSome Value if nullable set") {
        MaybeField.nulled.$getOrElse(DObject("nulled" := 123), DSome(124)).right.value shouldBe DSome(123)
      }
    }
    describe("$maybeSet") {
      it("Should not change nothing if not set") {
        val base = DObject.empty
        MaybeField.field.$maybeSet(None)(base) shouldBe DObject.empty
      }
      it("Should not alter a value if not set") {
        val base = DObject("field" := "test")
        MaybeField.field.$maybeSet(None)(base) shouldBe base
      }
      it("Should not alter a value if not set and value of wrong type") {
        val base = DObject("field" := 123)
        MaybeField.field.$maybeSet(None)(base) shouldBe base
      }
      it("Should alter a value if empty") {
        val base = DObject.empty
        MaybeField.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
      }
      it("Should alter a value if set") {
        val base = DObject("field" := "test")
        MaybeField.field.$maybeSet(Some("test2"))(base) shouldBe DObject("field" := "test2")
      }
      it("Should alter if value of wrong type") {
        val base = DObject("field" := 123)
        MaybeField.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
      }
      it("Should not create the nested field if internal is not set") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$maybeSet(None)(base) shouldBe base
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$maybeSet(Some("value2"))(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        MaybeField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        MaybeField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        MaybeField.nested.nestedField.$maybeSet(Some("value"))(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        MaybeField.nulled.$maybeSet(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        MaybeField.nulled.$maybeSet(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should not set nullable if empty") {
        val base = DObject.empty
        MaybeField.nulled.$maybeSet(None)(base) shouldBe base
      }
    }
    describe("$modify") {
      val pf:Function[Option[String], String] = {
        case None => "wasEmpty"
        case Some(t) => t + " found"
      }
      it("Should modify empty value") {
        val base = DObject.empty
        MaybeField.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        MaybeField.field.$modify(pf)(base).right.value shouldBe DObject("field" := "value found")
      }
      it("Should fail incorrect type if wrong type") {
        val base = DObject("field" := 123)
        MaybeField.field.$modify(pf)(base).left.value should contain (IncorrectTypeFailure(MaybeField.field, 123))
      }
      it("Should treat as empty a wrong type when EmptyOnIncorrectType") {
        val base = DObject("field" := 123)
        EmptyMaybeField.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }
      it("Should fail incorrect type if  leave a null type") {
        val base = DObject("field" := DNull)
        MaybeField.field.$modify(pf)(base).left.value should contain (IncorrectTypeFailure(MaybeField.field, DNull))
      }
      it("Should treat as empty null when EmptyOnIncorrectType") {
        val base = DObject("field" := DNull)
        EmptyMaybeField.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$modify(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField" := "value found"))
      }
      it("Should modify structure if nested value not found found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        MaybeField.nested.nestedField.$modify(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField2" := "value", "nestedField" := "wasEmpty"))
      }
      it("Should create structure if nested value not found found") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$modify(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField" := "wasEmpty"))
      }
      it("Should modify a null value") {
        val pfN:Function[Option[DNullable[Int]], DNullable[Int]] = {
          case None => DNull
          case Some(DNull) => DSome(0)
          case Some(DSome(x)) => DSome(x + 1)
        }
        val base = DObject("nulled" := DNull)
        MaybeField.nulled.$modify(pfN)(base).right.value shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        MaybeField.nulled.$modify(pfN)(base2).right.value shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        MaybeField.nulled.$modify(pfN)(base3).right.value shouldBe DObject("nulled" := DNull)
      }
    }
    describe("$modifyOrDrop") {
      val pf:Function[Option[String], Option[String]] = {
        case None => Some("wasEmpty")
        case Some("drop") => None
        case Some(t) => Some(t + " found")
      }
      it("Should modify empty value") {
        val base = DObject.empty
        MaybeField.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        MaybeField.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("field" := "value found")
      }
      it("Should drop on a corresponding value") {
        val base = DObject("field" := "drop")
        MaybeField.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject.empty
      }
      it("Should incorrect type fail on a wrong type") {
        val base = DObject("field" := 123)
        MaybeField.field.$modifyOrDrop(pf)(base).left.value should contain (IncorrectTypeFailure(MaybeField.field, 123))
      }
      it("Should incorrect type fail on null") {
        val base = DObject("field" := DNull)
        MaybeField.field.$modifyOrDrop(pf)(base).left.value should contain (IncorrectTypeFailure(MaybeField.field, DNull))
      }
      it("Should treat as empty a wrong type when EmptyOnIncorrectTypeBehaviour") {
        val base = DObject("field" := 123)
        EmptyMaybeField.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }
      it("Should treat as empty a null when EmptyOnIncorrectTypeBehaviour") {
        val base = DObject("field" := DNull)
        EmptyMaybeField.field.$modifyOrDrop(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$modifyOrDrop(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField" := "value found"))
      }
      it("Should modify structure if nested value not found found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        MaybeField.nested.nestedField.$modifyOrDrop(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField2" := "value", "nestedField" := "wasEmpty"))
      }
      it("Should create structure if nested value not found found") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$modifyOrDrop(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField" := "wasEmpty"))
      }
      it("Should remove nested structure if nested field is dropped") {
        val base = DObject("nested" ::= ("nestedField" := "drop"))
        MaybeField.nested.nestedField.$modifyOrDrop(pf)(base).right.value shouldBe DObject.empty
      }
      it("Should modify a null value") {
        val pfN:Function[Option[DNullable[Int]], Option[DNullable[Int]]] = {
          case None => Some(DNull)
          case Some(DNull) => Some(DSome(0))
          case Some(DSome(0)) => None
          case Some(DSome(x)) => Some(DSome(x + 1))
        }

        val base = DObject("nulled" := DNull)
        MaybeField.nulled.$modifyOrDrop(pfN)(base).right.value shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        MaybeField.nulled.$modifyOrDrop(pfN)(base2).right.value shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        MaybeField.nulled.$modifyOrDrop(pfN)(base3).right.value shouldBe DObject("nulled" := DNull)
        val base4 = DObject("nulled" := 0)
        MaybeField.nulled.$modifyOrDrop(pfN)(base4).right.value shouldBe DObject.empty
      }
    }
    describe("$drop") {
      it("Not change if dropped field not present") {
        val base = DObject("leave" := 123)
        MaybeField.field.$drop(base) shouldBe base
      }
      it("Drop value if present") {
        val base = DObject("field" := "value")
        MaybeField.field.$drop(base) shouldBe DObject.empty
      }
      it("Drop value if wrong type") {
        val base = DObject("field" := 123)
        MaybeField.field.$drop(base) shouldBe DObject.empty
      }
      it("Drop nested value should clear object") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$drop(base) shouldBe DObject.empty
      }
      it("Drop nested value should not clear object if not empty") {
        val base = DObject("nested" ::= ("nestedField" := "value", "nestedFeld2" := 123))
        MaybeField.nested.nestedField.$drop(base) shouldBe DObject("nested" ::= ("nestedFeld2" := 123))
      }
    }
    describe("$setOrDrop") {
      it("should set an empty field") {
        val base = DObject.empty
        MaybeField.field.$setOrDrop(Some("test"))(base) should be(DObject("field" := "test"))
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        MaybeField.field.$setOrDrop(Some("test2"))(base) should be(DObject("field" := "test2"))
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        MaybeField.field.$setOrDrop(Some("test"))(base) should be(DObject("field" := "test"))
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$setOrDrop(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$setOrDrop(Some("value2"))(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        MaybeField.nested.nestedField.$setOrDrop(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        MaybeField.nested.nestedField.$setOrDrop(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        MaybeField.nested.nestedField.$setOrDrop(Some("value"))(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        MaybeField.nulled.$setOrDrop(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        MaybeField.nulled.$setOrDrop(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set value if nullable over a null value") {
        val base = DObject("nulled" := DNull)
        MaybeField.nulled.$setOrDrop(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set null over value ") {
        val base = DObject("nulled" := 123)
        MaybeField.nulled.$setOrDrop(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Not change if dropped field not present") {
        val base = DObject("leave" := 123)
        MaybeField.field.$setOrDrop(None)(base) shouldBe base
      }
      it("Drop value if present") {
        val base = DObject("field" := "value")
        MaybeField.field.$setOrDrop(None)(base) shouldBe DObject.empty
      }
      it("Drop value if wrong type") {
        val base = DObject("field" := 123)
        MaybeField.field.$setOrDrop(None)(base) shouldBe DObject.empty
      }
      it("Drop nested value should clear object") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$setOrDrop(None)(base) shouldBe DObject.empty
      }
      it("Drop nested value should not clear object if not empty") {
        val base = DObject("nested" ::= ("nestedField" := "value", "nestedFeld2" := 123))
        MaybeField.nested.nestedField.$setOrDrop(None)(base) shouldBe DObject("nested" ::= ("nestedFeld2" := 123))
      }
    }
    describe("$copy") {
      it("copying empty is empty") {
        val base = DObject.empty
        MaybeField.copy.$copy(MaybeField.field)(base).right.value shouldBe base
      }
      it("Copying an empty value to a set value will set value to empty") {
        val base = DObject("copy" := "leave")
        MaybeField.copy.$copy(MaybeField.field)(base).right.value shouldBe DObject.empty
      }
      it("Copying a wrong type to a set value is incorrect type failure") {
        val base1 = DObject("field" := 123)
        MaybeField.copy.$copy(MaybeField.field)(base1).left.value should contain (IncorrectTypeFailure(MaybeField.field, 123))
        val base2 = DObject("copy" := "leave", "field" := 123)
        MaybeField.copy.$copy(MaybeField.field)(base2).left.value should contain (IncorrectTypeFailure(MaybeField.field, 123))
      }
      it("Copying a wrong type to a set value is treated as empty on EmptyOnIncorrectType") {
        val base1 = DObject("field" := 123)
        EmptyMaybeField.copy.$copy(EmptyMaybeField.field)(base1).right.value shouldBe base1
        val base2 = DObject("copy" := "leave", "field" := 123)
        EmptyMaybeField.copy.$copy(EmptyMaybeField.field)(base2).right.value shouldBe base1
      }
      it("Copying a null value to a set value  is incorrect type failure") {
        val base1 = DObject("field" := DNull)
        MaybeField.copy.$copy(MaybeField.field)(base1).left.value should contain (IncorrectTypeFailure(MaybeField.field, DNull))
        val base2 = DObject("copy" := "leave", "field" := DNull)
        MaybeField.copy.$copy(MaybeField.field)(base2).left.value should contain (IncorrectTypeFailure(MaybeField.field, DNull))
      }
      it("Copying to an empty value will copy") {
        val base = DObject("field" := "copyMe")
        MaybeField.copy.$copy(MaybeField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying to a set value will override") {
        val base = DObject("field" := "copyMe", "copy" := "replaceMe")
        MaybeField.copy.$copy(MaybeField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying over a value of the wrong type will replace") {
        val base = DObject("field" := "copyMe", "copy" := 123)
        MaybeField.copy.$copy(MaybeField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying into a nested value will create nested structure") {
        val base = DObject("field" := "copyMe")
        MaybeField.nested.nestedField.$copy(MaybeField.field)(base).right.value shouldBe DObject("field" := "copyMe", "nested" ::= ("nestedField" := "copyMe"))
      }
      it("Copying into a nested value with empty will not create nested structure") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$copy(MaybeField.field)(base).right.value shouldBe base
      }
      it("Copying into a nested value with wrong type is incorrect type failure") {
        val base = DObject("field" := 123)
        MaybeField.nested.nestedField.$copy(MaybeField.field)(base).left.value should contain (IncorrectTypeFailure(MaybeField.field, 123))
      }
      it("Copying into a nested value with wrong type is will not create structure when EmptyOnIncorrectType") {
        val base = DObject("field" := 123)
        MaybeField.nested.nestedField.$copy(EmptyMaybeField.field)(base).right.value shouldBe base
      }
      it("Copying a from a nested structure into a nested structure will work") {
        val base = DObject("nested" ::= ("nestedCopy" := "copyMe"))
        MaybeField.nested.nestedField.$copy(MaybeField.nested.nestedCopy)(base).right.value shouldBe DObject("nested" ::= ("nestedCopy" := "copyMe", "nestedField" := "copyMe"))
      }
      it("Copying an expected value") {
        val base = DObject("expectedCopied" := "test2")
        MaybeField.copy.$copy(MaybeField.expectedCopied)(base).right.value shouldBe DObject("expectedCopied" := "test2", "copy" := "test2")
      }
      it("Copying an expected value will fail expected when empty") {
        val base = DObject.empty
        MaybeField.copy.$copy(MaybeField.expectedCopied)(base).left.value should contain (ExpectedFailure(MaybeField.expectedCopied))
      }
      it("Copying an expected value will fail incorrect type when incorrect type") {
        val base = DObject("expectedCopied" := 123)
        MaybeField.copy.$copy(MaybeField.expectedCopied)(base).left.value should contain (IncorrectTypeFailure(MaybeField.expectedCopied, 123))
      }
      it("Copying an expected value will fail expected when EmptyOnIncorrectType") {
        val base = DObject("expectedCopied" := 123)
        MaybeField.copy.$copy(EmptyMaybeField.expectedCopied)(base).left.value should contain (ExpectedFailure(EmptyMaybeField.expectedCopied))
      }
      it("Copying an empty default value should set as default") {
        val base = DObject.empty
        MaybeField.copy.$copy(MaybeField.defaultCopied)(base).right.value shouldBe DObject("copy" := "default")
      }
      it("Copying a set default value should set as default value") {
        val base = DObject("defaultCopied" := "notDefault")
        MaybeField.copy.$copy(MaybeField.defaultCopied)(base).right.value  shouldBe DObject("defaultCopied" := "notDefault", "copy" := "notDefault")
      }
      it("Copying a default value set as the wrong type should fail with incorrect type)") {
        val base = DObject("defaultCopied" := 1234)
        MaybeField.copy.$copy(MaybeField.defaultCopied)(base).left.value should contain (IncorrectTypeFailure(MaybeField.defaultCopied, 1234))
      }
      it("Copying a default value set as the wrong type should set default when EmptyOnIncorrectType)") {
        val base = DObject("defaultCopied" := 1234)
        MaybeField.copy.$copy(EmptyMaybeField.defaultCopied)(base).right.value shouldBe DObject("defaultCopied" := 1234, "copy" := "default")
      }
    }
  }

  object DefaultField extends Contract {
    val field = \![String]("defaultValue")
    val copy = \![String]("default")
    val maybeCopied = \?[String]
    val expectedCopied = \[String]
    val nested = new \\? {
      val nestedField = \![String]("defaultValue")
      val nestedCopy = \![String]("defaultValue")
    }
    val nulled = \![DNullable[Int]](DSome(23))
  }

  object EmptyDefaultField extends Contract with EmptyOnIncorrectType {
    val field = \![String]("defaultValue")
    val copy = \![String]("default")
    val expectedCopied = \[String]
    val maybeCopied = \?[String]
    val nested = new \\?  {
      val nestedField = \![String]("defaultValue")
      val nestedCopy = \![String]("defaultValue")
    }
    val nulled = \?[DNullable[Int]]
  }

  describe("default lens") {
    describe("$set") {
      it("should set an empty field") {
        val base = DObject.empty
        DefaultField.field.$set("test")(base) should be(DObject("field" := "test"))
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        DefaultField.field.$set("test2")(base) should be(DObject("field" := "test2"))
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        DefaultField.field.$set("test")(base) should be(DObject("field" := "test"))
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$set("value2")(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        DefaultField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        DefaultField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        DefaultField.nested.nestedField.$set("value")(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set value if nullable over a null value") {
        val base = DObject("nulled" := DNull)
        DefaultField.nulled.$set(DSome(132))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set null over value ") {
        val base = DObject("nulled" := 123)
        DefaultField.nulled.$set(DNull)(base) shouldBe DObject("nulled" := DNull)
      }
    }
    describe("$get") {
      it("Should return default if empty value") {
        DefaultField.field.$get(DObject.empty).right.value shouldBe "defaultValue"
      }
      it("Should return incorrect type failure if null value") {
        DefaultField.field.$get(DObject("field" := DNull)).left.value should contain (IncorrectTypeFailure(DefaultField.field, DNull))
      }
      it("Should return default value if null value and EmptyOnIncorrectType") {
        EmptyDefaultField.field.$get(DObject("field" := DNull)).right.value shouldBe "defaultValue"
      }
      it("Should return incorrect type failure if value is of the wrong type") {
        DefaultField.field.$get(DObject("field" := false)).left.value should contain (IncorrectTypeFailure(DefaultField.field, false))
      }
      it("Should return default value if wrong type and EmptyOnIncorrectType") {
        EmptyDefaultField.field.$get(DObject("field" := false)).right.value shouldBe "defaultValue"
      }
      it("Should return value if set") {
        DefaultField.field.$get(DObject("field" := "test")).right.value shouldBe "test"
      }
      it("Should return nested field value") {
        DefaultField.nested.nestedField.$get(DObject("nested" ::= ("nestedField" := "value"))).right.value shouldBe "value"
      }
      it("Should return default if nullable empty") {
        DefaultField.nulled.$get(DObject.empty).right.value shouldBe DSome(23)
      }
      it("Should return incorrect type failure if nullable wrong type") {
        DefaultField.nulled.$get(DObject("nulled" := "wrong")).left.value should contain (IncorrectTypeFailure(DefaultField.nulled, "wrong"))
      }
      it("Should return null if nullable null") {
        DefaultField.nulled.$get(DObject("nulled" := DNull)).right.value shouldBe DNull
      }
      it("Should return DSome Value if nullable set") {
        DefaultField.nulled.$get(DObject("nulled" := 123)).right.value shouldBe DSome(123)
      }
    }
    describe("$maybeSet") {
      it("Should not change nothing if not set") {
        val base = DObject.empty
        DefaultField.field.$maybeSet(None)(base) shouldBe DObject.empty
      }
      it("Should not alter a value if not set") {
        val base = DObject("field" := "test")
        DefaultField.field.$maybeSet(None)(base) shouldBe base
      }
      it("Should not alter a value if not set and value of wrong type") {
        val base = DObject("field" := 123)
        DefaultField.field.$maybeSet(None)(base) shouldBe base
      }
      it("Should alter a value if empty") {
        val base = DObject.empty
        DefaultField.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
      }
      it("Should alter a value if set") {
        val base = DObject("field" := "test")
        DefaultField.field.$maybeSet(Some("test2"))(base) shouldBe DObject("field" := "test2")
      }
      it("Should alter if value of wrong type") {
        val base = DObject("field" := 123)
        DefaultField.field.$maybeSet(Some("test"))(base) shouldBe DObject("field" := "test")
      }
      it("Should not create the nested field if internal is not set") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$maybeSet(None)(base) shouldBe base
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$maybeSet(Some("value2"))(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        DefaultField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        DefaultField.nested.nestedField.$maybeSet(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        DefaultField.nested.nestedField.$maybeSet(Some("value"))(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$maybeSet(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$maybeSet(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should not set nullable if empty") {
        val base = DObject.empty
        DefaultField.nulled.$maybeSet(None)(base) shouldBe base
      }
    }
    describe("$modify") {
      val pf:Function[String, String] = {
        case "defaultValue" => "wasEmpty"
        case t => t + " found"
      }
      it("Should modify default value") {
        val base = DObject.empty
        DefaultField.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        DefaultField.field.$modify(pf)(base).right.value shouldBe DObject("field" := "value found")
      }
      it("Should fail with incorrect type when type is wrong") {
        val base = DObject("field" := 123)
        DefaultField.field.$modify(pf)(base).left.value should contain (IncorrectTypeFailure(DefaultField.field, 123))
      }
      it("Should modify default when type is wrong and EmptyOnIncorrect") {
        val base = DObject("field" := 123)
        EmptyDefaultField.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }
      it("Should fail with incorrect type when type is null") {
        val base = DObject("field" := DNull)
        DefaultField.field.$modify(pf)(base).left.value should contain (IncorrectTypeFailure(DefaultField.field, DNull))
      }
      it("Should modify default when value is Null and EmptyOnIncorrect") {
        val base = DObject("field" := DNull)
        EmptyDefaultField.field.$modify(pf)(base).right.value shouldBe DObject("field" := "wasEmpty")
      }

      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$modify(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField" := "value found"))
      }
      it("Should modify structure if nested value not found found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        DefaultField.nested.nestedField.$modify(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField2" := "value", "nestedField" := "wasEmpty"))
      }
      it("Should create structure if nested value not found found") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$modify(pf)(base).right.value shouldBe DObject("nested" ::= ("nestedField" := "wasEmpty"))
      }
      it("Should modify a null value") {
        val pfN:Function[DNullable[Int], DNullable[Int]] = {
          case DNull => DSome(0)
          case DSome(x) => DSome(x + 1)
        }
        val base = DObject("nulled" := DNull)
        DefaultField.nulled.$modify(pfN)(base).right.value shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        DefaultField.nulled.$modify(pfN)(base2).right.value shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        DefaultField.nulled.$modify(pfN)(base3).right.value shouldBe DObject("nulled" := 24)
      }
    }
    describe("$restore") {
      it("Not change if dropped field not present") {
        val base = DObject("leave" := 123)
        DefaultField.field.$restore(base) shouldBe base
      }
      it("Drop value if present") {
        val base = DObject("field" := "value")
        DefaultField.field.$restore(base) shouldBe DObject.empty
      }
      it("Drop value if wrong type") {
        val base = DObject("field" := 123)
        DefaultField.field.$restore(base) shouldBe DObject.empty
      }
      it("Drop nested value should clear object") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$restore(base) shouldBe DObject.empty
      }
      it("Drop nested value should not clear object if not empty") {
        val base = DObject("nested" ::= ("nestedField" := "value", "nestedFeld2" := 123))
        DefaultField.nested.nestedField.$restore(base) shouldBe DObject("nested" ::= ("nestedFeld2" := 123))
      }
    }
    describe("$setOrRestore") {
      it("should set an empty field") {
        val base = DObject.empty
        DefaultField.field.$setOrRestore(Some("test"))(base) should be(DObject("field" := "test"))
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        DefaultField.field.$setOrRestore(Some("test2"))(base) should be(DObject("field" := "test2"))
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        DefaultField.field.$setOrRestore(Some("test"))(base) should be(DObject("field" := "test"))
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$setOrRestore(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$setOrRestore(Some("value2"))(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        DefaultField.nested.nestedField.$setOrRestore(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        DefaultField.nested.nestedField.$setOrRestore(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        DefaultField.nested.nestedField.$setOrRestore(Some("value"))(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$setOrRestore(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$setOrRestore(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set value if nullable over a null value") {
        val base = DObject("nulled" := DNull)
        DefaultField.nulled.$setOrRestore(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set null over value ") {
        val base = DObject("nulled" := 123)
        DefaultField.nulled.$setOrRestore(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Not change if dropped field not present") {
        val base = DObject("leave" := 123)
        DefaultField.field.$setOrRestore(None)(base) shouldBe base
      }
      it("Drop value if present") {
        val base = DObject("field" := "value")
        DefaultField.field.$setOrRestore(None)(base) shouldBe DObject.empty
      }
      it("Drop value if wrong type") {
        val base = DObject("field" := 123)
        DefaultField.field.$setOrRestore(None)(base) shouldBe DObject.empty
      }
      it("Drop nested value should clear object") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$setOrRestore(None)(base) shouldBe DObject.empty
      }
      it("Drop nested value should not clear object if not empty") {
        val base = DObject("nested" ::= ("nestedField" := "value", "nestedFeld2" := 123))
        DefaultField.nested.nestedField.$setOrRestore(None)(base) shouldBe DObject("nested" ::= ("nestedFeld2" := 123))
      }
    }
    describe("$copy") {
      it("copying empty is default value") {
        val base = DObject.empty
        DefaultField.copy.$copy(DefaultField.field)(base).right.value shouldBe DObject("copy" := "defaultValue")
      }
      it("Copying an empty value to a set value will set default") {
        val base = DObject("copy" := "leave")
        DefaultField.copy.$copy(DefaultField.field)(base).right.value shouldBe DObject("copy" := "defaultValue")
      }
      it("Copying a wrong type to a set value will fail with incorrect type") {
        val base1 = DObject("field" := 123)
        DefaultField.copy.$copy(DefaultField.field)(base1).left.value should contain (IncorrectTypeFailure(DefaultField.field, 123))
        val base2 = DObject("copy" := "leave", "field" := 123)
        DefaultField.copy.$copy(DefaultField.field)(base2).left.value should contain (IncorrectTypeFailure(DefaultField.field, 123))
      }
      it("Copying a wrong type to a set value will set default on EmptyOnIncorrectType") {
        val base1 = DObject("field" := 123)
        EmptyDefaultField.copy.$copy(EmptyDefaultField.field)(base1).right.value shouldBe DObject("copy" := "defaultValue", "field" := 123)
        val base2 = DObject("copy" := "leave", "field" := 123)
        EmptyDefaultField.copy.$copy(EmptyDefaultField.field)(base2).right.value shouldBe DObject("copy" := "defaultValue", "field" := 123)
      }
      it("Copying a null to a set value will fail with incorrect type") {
        val base1 = DObject("field" := DNull)
        DefaultField.copy.$copy(DefaultField.field)(base1).left.value should contain (IncorrectTypeFailure(DefaultField.field, DNull))
        val base2 = DObject("copy" := "leave", "field" := DNull)
        DefaultField.copy.$copy(DefaultField.field)(base2).left.value should contain (IncorrectTypeFailure(DefaultField.field, DNull))
      }
      it("Copying a null to a set value will set default on EmptyOnIncorrectType") {
        val base1 = DObject("field" := DNull)
        EmptyDefaultField.copy.$copy(EmptyDefaultField.field)(base1).right.value shouldBe DObject("copy" := "defaultValue", "field" := DNull)
        val base2 = DObject("copy" := "leave", "field" := DNull)
        EmptyDefaultField.copy.$copy(EmptyDefaultField.field)(base2).right.value shouldBe DObject("copy" := "defaultValue", "field" := DNull)
      }
      it("Copying to an empty value will copy") {
        val base = DObject("field" := "copyMe")
        DefaultField.copy.$copy(DefaultField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying to a set value will override") {
        val base = DObject("field" := "copyMe", "copy" := "replaceMe")
        DefaultField.copy.$copy(DefaultField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying over a value of the wrong type will replace") {
        val base = DObject("field" := "copyMe", "copy" := 123)
        DefaultField.copy.$copy(DefaultField.field)(base).right.value shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying into a nested value will create nested structure") {
        val base = DObject("field" := "copyMe")
        DefaultField.nested.nestedField.$copy(DefaultField.field)(base).right.value shouldBe DObject("field" := "copyMe", "nested" ::= ("nestedField" := "copyMe"))
      }
      it("Copying into a nested value with empty will create nested structure with default") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$copy(DefaultField.field)(base).right.value shouldBe DObject("nested" ::= ("nestedField" := "defaultValue"))
      }
      it("Copying into a nested value with wrong type will fail with incorrect type") {
        val base = DObject("field" := 123)
        DefaultField.nested.nestedField.$copy(DefaultField.field)(base).left.value should contain (IncorrectTypeFailure(DefaultField.field, 123))
      }
      it("Copying into a nested value with wrong type will create structure when EmptyOnIncorrectType") {
        val base = DObject("field" := 123)
        EmptyDefaultField.nested.nestedField.$copy(EmptyDefaultField.field)(base).right.value shouldBe DObject("field" := 123, "nested" ::= ("nestedField" := "defaultValue"))
      }
      it("Copying a from a nested structure into a nested structure will work") {
        val base = DObject("nested" ::= ("nestedCopy" := "copyMe"))
        DefaultField.nested.nestedField.$copy(DefaultField.nested.nestedCopy)(base).right.value shouldBe DObject("nested" ::= ("nestedCopy" := "copyMe", "nestedField" := "copyMe"))
      }
      it("Copying a maybe field will copy the value") {
        val base = DObject("maybeCopied" := "test2")
        DefaultField.copy.$copy(DefaultField.maybeCopied)(base).right.value shouldBe DObject("maybeCopied" := "test2", "copy" := "test2")
      }
      it("Copying an empty maybe field will set as empty") {
        val base = DObject("copy" := "test")
        DefaultField.copy.$copy(DefaultField.maybeCopied)(base).right.value  shouldBe DObject.empty
      }
      it("Copying an empty expected value should fail with expected ") {
        val base = DObject.empty
        DefaultField.copy.$copy(DefaultField.expectedCopied)(base).left.value should contain (ExpectedFailure(DefaultField.expectedCopied))
      }
      it("Copying an expected value should copy ") {
        val base = DObject("expectedCopied" := "myValue")
        DefaultField.copy.$copy(DefaultField.expectedCopied)(base).right.value shouldBe DObject("expectedCopied" := "myValue", "copy" := "myValue")
      }
      it("Copying a set default value to itself when empty will set as the default value") {
        val base = DObject.empty
        DefaultField.field.$copy(DefaultField.field)(base).right.value shouldBe DObject("field" := "defaultValue")
      }
    }
  }

}
