package dsentric.contracts

import dsentric._
import org.scalatest.{FunSpec, Matchers}

class PropertyLensTests extends FunSpec with Matchers with FailureMatchers {

  import Dsentric._
  import PessimisticCodecs._

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
      it("Should return empty if empty value") {
        ExpectedField.field.$get(DObject.empty) shouldBe empty
      }
      it("Should return empty if null value") {
        ExpectedField.field.$get(DObject("field" := DNull)) shouldBe empty
      }
      it("Should return empty if value is of the wrong type") {
        ExpectedField.field.$get(DObject("field" := false)) shouldBe empty
      }
      it("Should return value if set") {
        ExpectedField.field.$get(DObject("field" := "test")) should contain ("test")
      }
      it("Should return nested object") {
        ExpectedField.nested.$get(DObject("nested" ::= ("nestedField" := "value"))) should contain (DObject("nestedField" := "value"))
      }
      it("Should return nested field value") {
        ExpectedField.nested.nestedField.$get(DObject("nested" ::= ("nestedField" := "value"))) should contain ("value")
      }
      it("Should return empty if nullable empty") {
        ExpectedField.nulled.$get(DObject.empty) shouldBe empty
      }
      it("Should return empty if nullable wrong type") {
        ExpectedField.nulled.$get(DObject("nulled" := "wrong")) shouldBe empty
      }
      it("Should return null if nullable null") {
        ExpectedField.nulled.$get(DObject("nulled" := DNull)) should contain (DNull)
      }
      it("Should return DSome Value if nullable set") {
        ExpectedField.nulled.$get(DObject("nulled" := 123)) should contain (DSome(123))
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
      it("Should leave an empty value empty") {
        val base = DObject.empty
        ExpectedField.field.$modify(_ + "2")(base) shouldBe base
      }
      it("Should leave an incorrect type") {
        val base = DObject("field" := 123)
        ExpectedField.field.$modify(_ + "2")(base) shouldBe base
      }
      it("Should leave a null type") {
        val base = DObject("field" := DNull)
        ExpectedField.field.$modify(_ + "2")(base) shouldBe base
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        ExpectedField.field.$modify(_ + "2")(base) shouldBe DObject("field" := "value2")
      }
      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        ExpectedField.nested.nestedField.$modify(_ + "2")(base) shouldBe DObject("nested" ::= ("nestedField" := "value2"))
      }
      it("Should not modify structure if nested value not found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        ExpectedField.nested.nestedField.$modify(_ + "2")(base) shouldBe base
      }
      it("Should modify a null value") {
        val base = DObject("nulled" := DNull)
        ExpectedField.nulled.$modify{case DNull => DSome(0); case DSome(x) => DSome(x + 1)}(base) shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        ExpectedField.nulled.$modify{case DNull => DSome(0); case DSome(x) => DSome(x + 1)}(base2) shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        ExpectedField.nulled.$modify{case DNull => DSome(0); case DSome(x) => DSome(x + 1)}(base3) shouldBe base3
        val base4 = DObject("nulled" := "wrong")
        ExpectedField.nulled.$modify{case DNull => DSome(0); case DSome(x) => DSome(x + 1)}(base4) shouldBe base4
      }
    }
    describe("$copy") {
      it("copying empty is empty") {
        val base = DObject.empty
        ExpectedField.copy.$copy(ExpectedField.field)(base) shouldBe base
      }
      it("Copying an empty value to a set value is ignored") {
        val base = DObject("copy" := "leave")
        ExpectedField.copy.$copy(ExpectedField.field)(base) shouldBe base
      }
      it("Copying a wrong type to a set value is ignored") {
        val base1 = DObject("field" := 123)
        ExpectedField.copy.$copy(ExpectedField.field)(base1) shouldBe base1
        val base2 = DObject("copy" := "leave", "field" := 123)
        ExpectedField.copy.$copy(ExpectedField.field)(base2) shouldBe base2
      }
      it("Copying a null value to a set value is ignored") {
        val base1 = DObject("field" := DNull)
        ExpectedField.copy.$copy(ExpectedField.field)(base1) shouldBe base1
        val base2 = DObject("copy" := "leave", "field" := DNull)
        ExpectedField.copy.$copy(ExpectedField.field)(base2) shouldBe base2
      }
      it("Copying to an empty value will copy") {
        val base = DObject("field" := "copyMe")
        ExpectedField.copy.$copy(ExpectedField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying to a set value will override") {
        val base = DObject("field" := "copyMe", "copy" := "replaceMe")
        ExpectedField.copy.$copy(ExpectedField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying over a value of the wrong type will replace") {
        val base = DObject("field" := "copyMe", "copy" := 123)
        ExpectedField.copy.$copy(ExpectedField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying into a nested value will create nested structure") {
        val base = DObject("field" := "copyMe")
        ExpectedField.nested.nestedField.$copy(ExpectedField.field)(base) shouldBe DObject("field" := "copyMe", "nested" ::= ("nestedField" := "copyMe"))
      }
      it("Copying into a nested value with empty will not create nested structure") {
        val base = DObject.empty
        ExpectedField.nested.nestedField.$copy(ExpectedField.field)(base) shouldBe base
      }
      it("Copying into a nested value with wrong type will not create nested structure") {
        val base = DObject("field" := 123)
        ExpectedField.nested.nestedField.$copy(ExpectedField.field)(base) shouldBe base
      }
      it("Copying a from a nested structure into a nested structure will work") {
        val base = DObject("nested" ::= ("nestedCopy" := "copyMe"))
        ExpectedField.nested.nestedField.$copy(ExpectedField.nested.nestedCopy)(base) shouldBe DObject("nested" ::= ("nestedCopy" := "copyMe", "nestedField" := "copyMe"))
      }
      it("Copying a maybe field will copy the value") {
        val base = DObject("maybeCopied" := "test2")
        ExpectedField.copy.$copy(ExpectedField.maybeCopied)(base) shouldBe DObject("maybeCopied" := "test2", "copy" := "test2")
      }
      it("Copying an empty maybe field will ignore") {
        val base = DObject("copy" := "test")
        ExpectedField.copy.$copy(ExpectedField.maybeCopied)(base) shouldBe base
      }
      it("Copying an empty default value should set as default") {
        val base = DObject.empty
        ExpectedField.copy.$copy(ExpectedField.defaultCopied)(base) shouldBe DObject("copy" := "default")
      }
      it("Copying a set default value should set as default value") {
        val base = DObject("defaultCopied" := "notDefault")
        ExpectedField.copy.$copy(ExpectedField.defaultCopied)(base) shouldBe DObject("defaultCopied" := "notDefault", "copy" := "notDefault")
      }
      it("Copying a default value set as the wrong type should be ignored (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("defaultCopied" := 1234)
        ExpectedField.copy.$copy(ExpectedField.defaultCopied)(base) shouldBe base
      }
    }
  }

  object MaybeField extends Contract {
    val field = \?[String]
    val copy = \[String]
    val maybeCopied = \?[String]
    val defaultCopied = \![String]("default")
    val nested = new \\? {
      val nestedField = \?[String]
      val nestedCopy = \?[String]
    }
    val nulled = \?[DNullable[Int]]
  }

  describe("maybe lens") {
    describe("$set") {
      it("should set an empty field") {
        val base = DObject.empty
        MaybeField.field.$set("test")(base) should be(DObject("field" := "test"))
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        MaybeField.field.$set("test2")(base) should be(DObject("field" := "test2"))
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        MaybeField.field.$set("test")(base) should be(DObject("field" := "test"))
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$set("value2")(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        MaybeField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        MaybeField.nested.nestedField.$set("value")(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        MaybeField.nested.nestedField.$set("value")(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
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
        MaybeField.field.$get(DObject.empty) shouldBe empty
      }
      it("Should return empty if null value") {
        MaybeField.field.$get(DObject("field" := DNull)) shouldBe empty
      }
      it("Should return empty if value is of the wrong type") {
        MaybeField.field.$get(DObject("field" := false)) shouldBe empty
      }
      it("Should return value if set") {
        MaybeField.field.$get(DObject("field" := "test")) should contain ("test")
      }
      it("Should return nested object") {
        MaybeField.nested.$get(DObject("nested" ::= ("nestedField" := "value"))) should contain (DObject("nestedField" := "value"))
      }
      it("Should return nested field value") {
        MaybeField.nested.nestedField.$get(DObject("nested" ::= ("nestedField" := "value"))) should contain ("value")
      }
      it("Should return empty if nullable empty") {
        MaybeField.nulled.$get(DObject.empty) shouldBe empty
      }
      it("Should return empty if nullable wrong type") {
        MaybeField.nulled.$get(DObject("nulled" := "wrong")) shouldBe empty
      }
      it("Should return null if nullable null") {
        MaybeField.nulled.$get(DObject("nulled" := DNull)) should contain (DNull)
      }
      it("Should return DSome Value if nullable set") {
        MaybeField.nulled.$get(DObject("nulled" := 123)) should contain (DSome(123))
      }
    }
    describe("$getOrElse") {
      it("Should return orElse if empty value") {
        MaybeField.field.$getOrElse(DObject.empty, "orElse") shouldBe "orElse"
      }
      it("Should return orElse if value is of the wrong type") {
        MaybeField.field.$getOrElse(DObject("field" := false), "orElse") shouldBe "orElse"
      }
      it("Should return orElseif null value") {
        MaybeField.field.$getOrElse(DObject("field" := DNull), "orElse") shouldBe "orElse"
      }
      it("Should return value if set") {
        MaybeField.field.$getOrElse(DObject("field" := "test"), "orElse") shouldBe "test"
      }
      it("Should return nested object if set") {
        MaybeField.nested.$getOrElse(DObject("nested" ::= ("nestedField" := "value")), DObject.empty) shouldBe DObject(("nestedField" := "value"))
      }
      it("Should return object if nested not set") {
        MaybeField.nested.$getOrElse(DObject.empty, DObject.empty) shouldBe DObject.empty
      }
      it("Should return nested field value") {
        MaybeField.nested.nestedField.$getOrElse(DObject("nested" ::= ("nestedField" := "value")), "orElse") shouldBe "value"
      }
      it("Should return orElse if nullable empty") {
        MaybeField.nulled.$getOrElse(DObject.empty, DSome(123)) shouldBe DSome(123)
        MaybeField.nulled.$getOrElse(DObject.empty, DNull) shouldBe DNull
      }
      it("Should return orElse if nullable wrong type") {
        MaybeField.nulled.$getOrElse(DObject("nulled" := "wrong"), DSome(123)) shouldBe DSome(123)
      }
      it("Should return null if nullable null") {
        MaybeField.nulled.$getOrElse(DObject("nulled" := DNull), DSome(123)) shouldBe DNull
      }
      it("Should return DSome Value if nullable set") {
        MaybeField.nulled.$getOrElse(DObject("nulled" := 123), DSome(124)) shouldBe DSome(123)
      }
    }
    describe("$deltaGet") {
      it("Should return empty if value empty") {
        MaybeField.field.$deltaGet(DObject.empty) shouldBe empty
      }
      it("Should return empty if value is of the wrong type") {
        MaybeField.field.$deltaGet(DObject("field" := false)) shouldBe empty
      }
      it("Should return deltaRemove if null value") {
        MaybeField.field.$deltaGet(DObject("field" := DNull)) should contain (DeltaRemove)
      }
      it("Should return set value if set") {
        MaybeField.field.$deltaGet(DObject("field" := "test")) should contain (DeltaSet("test"))
      }
      it("Should return nested object") {
        MaybeField.nested.$deltaGet(DObject("nested" ::= ("nestedField" := "value"))) should contain (DeltaSet(DObject("nestedField" := "value")))
      }
      it("Should return nested field value") {
        MaybeField.nested.nestedField.$deltaGet(DObject("nested" ::= ("nestedField" := "value"))) should contain (DeltaSet("value"))
      }
      it("Should return empty object if empty object, empty objects under delta are a kind of remove however") {
        MaybeField.nested.$deltaGet(DObject("nested" ::= ())) should contain (DeltaSet(DObject.empty))
      }
      it("Should return empty if nullable empty") {
        MaybeField.nulled.$deltaGet(DObject.empty) shouldBe empty
      }
      it("Should return empty if nullable wrong type") {
        MaybeField.nulled.$deltaGet(DObject("nulled" := "wrong")) shouldBe empty
      }
      it("Should return null if nullable null") {
        MaybeField.nulled.$deltaGet(DObject("nulled" := DNull)) should contain (DeltaSet(DNull))
      }
      it("Should return DSome Value if nullable set") {
        MaybeField.nulled.$deltaGet(DObject("nulled" := 123)) should contain (DeltaSet(DSome(123)))
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
        MaybeField.field.$modify(pf)(base) shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        MaybeField.field.$modify(pf)(base) shouldBe DObject("field" := "value found")
      }
      it("Should leave a wrong type (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("field" := 123)
        MaybeField.field.$modify(pf)(base) shouldBe base
      }
      it("Should leave a null type (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("field" := DNull)
        MaybeField.field.$modify(pf)(base) shouldBe base
      }

      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$modify(pf)(base) shouldBe DObject("nested" ::= ("nestedField" := "value found"))
      }
      it("Should modify structure if nested value not found found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        MaybeField.nested.nestedField.$modify(pf)(base) shouldBe DObject("nested" ::= ("nestedField2" := "value", "nestedField" := "wasEmpty"))
      }
      it("Should create structure if nested value not found found") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$modify(pf)(base) shouldBe DObject("nested" ::= ("nestedField" := "wasEmpty"))
      }
      it("Should modify a null value") {
        val pfN:Function[Option[DNullable[Int]], DNullable[Int]] = {
          case None => DNull
          case Some(DNull) => DSome(0)
          case Some(DSome(x)) => DSome(x + 1)
        }
        val base = DObject("nulled" := DNull)
        MaybeField.nulled.$modify(pfN)(base) shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        MaybeField.nulled.$modify(pfN)(base2) shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        MaybeField.nulled.$modify(pfN)(base3) shouldBe DObject("nulled" := DNull)
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
        MaybeField.field.$modifyOrDrop(pf)(base) shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        MaybeField.field.$modifyOrDrop(pf)(base) shouldBe DObject("field" := "value found")
      }
      it("Should drop on a corresponding value") {
        val base = DObject("field" := "drop")
        MaybeField.field.$modifyOrDrop(pf)(base) shouldBe DObject.empty
      }
      it("Should leave a wrong type (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("field" := 123)
        MaybeField.field.$modifyOrDrop(pf)(base) shouldBe base
      }
      it("Should leave a null type (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("field" := DNull)
        MaybeField.field.$modifyOrDrop(pf)(base) shouldBe base
      }
      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$modifyOrDrop(pf)(base) shouldBe DObject("nested" ::= ("nestedField" := "value found"))
      }
      it("Should modify structure if nested value not found found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        MaybeField.nested.nestedField.$modifyOrDrop(pf)(base) shouldBe DObject("nested" ::= ("nestedField2" := "value", "nestedField" := "wasEmpty"))
      }
      it("Should create structure if nested value not found found") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$modifyOrDrop(pf)(base) shouldBe DObject("nested" ::= ("nestedField" := "wasEmpty"))
      }
      it("Should remove nested structure if nested field is dropped") {
        val base = DObject("nested" ::= ("nestedField" := "drop"))
        MaybeField.nested.nestedField.$modifyOrDrop(pf)(base) shouldBe DObject.empty
      }
      it("Should modify a null value") {
        val pfN:Function[Option[DNullable[Int]], Option[DNullable[Int]]] = {
          case None => Some(DNull)
          case Some(DNull) => Some(DSome(0))
          case Some(DSome(0)) => None
          case Some(DSome(x)) => Some(DSome(x + 1))
        }

        val base = DObject("nulled" := DNull)
        MaybeField.nulled.$modifyOrDrop(pfN)(base) shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        MaybeField.nulled.$modifyOrDrop(pfN)(base2) shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        MaybeField.nulled.$modifyOrDrop(pfN)(base3) shouldBe DObject("nulled" := DNull)
        val base4 = DObject("nulled" := 0)
        MaybeField.nulled.$modifyOrDrop(pfN)(base4) shouldBe DObject.empty
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
        MaybeField.copy.$copy(MaybeField.field)(base) shouldBe base
      }
      it("Copying an empty value to a set value will set value to empty") {
        val base = DObject("copy" := "leave")
        MaybeField.copy.$copy(MaybeField.field)(base) shouldBe DObject.empty
      }
      it("Copying a wrong type to a set value is ignored") {
        val base1 = DObject("field" := 123)
        MaybeField.copy.$copy(MaybeField.field)(base1) shouldBe base1
        val base2 = DObject("copy" := "leave", "field" := 123)
        MaybeField.copy.$copy(MaybeField.field)(base2) shouldBe base2
      }
      it("Copying a null value to a set value is ignored") {
        val base1 = DObject("field" := DNull)
        MaybeField.copy.$copy(MaybeField.field)(base1) shouldBe base1
        val base2 = DObject("copy" := "leave", "field" := DNull)
        MaybeField.copy.$copy(MaybeField.field)(base2) shouldBe base2
      }
      it("Copying to an empty value will copy") {
        val base = DObject("field" := "copyMe")
        MaybeField.copy.$copy(MaybeField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying to a set value will override") {
        val base = DObject("field" := "copyMe", "copy" := "replaceMe")
        MaybeField.copy.$copy(MaybeField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying over a value of the wrong type will replace") {
        val base = DObject("field" := "copyMe", "copy" := 123)
        MaybeField.copy.$copy(MaybeField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying into a nested value will create nested structure") {
        val base = DObject("field" := "copyMe")
        MaybeField.nested.nestedField.$copy(MaybeField.field)(base) shouldBe DObject("field" := "copyMe", "nested" ::= ("nestedField" := "copyMe"))
      }
      it("Copying into a nested value with empty will not create nested structure") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$copy(MaybeField.field)(base) shouldBe base
      }
      it("Copying into a nested value with wrong type will not create nested structure") {
        val base = DObject("field" := 123)
        MaybeField.nested.nestedField.$copy(MaybeField.field)(base) shouldBe base
      }
      it("Copying a from a nested structure into a nested structure will work") {
        val base = DObject("nested" ::= ("nestedCopy" := "copyMe"))
        MaybeField.nested.nestedField.$copy(MaybeField.nested.nestedCopy)(base) shouldBe DObject("nested" ::= ("nestedCopy" := "copyMe", "nestedField" := "copyMe"))
      }
      it("Copying a maybe field will copy the value") {
        val base = DObject("maybeCopied" := "test2")
        MaybeField.copy.$copy(MaybeField.maybeCopied)(base) shouldBe DObject("maybeCopied" := "test2", "copy" := "test2")
      }
      it("Copying an empty maybe field will ignore") {
        val base = DObject("copy" := "test")
        MaybeField.copy.$copy(MaybeField.maybeCopied)(base) shouldBe base
      }
      it("Copying an empty default value should set as default") {
        val base = DObject.empty
        MaybeField.copy.$copy(MaybeField.defaultCopied)(base) shouldBe DObject("copy" := "default")
      }
      it("Copying a set default value should set as default value") {
        val base = DObject("defaultCopied" := "notDefault")
        MaybeField.copy.$copy(MaybeField.defaultCopied)(base) shouldBe DObject("defaultCopied" := "notDefault", "copy" := "notDefault")
      }
      it("Copying a default value set as the wrong type should be ignored (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("defaultCopied" := 1234)
        MaybeField.copy.$copy(MaybeField.defaultCopied)(base) shouldBe base
      }
    }
    describe("$setNull") {
      it("should set an empty field") {
        val base = DObject.empty
        MaybeField.field.$setNull(base) should be(DObject("field" := DNull))
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        MaybeField.field.$setNull(base) should be(DObject("field" := DNull))
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        MaybeField.field.$setNull(base) should be(DObject("field" := DNull))
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        MaybeField.nested.nestedField.$setNull(base) should be(DObject("nested" ::= ("nestedField" := DNull)))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        MaybeField.nested.nestedField.$setNull(base) should be(DObject("nested" ::= ("nestedField" := DNull)))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        MaybeField.nested.nestedField.$setNull(base) should be(DObject("nested" ::= ("nestedField" := DNull)))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        MaybeField.nested.nestedField.$setNull(base) should be(DObject("nested" ::= ("nestedField" := DNull, "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        MaybeField.nested.nestedField.$setNull(base2) should be(DObject("nested" ::= ("nestedField" := DNull, "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        MaybeField.nulled.$setNull(base) shouldBe DObject("nulled" := DNull)
      }
    }
  }

  object DefaultField extends Contract {
    val field = \![String]("defaultValue")
    val copy = \[String]
    val maybeCopied = \?[String]
    val defaultCopied = \![String]("default")
    val nested = new \\? {
      val nestedField = \![String]("defaultValue")
      val nestedCopy = \![String]("defaultValue")
    }
    val nulled = \![DNullable[Int]](DSome(23))
  }

  describe("deafult lens") {
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
        DefaultField.field.$get(DObject.empty) shouldBe "defaultValue"
      }
      it("Should return default if null value") {
        DefaultField.field.$get(DObject("field" := DNull)) shouldBe "defaultValue"
      }
      it("Should return empty if value is of the wrong type") {
        DefaultField.field.$get(DObject("field" := false)) shouldBe empty
      }
      it("Should return value if set") {
        DefaultField.field.$get(DObject("field" := "test")) should contain ("test")
      }
      it("Should return nested object") {
        DefaultField.nested.$get(DObject("nested" ::= ("nestedField" := "value"))) should contain (DObject("nestedField" := "value"))
      }
      it("Should return nested field value") {
        DefaultField.nested.nestedField.$get(DObject("nested" ::= ("nestedField" := "value"))) should contain ("value")
      }
      it("Should return empty if nullable empty") {
        DefaultField.nulled.$get(DObject.empty) shouldBe empty
      }
      it("Should return empty if nullable wrong type") {
        DefaultField.nulled.$get(DObject("nulled" := "wrong")) shouldBe empty
      }
      it("Should return null if nullable null") {
        DefaultField.nulled.$get(DObject("nulled" := DNull)) should contain (DNull)
      }
      it("Should return DSome Value if nullable set") {
        DefaultField.nulled.$get(DObject("nulled" := 123)) should contain (DSome(123))
      }
    }
    describe("$getOrElse") {
      it("Should return orElse if empty value") {
        DefaultField.field.$getOrElse(DObject.empty, "orElse") shouldBe "orElse"
      }
      it("Should return orElse if value is of the wrong type") {
        DefaultField.field.$getOrElse(DObject("field" := false), "orElse") shouldBe "orElse"
      }
      it("Should return orElseif null value") {
        DefaultField.field.$getOrElse(DObject("field" := DNull), "orElse") shouldBe "orElse"
      }
      it("Should return value if set") {
        DefaultField.field.$getOrElse(DObject("field" := "test"), "orElse") shouldBe "test"
      }
      it("Should return nested object if set") {
        DefaultField.nested.$getOrElse(DObject("nested" ::= ("nestedField" := "value")), DObject.empty) shouldBe DObject(("nestedField" := "value"))
      }
      it("Should return object if nested not set") {
        DefaultField.nested.$getOrElse(DObject.empty, DObject.empty) shouldBe DObject.empty
      }
      it("Should return nested field value") {
        DefaultField.nested.nestedField.$getOrElse(DObject("nested" ::= ("nestedField" := "value")), "orElse") shouldBe "value"
      }
      it("Should return orElse if nullable empty") {
        DefaultField.nulled.$getOrElse(DObject.empty, DSome(123)) shouldBe DSome(123)
        DefaultField.nulled.$getOrElse(DObject.empty, DNull) shouldBe DNull
      }
      it("Should return orElse if nullable wrong type") {
        DefaultField.nulled.$getOrElse(DObject("nulled" := "wrong"), DSome(123)) shouldBe DSome(123)
      }
      it("Should return null if nullable null") {
        DefaultField.nulled.$getOrElse(DObject("nulled" := DNull), DSome(123)) shouldBe DNull
      }
      it("Should return DSome Value if nullable set") {
        DefaultField.nulled.$getOrElse(DObject("nulled" := 123), DSome(124)) shouldBe DSome(123)
      }
    }
    describe("$deltaGet") {
      it("Should return empty if value empty") {
        DefaultField.field.$deltaGet(DObject.empty) shouldBe empty
      }
      it("Should return empty if value is of the wrong type") {
        DefaultField.field.$deltaGet(DObject("field" := false)) shouldBe empty
      }
      it("Should return deltaRemove if null value") {
        DefaultField.field.$deltaGet(DObject("field" := DNull)) should contain (DeltaRemove)
      }
      it("Should return set value if set") {
        DefaultField.field.$deltaGet(DObject("field" := "test")) should contain (DeltaSet("test"))
      }
      it("Should return nested object") {
        DefaultField.nested.$deltaGet(DObject("nested" ::= ("nestedField" := "value"))) should contain (DeltaSet(DObject("nestedField" := "value")))
      }
      it("Should return nested field value") {
        DefaultField.nested.nestedField.$deltaGet(DObject("nested" ::= ("nestedField" := "value"))) should contain (DeltaSet("value"))
      }
      it("Should return empty object if empty object, empty objects under delta are a kind of remove however") {
        DefaultField.nested.$deltaGet(DObject("nested" ::= ())) should contain (DeltaSet(DObject.empty))
      }
      it("Should return empty if nullable empty") {
        DefaultField.nulled.$deltaGet(DObject.empty) shouldBe empty
      }
      it("Should return empty if nullable wrong type") {
        DefaultField.nulled.$deltaGet(DObject("nulled" := "wrong")) shouldBe empty
      }
      it("Should return null if nullable null") {
        DefaultField.nulled.$deltaGet(DObject("nulled" := DNull)) should contain (DeltaSet(DNull))
      }
      it("Should return DSome Value if nullable set") {
        DefaultField.nulled.$deltaGet(DObject("nulled" := 123)) should contain (DeltaSet(DSome(123)))
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
      val pf:Function[Option[String], String] = {
        case None => "wasEmpty"
        case Some(t) => t + " found"
      }
      it("Should modify empty value") {
        val base = DObject.empty
        DefaultField.field.$modify(pf)(base) shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        DefaultField.field.$modify(pf)(base) shouldBe DObject("field" := "value found")
      }
      it("Should leave a wrong type (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("field" := 123)
        DefaultField.field.$modify(pf)(base) shouldBe base
      }
      it("Should leave a null type (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("field" := DNull)
        DefaultField.field.$modify(pf)(base) shouldBe base
      }

      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$modify(pf)(base) shouldBe DObject("nested" ::= ("nestedField" := "value found"))
      }
      it("Should modify structure if nested value not found found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        DefaultField.nested.nestedField.$modify(pf)(base) shouldBe DObject("nested" ::= ("nestedField2" := "value", "nestedField" := "wasEmpty"))
      }
      it("Should create structure if nested value not found found") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$modify(pf)(base) shouldBe DObject("nested" ::= ("nestedField" := "wasEmpty"))
      }
      it("Should modify a null value") {
        val pfN:Function[Option[DNullable[Int]], DNullable[Int]] = {
          case None => DNull
          case Some(DNull) => DSome(0)
          case Some(DSome(x)) => DSome(x + 1)
        }
        val base = DObject("nulled" := DNull)
        DefaultField.nulled.$modify(pfN)(base) shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        DefaultField.nulled.$modify(pfN)(base2) shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        DefaultField.nulled.$modify(pfN)(base3) shouldBe DObject("nulled" := DNull)
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
        DefaultField.field.$modifyOrDrop(pf)(base) shouldBe DObject("field" := "wasEmpty")
      }
      it("Should modify a set value") {
        val base = DObject("field" := "value")
        DefaultField.field.$modifyOrDrop(pf)(base) shouldBe DObject("field" := "value found")
      }
      it("Should drop on a corresponding value") {
        val base = DObject("field" := "drop")
        DefaultField.field.$modifyOrDrop(pf)(base) shouldBe DObject.empty
      }
      it("Should leave a wrong type (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("field" := 123)
        DefaultField.field.$modifyOrDrop(pf)(base) shouldBe base
      }
      it("Should leave a null type (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("field" := DNull)
        DefaultField.field.$modifyOrDrop(pf)(base) shouldBe base
      }
      it("Should modify a nested value") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$modifyOrDrop(pf)(base) shouldBe DObject("nested" ::= ("nestedField" := "value found"))
      }
      it("Should modify structure if nested value not found found") {
        val base = DObject("nested" ::= ("nestedField2" := "value"))
        DefaultField.nested.nestedField.$modifyOrDrop(pf)(base) shouldBe DObject("nested" ::= ("nestedField2" := "value", "nestedField" := "wasEmpty"))
      }
      it("Should create structure if nested value not found found") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$modifyOrDrop(pf)(base) shouldBe DObject("nested" ::= ("nestedField" := "wasEmpty"))
      }
      it("Should remove nested structure if nested field is dropped") {
        val base = DObject("nested" ::= ("nestedField" := "drop"))
        DefaultField.nested.nestedField.$modifyOrDrop(pf)(base) shouldBe DObject.empty
      }
      it("Should modify a null value") {
        val pfN:Function[Option[DNullable[Int]], Option[DNullable[Int]]] = {
          case None => Some(DNull)
          case Some(DNull) => Some(DSome(0))
          case Some(DSome(0)) => None
          case Some(DSome(x)) => Some(DSome(x + 1))
        }

        val base = DObject("nulled" := DNull)
        DefaultField.nulled.$modifyOrDrop(pfN)(base) shouldBe DObject("nulled" := 0)
        val base2 = DObject("nulled" := 123)
        DefaultField.nulled.$modifyOrDrop(pfN)(base2) shouldBe DObject("nulled" := 124)
        val base3 = DObject.empty
        DefaultField.nulled.$modifyOrDrop(pfN)(base3) shouldBe DObject("nulled" := DNull)
        val base4 = DObject("nulled" := 0)
        DefaultField.nulled.$modifyOrDrop(pfN)(base4) shouldBe DObject.empty
      }
    }
    describe("$drop") {
      it("Not change if dropped field not present") {
        val base = DObject("leave" := 123)
        DefaultField.field.$drop(base) shouldBe base
      }
      it("Drop value if present") {
        val base = DObject("field" := "value")
        DefaultField.field.$drop(base) shouldBe DObject.empty
      }
      it("Drop value if wrong type") {
        val base = DObject("field" := 123)
        DefaultField.field.$drop(base) shouldBe DObject.empty
      }
      it("Drop nested value should clear object") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$drop(base) shouldBe DObject.empty
      }
      it("Drop nested value should not clear object if not empty") {
        val base = DObject("nested" ::= ("nestedField" := "value", "nestedFeld2" := 123))
        DefaultField.nested.nestedField.$drop(base) shouldBe DObject("nested" ::= ("nestedFeld2" := 123))
      }
    }
    describe("$setOrDrop") {
      it("should set an empty field") {
        val base = DObject.empty
        DefaultField.field.$setOrDrop(Some("test"))(base) should be(DObject("field" := "test"))
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        DefaultField.field.$setOrDrop(Some("test2"))(base) should be(DObject("field" := "test2"))
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        DefaultField.field.$setOrDrop(Some("test"))(base) should be(DObject("field" := "test"))
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$setOrDrop(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$setOrDrop(Some("value2"))(base) should be(DObject("nested" ::= ("nestedField" := "value2")))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        DefaultField.nested.nestedField.$setOrDrop(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value")))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        DefaultField.nested.nestedField.$setOrDrop(Some("value"))(base) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        DefaultField.nested.nestedField.$setOrDrop(Some("value"))(base2) should be(DObject("nested" ::= ("nestedField" := "value", "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$setOrDrop(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Should set value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$setOrDrop(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set value if nullable over a null value") {
        val base = DObject("nulled" := DNull)
        DefaultField.nulled.$setOrDrop(Some(DSome(132)))(base) shouldBe DObject("nulled" := 132)
      }
      it("Should set null over value ") {
        val base = DObject("nulled" := 123)
        DefaultField.nulled.$setOrDrop(Some(DNull))(base) shouldBe DObject("nulled" := DNull)
      }
      it("Not change if dropped field not present") {
        val base = DObject("leave" := 123)
        DefaultField.field.$setOrDrop(None)(base) shouldBe base
      }
      it("Drop value if present") {
        val base = DObject("field" := "value")
        DefaultField.field.$setOrDrop(None)(base) shouldBe DObject.empty
      }
      it("Drop value if wrong type") {
        val base = DObject("field" := 123)
        DefaultField.field.$setOrDrop(None)(base) shouldBe DObject.empty
      }
      it("Drop nested value should clear object") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$setOrDrop(None)(base) shouldBe DObject.empty
      }
      it("Drop nested value should not clear object if not empty") {
        val base = DObject("nested" ::= ("nestedField" := "value", "nestedFeld2" := 123))
        DefaultField.nested.nestedField.$setOrDrop(None)(base) shouldBe DObject("nested" ::= ("nestedFeld2" := 123))
      }
    }
    describe("$copy") {
      it("copying empty is empty") {
        val base = DObject.empty
        DefaultField.copy.$copy(DefaultField.field)(base) shouldBe base
      }
      it("Copying an empty value to a set value is ignored") {
        val base = DObject("copy" := "leave")
        DefaultField.copy.$copy(DefaultField.field)(base) shouldBe base
      }
      it("Copying a wrong type to a set value is ignored") {
        val base1 = DObject("field" := 123)
        DefaultField.copy.$copy(DefaultField.field)(base1) shouldBe base1
        val base2 = DObject("copy" := "leave", "field" := 123)
        DefaultField.copy.$copy(DefaultField.field)(base2) shouldBe base2
      }
      it("Copying a null value to a set value is ignored") {
        val base1 = DObject("field" := DNull)
        DefaultField.copy.$copy(DefaultField.field)(base1) shouldBe base1
        val base2 = DObject("copy" := "leave", "field" := DNull)
        DefaultField.copy.$copy(DefaultField.field)(base2) shouldBe base2
      }
      it("Copying to an empty value will copy") {
        val base = DObject("field" := "copyMe")
        DefaultField.copy.$copy(DefaultField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying to a set value will override") {
        val base = DObject("field" := "copyMe", "copy" := "replaceMe")
        DefaultField.copy.$copy(DefaultField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying over a value of the wrong type will replace") {
        val base = DObject("field" := "copyMe", "copy" := 123)
        DefaultField.copy.$copy(DefaultField.field)(base) shouldBe DObject("field" := "copyMe", "copy" := "copyMe")
      }
      it("Copying into a nested value will create nested structure") {
        val base = DObject("field" := "copyMe")
        DefaultField.nested.nestedField.$copy(DefaultField.field)(base) shouldBe DObject("field" := "copyMe", "nested" ::= ("nestedField" := "copyMe"))
      }
      it("Copying into a nested value with empty will not create nested structure") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$copy(DefaultField.field)(base) shouldBe base
      }
      it("Copying into a nested value with wrong type will not create nested structure") {
        val base = DObject("field" := 123)
        DefaultField.nested.nestedField.$copy(DefaultField.field)(base) shouldBe base
      }
      it("Copying a from a nested structure into a nested structure will work") {
        val base = DObject("nested" ::= ("nestedCopy" := "copyMe"))
        DefaultField.nested.nestedField.$copy(DefaultField.nested.nestedCopy)(base) shouldBe DObject("nested" ::= ("nestedCopy" := "copyMe", "nestedField" := "copyMe"))
      }
      it("Copying a maybe field will copy the value") {
        val base = DObject("maybeCopied" := "test2")
        DefaultField.copy.$copy(DefaultField.maybeCopied)(base) shouldBe DObject("maybeCopied" := "test2", "copy" := "test2")
      }
      it("Copying an empty maybe field will ignore") {
        val base = DObject("copy" := "test")
        DefaultField.copy.$copy(DefaultField.maybeCopied)(base) shouldBe base
      }
      it("Copying an empty default value should set as default") {
        val base = DObject.empty
        DefaultField.copy.$copy(DefaultField.defaultCopied)(base) shouldBe DObject("copy" := "default")
      }
      it("Copying a set default value should set as default value") {
        val base = DObject("defaultCopied" := "notDefault")
        DefaultField.copy.$copy(DefaultField.defaultCopied)(base) shouldBe DObject("defaultCopied" := "notDefault", "copy" := "notDefault")
      }
      it("Copying a default value set as the wrong type should be ignored (IgnoreOnIncorrectTypeBehaviour)") {
        val base = DObject("defaultCopied" := 1234)
        DefaultField.copy.$copy(DefaultField.defaultCopied)(base) shouldBe base
      }
    }
    describe("$setNull") {
      it("should set an empty field") {
        val base = DObject.empty
        DefaultField.field.$setNull(base) should be(DObject("field" := DNull))
      }
      it("Should replace a set field") {
        val base = DObject("field" := "test")
        DefaultField.field.$setNull(base) should be(DObject("field" := DNull))
      }
      it("Should replace a set field of the wrong type") {
        val base = DObject("field" := false)
        DefaultField.field.$setNull(base) should be(DObject("field" := DNull))
      }
      it("Should create the nested field if internal is set") {
        val base = DObject.empty
        DefaultField.nested.nestedField.$setNull(base) should be(DObject("nested" ::= ("nestedField" := DNull)))
      }
      it("Should create the nested field if internal is altered") {
        val base = DObject("nested" ::= ("nestedField" := "value"))
        DefaultField.nested.nestedField.$setNull(base) should be(DObject("nested" ::= ("nestedField" := DNull)))
      }
      it("Should replace the nested field if it is of wrong type") {
        val base = DObject("nested" := 1)
        DefaultField.nested.nestedField.$setNull(base) should be(DObject("nested" ::= ("nestedField" := DNull)))
      }
      it("Should not effect sibling nested fields when being set or replaced") {
        val base = DObject("nested" ::= ("nestedField2" := 23))
        DefaultField.nested.nestedField.$setNull(base) should be(DObject("nested" ::= ("nestedField" := DNull, "nestedField2" := 23)))
        val base2 = DObject("nested" ::= ("nestedField" := "value2", "nestedField2" := 23))
        DefaultField.nested.nestedField.$setNull(base2) should be(DObject("nested" ::= ("nestedField" := DNull, "nestedField2" := 23)))
      }
      it("Should set null value if nullable") {
        val base = DObject.empty
        DefaultField.nulled.$setNull(base) shouldBe DObject("nulled" := DNull)
      }
    }
  }

}
