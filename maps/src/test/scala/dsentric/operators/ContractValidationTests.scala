package dsentric.operators

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractValidationTests extends FunSuite with Matchers with FailureMatchers {

  implicit def strictness = MaybePessimistic
  import Dsentric._
  import PessimisticCodecs._

  object Empty extends Contract with Validation

  def f(elems:(Path, String)*) =
    elems.toVector


  test("validation of contract type")  {

    Empty.$validate(DObject.empty) shouldBe Vector.empty
    Empty.$validate(DObject("key" := "value")) shouldBe Vector.empty
  }


  object ExpectedField extends Contract with Validation {

    val expGT = \[Int](Validators.>(5))
  }

  test("validation of expected field") {

    ExpectedField.$validate(DObject.empty) should be (f(Path("expGT") -> "Value is required."))
    ExpectedField.$validate(DObject("expGT" := false)) should be (f(Path("expGT") -> "Value is not of the expected type."))
    ExpectedField.$validate(DObject("expGT" := 7)) shouldBe Vector.empty
    ExpectedField.$validate(DObject("expGT" := 3)) should be (f(Path("expGT") -> "Value 3 is not greater than 5."))
  }

  object MaybeField extends Contract with Validation {
    implicit def strictness = MaybePessimistic
    val mayNonEmpty = \?[String](Validators.nonEmptyOrWhiteSpace)

  }

  test("validation of optional field") {
    MaybeField.$validate(DObject.empty) shouldBe Vector.empty
    MaybeField.$validate(DObject("mayNonEmpty" := false)) should be (f(Path("mayNonEmpty") -> "Value is not of the expected type."))
    MaybeField.$validate(DObject("mayNonEmpty" := "TEST")) shouldBe Vector.empty
    MaybeField.$validate(DObject("mayNonEmpty" := "")) should be (f(Path("mayNonEmpty") -> "String must not be empty or whitespace."))
  }

  object DefaultField extends Contract with Validation {
    implicit def strictness = MaybePessimistic
    val inDefault = \![String]("default", Validators.in("default", "one", "two"))
  }

  test("validation of default field") {
    DefaultField.$validate(DObject.empty) shouldBe Vector.empty
    DefaultField.$validate(DObject("inDefault" := false)) should be (f(Path("inDefault") -> "Value is not of the expected type."))
    DefaultField.$validate(DObject("inDeafult" := "two")) shouldBe Vector.empty
    DefaultField.$validate(DObject("inDefault" := "three")) should be (f(Path("inDefault") -> "'three' is not an allowed value."))
  }

  object NestedExpectedField extends Contract with Validation {

    val nested = new \\{
      val expected = \[String]
    }
  }

  test("validation of expected nested contract") {
    NestedExpectedField.$validate(DObject.empty) should be (f(Path("nested") -> "Value is required."))
    NestedExpectedField.$validate(DObject("nested" := DObject.empty)) should be (f(Path("nested", "expected") -> "Value is required."))
    NestedExpectedField.$validate(DObject("nested" := DObject("expected" := "value"))) shouldBe Vector.empty
  }

  object NestedMaybeField extends Contract with Validation {
    val nested = new \\?{
      val expected = \[String]
    }
  }

  test("validation of maybe nested contract") {
    NestedMaybeField.$validate(DObject.empty) shouldBe Vector.empty
    NestedMaybeField.$validate(DObject("nested" := DObject.empty)) should be (f(Path("nested", "expected") -> "Value is required."))
    NestedExpectedField.$validate(DObject("nested" := DObject("expected" := "value"))) shouldBe Vector.empty
  }

  test("Nested validation") {
    implicit def strictness = MaybePessimistic
    object NestValid extends Contract with Validation {
      val value1 = \[String]
      val nest1 = new \\ {
        val value2 = \[String]
        val value3 = \[String]
      }
      val nest2 = new \\? {
        val nest3 = new \\ {
          val value4 = \[String]
        }
        val value5 = \[String]
      }
    }

    val json1 = DObject("value1" := "V", "nest1" := DObject("value2" := "V", "value3" := "V"))
    NestValid.$validate(json1) shouldBe Vector.empty
    val json2 = DObject("value1" := "V", "nest1" := DObject("value2" := "V", "value3" := "V"), "nest2" := DObject("nest3" := DObject("value4" := "V"), "value5" := "V"))
    NestValid.$validate(json2) shouldBe Vector.empty

    NestValid.$validate(DObject("value1" := "V", "nest1" := DObject("value3" := 3))).toSet shouldBe Set("nest1"\"value2" -> "Value is required.", "nest1"\"value3" -> "Value is not of the expected type.")

    NestValid.$validate(DObject("value1" := "V", "nest2" := DObject.empty)).toSet shouldBe Set(Path("nest1") -> "Value is required." , "nest2"\"nest3" -> "Value is required.", "nest2"\"value5" -> "Value is required.")
  }

//  object ToSanitize extends Contract with Validation {
//    val sanitize = \?[String](Validators.internal)
//    val value = \?[Boolean]
//    val nested = new \\ {
//      val sanitize = \?[String](Validators.internal)
//      val value = \?[Int]
//    }
//  }
//
//
//  test("Sanitize data") {
//    val j = DObject("sanitize" := "value", "value" := true, "nested" := DObject("sanitize" := "value", "value" := 123))
//
//    ToSanitize.$sanitize(j) should
//      be (DObject("value" := true, "nested" := DObject("value" := 123)))
//  }

  object ContractArray extends Contract with Validation {
    val array = \:(ExpectedField)
  }

  test("Contract array validation") {
    ContractArray.$validate(DObject.empty) should be (f(Path("array") -> "Value is required."))
    ContractArray.$validate(DObject("array" -> DArray.empty)) shouldBe Vector.empty

    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) shouldBe Vector.empty
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))) shouldBe Vector.empty
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 4)))) should be (f("array" \ 0 \ "expGT" -> "Value 4 is not greater than 5."))
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 4)))) should be (f("array" \ 1 \ "expGT" -> "Value 4 is not greater than 5."))
  }

  object ContractArrayNonEmpty extends Contract with Validation {
    val array  = \:(ExpectedField, Validators.nonEmpty)
  }

  test("Contract array nonEmpty validation") {
    ContractArrayNonEmpty.$validate(DObject.empty) should be (f(Path("array") -> "Value is required."))
    ContractArrayNonEmpty.$validate(DObject("array" -> DArray.empty)) should be (f(Path("array") -> "Value must not be empty."))
    ContractArrayNonEmpty.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) shouldBe Vector.empty
  }

  object ContractMaybeArray extends Contract with Validation {
    val array = \:?(ExpectedField)
  }

  test("Contract maybe array validation") {
    ContractMaybeArray.$validate(DObject.empty) shouldBe Vector.empty
    ContractMaybeArray.$validate(DObject("array" -> DArray.empty)) shouldBe Vector.empty

    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) shouldBe Vector.empty
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))) shouldBe Vector.empty
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 4)))) should be (f("array" \ 0 \ "expGT" -> "Value 4 is not greater than 5."))
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 4)))) should be (f("array" \ 1 \ "expGT" -> "Value 4 is not greater than 5."))
  }


  object Element extends Contract with Validation {
    val id = \[Int]
    val name = \?[String]
  }

  object Parent extends Contract with Validation {
    val elements = \[Map[String, DObject]](Validators.mapContract(Element))
  }

  test("map contract validator") {

    val succ = DObject("elements" := Map("first" -> DObject("id" := 4), "second" -> DObject("id" := 2, "name" := "bob")))
    Parent.$validate(succ) shouldBe Vector.empty

    val failfirst = DObject("elements" := Map("first" -> DObject(), "second" -> DObject("id" := 2, "name" := "bob")))
    Parent.$validate(failfirst) should be (f(Path("elements", "first", "id") -> "Value is required."))

    val failSecond = DObject("elements" := Map("first" -> DObject("id" := 4), "second" -> DObject("id" := 2, "name" := false)))
    Parent.$validate(failSecond) should be (f(Path("elements", "second", "name") -> "Value is not of the expected type."))

    val failBoth = DObject("elements" := Map("first" -> DObject(), "second" -> DObject("id" := 2, "name" := false)))
    Parent.$validate(failBoth) should be (f(Path("elements", "first", "id") -> "Value is required.",Path("elements", "second", "name") -> "Value is not of the expected type."))
  }

  test("delta validation") {
    val current = Element.$create(e => e.id.$set(1) ~ e.name.$set("Value"))
    val delta = Element.$create(_.name.$setNull)
    Element.$validate(delta, current) shouldBe Vector.empty
  }

  object Nulls extends Contract with Validation {
    val int = \?[Int]
    val bool = \[Boolean]

    val nested = new \\? {}
  }

  val DNULL = new DNull

  test("Null validation") {
    val all = DObject("int" := 10, "bool" := true, "nested" -> DObject("values" := 1))
    Keys.$validate(DObject("int" := DNULL), all) shouldBe Vector.empty
    Keys.$validate(DObject("nested" := DNULL), all) shouldBe Vector.empty
    Keys.$validate(DObject("nested" -> DObject("values" := DNULL)), all) shouldBe Vector.empty
    Keys.$validate(DObject("bool" -> DObject("key" := DNULL)), all) shouldBe f(Path("bool") -> "Value is required.")
  }

  object Keys extends Contract with Validation {
    val nested = new \\?(Validators.keyValidator("[a-z]*".r, "Invalid key"))
  }


  test("key Validation") {
    Keys.$validate(DObject("nested" -> DObject("values" := 1))) shouldBe Vector.empty
    Keys.$validate(DObject("nested" -> DObject("123" := 3))) shouldBe f(Path("nested", "123") -> "Invalid key")
  }

  object Appending extends Contract with Validation {
    val map = \?[Map[String, String]](Validators.noKeyRemoval)
  }

  test("No key removal only") {
    val obj = DObject("map" := Map("Key" := "value") )
    Appending.$validate(DObject("map" := ("Key" := "Value2")), obj) shouldBe Vector.empty
    Appending.$validate(DObject("map" := ("Key2" := "Value2")), obj) shouldBe Vector.empty
    Appending.$validate(DObject("map" := ("Key" := DNULL)), obj) should not be Vector.empty
    Appending.$validate(DObject("map" := ("Key2" := DNULL)), obj) shouldBe Vector.empty
  }

  object Closed extends Contract with Validation with ClosedFields {
    val internalClosed = new \\? with ClosedFields {
      val one = \[String]
      val two = \?[Boolean]
    }
    val internalOpen = new \\? {
      val one = \[String]
    }
  }

  test("Closing an objects key options") {
    Closed.$validate(DObject.empty) shouldBe Vector.empty
    Closed.$validate(DObject("unexpected" := 2)) shouldBe f(Path.empty -> "Additional key 'unexpected' not allowed.")
    Closed.$validate(DObject("internalClosed" := ("one" := "value"))) shouldBe Vector.empty
    Closed.$validate(DObject("internalOpen" := ("one" := "value", "three" := 3))) shouldBe Vector.empty
    Closed.$validate(DObject("internalClosed" := ("one" := "value", "three" := 3))) shouldBe f(Path("internalClosed") -> "Additional key 'three' not allowed.")
  }

//  object Masking extends Contract with Validation {
//
//    val value = \?[Int](Validators.mask("******"))
//  }
//
//  test("Application of mask") {
//    val obj = DObject("value" := 3)
//    Masking.$sanitize(obj) shouldBe DObject("value" := "******")
//    val obj2 = DObject("value2" := 3)
//    Masking.$sanitize(obj2) shouldBe DObject("value2" := 3)
//  }

}
