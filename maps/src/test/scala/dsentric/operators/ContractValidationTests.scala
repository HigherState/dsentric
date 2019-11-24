package dsentric.operators

import dsentric._
import dsentric.failure.{ExpectedFailure, IncorrectTypeFailure, NumericalFailure, ValidationFailures}
import org.scalatest.{FunSpec, FunSuite, Matchers}

class ContractValidationTests extends FunSpec with Matchers with FailureMatchers {

  import Dsentric._
  import PessimisticCodecs._

  describe("Contract validation") {
    object Empty extends Contract

    it("Should validate an empty Contract") {
      Empty.$ops.validate(DObject.empty) shouldBe Vector.empty
      Empty.$ops.validate(DObject("key" := "value")) shouldBe Vector.empty
    }
    it("Should validate deltas") {
      Empty.$ops.validate(DObject("key" := "value"), DObject("key" := 123)) shouldBe Vector.empty
      Empty.$ops.validate(DObject("key" := DNull), DObject("key" := 123)) shouldBe Vector.empty
    }
  }

  describe("Expected field validation") {
    describe("Expected field structure") {
      object ExpectedField extends Contract {
        val exp = \[String]
      }
      it("Should fail if field not found") {
        ExpectedField.$ops.validate(DObject.empty) should contain(ExpectedFailure(ExpectedField.exp))
      }
      it("Should fail if field is of wrong type") {
        ExpectedField.$ops.validate(DObject("exp" := false)) should contain(IncorrectTypeFailure(ExpectedField.exp, false.getClass))
      }
      it("Should succeed if field exists and is of correct type") {
        ExpectedField.$ops.validate(DObject("exp" := "test")) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta and field is empty") {
        ExpectedField.$ops.validate(DObject.empty, DObject.empty) should contain(ExpectedFailure(ExpectedField.exp))
      }
      it("Should fail if delta is incorrect type") {
        ExpectedField.$ops.validate(DObject("exp" := false), DObject("exp" := "test")) should contain(IncorrectTypeFailure(ExpectedField.exp, false.getClass))
      }
      it("Should succeed if delta is correct type") {
        ExpectedField.$ops.validate(DObject("exp" := "test"), DObject("exp" := "test2")) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is correct type and state is incorrect") {
        ExpectedField.$ops.validate(DObject("exp" := "test"), DObject("exp" := false)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is empty and state is incorrect") {
        ExpectedField.$ops.validate(DObject.empty, DObject("exp" := false)) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta is null") {
        ExpectedField.$ops.validate(DObject("exp" := DNull), DObject("exp" := "test")) should contain(ExpectedFailure(ExpectedField.exp))
      }
    }
    describe("Expected field with validator") {
      object ExpectedValidator extends Contract {
        val expGT = \[Int](Validators.>(5))
      }
      it("Should succeed if value is valid") {
        ExpectedValidator.$ops.validate(DObject("expGT" := 6)) shouldBe ValidationFailures.empty
      }
      it("Should fail if value is invalid") {
        ExpectedValidator.$ops.validate(DObject("expGT" := 3)) should contain(NumericalFailure(ExpectedValidator, Path("expGT"), 3, 5, "greater than"))
      }
      it("Should succeed if delta is valid and state is invalid") {
        ExpectedValidator.$ops.validate(DObject("expGT" := 6), DObject("expGT" := 3)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is valid and state is valid") {
        ExpectedValidator.$ops.validate(DObject("expGT" := 6), DObject("expGT" := 7)) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta is invalid") {
        ExpectedValidator.$ops.validate(DObject("expGT" := 3), DObject("expGT" := 7)) should contain(NumericalFailure(ExpectedValidator, Path("expGT"), 3, 5, "greater than"))
      }
    }
  }

  describe("Maybe field validation") {
    describe("Maybe field structure") {
      object MaybeField extends Contract {
        val exp = \?[Long]
      }
      it("Should succeed if field not found") {
        MaybeField.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if field is of wrong type") {
        MaybeField.$ops.validate(DObject("exp" := false)) should contain(IncorrectTypeFailure(MaybeField.exp, false.getClass))
      }
      it("Should succeed if field exists and is of correct type") {
        MaybeField.$ops.validate(DObject("exp" := "test")) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta and field is empty") {
        MaybeField.$ops.validate(DObject.empty, DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta is incorrect type") {
        MaybeField.$ops.validate(DObject("exp" := false), DObject("exp" := 1324)) should contain(IncorrectTypeFailure(MaybeField.exp, false.getClass))
      }
      it("Should succeed if delta is correct type") {
        MaybeField.$ops.validate(DObject("exp" := "test"), DObject("exp" := "test2")) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is correct type and state is incorrect") {
        MaybeField.$ops.validate(DObject("exp" := "test"), DObject("exp" := false)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is empty and state is incorrect") {
        MaybeField.$ops.validate(DObject.empty, DObject("exp" := false)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is null") {
        MaybeField.$ops.validate(DObject("exp" := DNull), DObject("exp" := "test")) shouldBe ValidationFailures.empty
      }
    }
  describe("Maybefield with validator") {
    object MaybeValidator extends Contract {
      val expGT = \?[Int](Validators.>(5))
    }
    it("Should succeed if value is valid") {
      MaybeValidator.$ops.validate(DObject("expGT" := 6)) shouldBe ValidationFailures.empty
    }
    it("Should fail if value is invalid") {
      MaybeValidator.$ops.validate(DObject("expGT" := 3)) should contain(NumericalFailure(MaybeValidator, Path("expGT"), 3, 5, "greater than"))
    }
    it("Should succeed if delta is valid and state is invalid") {
      MaybeValidator.$ops.validate(DObject("expGT" := 6), DObject("expGT" := 3)) shouldBe ValidationFailures.empty
    }
    it("Should succeed if delta is valid and state is valid") {
      MaybeValidator.$ops.validate(DObject("expGT" := 6), DObject("expGT" := 7)) shouldBe ValidationFailures.empty
    }
    it("Should fail if delta is invalid") {
      MaybeValidator.$ops.validate(DObject("expGT" := 3), DObject("expGT" := 7)) should contain(NumericalFailure(MaybeValidator, Path("expGT"), 3, 5, "greater than"))
    }
  }
  }

  describe("Default field validation") {

  }

  describe("Expected nested object validation") {

  }

  describe("Maybe nested object validation") {

  }

  describe("Objects validation") {

  }

  describe("Object map validation") {

  }
  
//
//  test("validation of optional field") {
//    MaybeField.$ops.validate(DObject.empty) shouldBe Vector.empty
//    MaybeField.$ops.validate(DObject("mayNonEmpty" := false)) should be (f(Path("mayNonEmpty") -> "Value is not of the expected type."))
//    MaybeField.$ops.validate(DObject("mayNonEmpty" := "TEST")) shouldBe Vector.empty
//    MaybeField.$ops.validate(DObject("mayNonEmpty" := "")) should be (f(Path("mayNonEmpty") -> "String must not be empty or whitespace."))
//  }
//
//  object DefaultField extends Contract {
//    val inDefault = \![String]("default", Validators.in("default", "one", "two"))
//  }
//
//  test("validation of default field") {
//    DefaultField.$ops.validate(DObject.empty) shouldBe Vector.empty
//    DefaultField.$ops.validate(DObject("inDefault" := false)) should be (f(Path("inDefault") -> "Value is not of the expected type."))
//    DefaultField.$ops.validate(DObject("inDeafult" := "two")) shouldBe Vector.empty
//    DefaultField.$ops.validate(DObject("inDefault" := "three")) should be (f(Path("inDefault") -> "'three' is not an allowed value."))
//  }
//
//  object NestedExpectedField extends Contract {
//
//    val nested = new \\{
//      val expected = \[String]
//    }
//  }
//
//  test("validation of expected nested contract") {
//    NestedExpectedField.$ops.validate(DObject.empty) should be (f(Path("nested") -> "Value is required."))
//    NestedExpectedField.$ops.validate(DObject("nested" := DObject.empty)) should be (f(Path("nested", "expected") -> "Value is required."))
//    NestedExpectedField.$ops.validate(DObject("nested" := DObject("expected" := "value"))) shouldBe Vector.empty
//  }
//
//  object NestedMaybeField extends Contract {
//    val nested = new \\?{
//      val expected = \[String]
//    }
//  }
//
//  test("validation of maybe nested contract") {
//    NestedMaybeField.$ops.validate(DObject.empty) shouldBe Vector.empty
//    NestedMaybeField.$ops.validate(DObject("nested" := DObject.empty)) should be (f(Path("nested", "expected") -> "Value is required."))
//    NestedExpectedField.$ops.validate(DObject("nested" := DObject("expected" := "value"))) shouldBe Vector.empty
//  }
//
//  test("Nested validation") {
//    object NestValid extends Contract {
//      val value1 = \[String]
//      val nest1 = new \\ {
//        val value2 = \[String]
//        val value3 = \[String]
//      }
//      val nest2 = new \\? {
//        val nest3 = new \\ {
//          val value4 = \[String]
//        }
//        val value5 = \[String]
//      }
//    }
//
//    val json1 = DObject("value1" := "V", "nest1" := DObject("value2" := "V", "value3" := "V"))
//    NestValid.$ops.validate(json1) shouldBe Vector.empty
//    val json2 = DObject("value1" := "V", "nest1" := DObject("value2" := "V", "value3" := "V"), "nest2" := DObject("nest3" := DObject("value4" := "V"), "value5" := "V"))
//    NestValid.$ops.validate(json2) shouldBe Vector.empty
//
//    NestValid.$ops.validate(DObject("value1" := "V", "nest1" := DObject("value3" := 3))).toSet shouldBe Set("nest1"\"value2" -> "Value is required.", "nest1"\"value3" -> "Value is not of the expected type.")
//
//    NestValid.$ops.validate(DObject("value1" := "V", "nest2" := DObject.empty)).toSet shouldBe Set(Path("nest1") -> "Value is required." , "nest2"\"nest3" -> "Value is required.", "nest2"\"value5" -> "Value is required.")
//  }
//
//  object ContractArray extends Contract {
//    val array = \::(ExpectedField)
//  }
//
//  test("Contract array validation") {
//    ContractArray.$ops.validate(DObject.empty) shouldBe Vector.empty
//    ContractArray.$ops.validate(DObject("array" -> DArray.empty)) shouldBe Vector.empty
//
//    ContractArray.$ops.validate(DObject("array" -> DArray(DObject("expGT" := 6)))) shouldBe Vector.empty
//    ContractArray.$ops.validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))) shouldBe Vector.empty
//    ContractArray.$ops.validate(DObject("array" -> DArray(DObject("expGT" := 4)))) should be (f("array" \ 0 \ "expGT" -> "Value 4 is not greater than 5."))
//    ContractArray.$ops.validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 4)))) should be (f("array" \ 1 \ "expGT" -> "Value 4 is not greater than 5."))
//  }
//
//  object ContractArrayNonEmpty extends Contract {
//    val array  = \::(ExpectedField, Validators.nonEmpty)
//  }
//
//  test("Contract array nonEmpty validation") {
//    ContractArrayNonEmpty.$ops.validate(DObject.empty) shouldBe f(Path("array") -> "Value must not be empty.")
//    ContractArrayNonEmpty.$ops.validate(DObject("array" -> DArray.empty)) shouldBe f(Path("array") -> "Value must not be empty.")
//    ContractArrayNonEmpty.$ops.validate(DObject("array" -> DArray(DObject("expGT" := 6)))) shouldBe Vector.empty
//  }
//
//
//  object Element extends Contract {
//    val id = \[Int]
//    val name = \?[String]
//  }
//
//  object Parent extends Contract {
//    val elements = \->[String, DObject](Element)
//  }
//
//  test("map contract validator") {
//
//    Parent.$ops.validate(DObject.empty) shouldBe Vector.empty
//
//    val succ = DObject("elements" := Map("first" -> DObject("id" := 4), "second" -> DObject("id" := 2, "name" := "bob")))
//    Parent.$ops.validate(succ) shouldBe Vector.empty
//
//    val failfirst = DObject("elements" := Map("first" -> DObject(), "second" -> DObject("id" := 2, "name" := "bob")))
//    Parent.$ops.validate(failfirst) should be (f(Path("elements", "first", "id") -> "Value is required."))
//
//    val failSecond = DObject("elements" := Map("first" -> DObject("id" := 4), "second" -> DObject("id" := 2, "name" := false)))
//    Parent.$ops.validate(failSecond) should be (f(Path("elements", "second", "name") -> "Value is not of the expected type."))
//
//    val failBoth = DObject("elements" := Map("first" -> DObject(), "second" -> DObject("id" := 2, "name" := false)))
//    Parent.$ops.validate(failBoth) should be (f(Path("elements", "first", "id") -> "Value is required.",Path("elements", "second", "name") -> "Value is not of the expected type."))
//  }
//
//  test("delta validation") {
//    val current = Element.$create(e => e.id.$set(1) ~ e.name.$set("Value"))
//    //val delta = Element.$create(_.name.$setNull)
//    //Element.$ops.validate(delta, current) shouldBe Vector.empty
//  }
//
//  object Nulls extends Contract {
//    val int = \?[Int]
//    val bool = \[Boolean]
//
//    val nested = new \\? {}
//  }
//
//
//  test("Null validation") {
//    val all = DObject("int" := 10, "bool" := true, "nested" -> DObject("values" := 1))
//    Nulls.$ops.validate(DObject("int" := DNull), all) shouldBe Vector.empty
//    Nulls.$ops.validate(DObject("nested" := DNull), all) shouldBe Vector.empty
//    Nulls.$ops.validate(DObject("nested" := ("values" := DNull)), all) shouldBe Vector.empty
//    Nulls.$ops.validate(DObject("bool" := DNull), all) shouldBe f(Path("bool") -> "Value is required.")
//  }
//
//  object Keys extends Contract {
//    val nested = new \\?(Validators.keyValidator("[a-z]*".r, "Invalid key"))
//  }
//
//
//  test("key Validation") {
//    Keys.$ops.validate(DObject("nested" -> DObject("values" := 1))) shouldBe Vector.empty
//    Keys.$ops.validate(DObject("nested" -> DObject("123" := 3))) shouldBe f(Path("nested", "123") -> "Invalid key")
//  }
//
//  object Appending extends Contract {
//    val map = \?[Map[String, String]](Validators.noKeyRemoval)
//  }
//
//  test("No key removal only") {
//    val obj = DObject("map" := Map("Key" := "value") )
//    Appending.$ops.validate(DObject("map" := ("Key" := "Value2")), obj) shouldBe Vector.empty
//    Appending.$ops.validate(DObject("map" := ("Key2" := "Value2")), obj) shouldBe Vector.empty
//    Appending.$ops.validate(DObject("map" := ("Key" := DNull)), obj) should not be Vector.empty
//    Appending.$ops.validate(DObject("map" := ("Key2" := DNull)), obj) shouldBe Vector.empty
//  }
//
//  object Closed extends Contract with ClosedFields {
//    val internalClosed = new \\? with ClosedFields {
//      val one = \[String]
//      val two = \?[Boolean]
//    }
//    val internalOpen = new \\? {
//      val one = \[String]
//    }
//  }
//
//  test("Closing an objects key options") {
//    Closed.$ops.validate(DObject.empty) shouldBe Vector.empty
//    Closed.$ops.validate(DObject("unexpected" := 2)) shouldBe f(Path.empty -> "Additional key 'unexpected' not allowed.")
//    Closed.$ops.validate(DObject("internalClosed" ::= ("one" := "value"))) shouldBe Vector.empty
//    Closed.$ops.validate(DObject("internalOpen" ::= ("one" := "value", "three" := 3))) shouldBe Vector.empty
//    Closed.$ops.validate(DObject("internalClosed" ::= ("one" := "value", "three" := 3))) shouldBe f(Path("internalClosed") -> "Additional key 'three' not allowed.")
//  }
//
//  object ReservedAndInternal extends Contract {
//    val internalP = \?[String](Validators.internal)
//
//    val reservedP = \?[Boolean](Validators.reserved)
//  }
//
//  test("Reserved and internal") {
//    ReservedAndInternal.$ops.validate(DObject.empty) shouldBe Vector.empty
//    ReservedAndInternal.$ops.validate(DObject("internalP" := "internal")) shouldBe f(Path("internalP") -> "Value is reserved and cannot be provided.")
//    ReservedAndInternal.$ops.validate(DObject("reservedP" := false)) shouldBe f(Path("reservedP") -> "Value is reserved and cannot be provided.")
//  }

}
