package dsentricTests

import dsentric._
import org.scalatest.{Matchers, FunSuite}

class ContractValidationTests extends FunSuite with Matchers with FailureMatchers {

  import J._

  object Empty extends Contract

  test("validation of contract type")  {

    Empty.$validate(JsBool(false)) should failWith(ValidationText.UNEXPECTED_TYPE)
    Empty.$validate(JsObject(Map.empty)) should be (Failures.empty)
    Empty.$validate(JsObject(Map("key" -> JsString("value")))) should be (Failures.empty)
  }


  object ExpectedField extends Contract {

    val expGT = \[Int](Validator.>(5))
  }

  test("validation of expected field") {

    ExpectedField.$validate(JsString("fail")) should be (Failures(Path.empty -> ValidationText.UNEXPECTED_TYPE))
    ExpectedField.$validate(JsObject(Map.empty)) should be (Failures(Path("expGT") -> ValidationText.EXPECTED_VALUE))
    ExpectedField.$validate(JsObject(Map("expGT" -> JsBool(false)))) should be (Failures(Path("expGT") -> ValidationText.UNEXPECTED_TYPE))
    ExpectedField.$validate(JsObject(Map("expGT" -> JsNumber(7)))) should be (Failures.empty)
    ExpectedField.$validate(JsObject(Map("expGT" -> JsNumber(3)))) should failWith(Path("expGT"))
  }

  object MaybeField extends Contract {
    implicit def strictness = MaybePessimistic
    val mayNonEmpty = \?[String](Validator.nonEmptyOrWhiteSpace)

  }

  test("validation of optional field") {
    MaybeField.$validate(JsObject(Map.empty)) should be (Failures.empty)
    MaybeField.$validate(JsObject(Map("mayNonEmpty" -> JsBool(false)))) should be (Failures(Path("mayNonEmpty") -> ValidationText.UNEXPECTED_TYPE))
    MaybeField.$validate(JsObject(Map("mayNonEmpty" -> JsString("TEST")))) should be (Failures.empty)
    MaybeField.$validate(JsObject(Map("mayNonEmpty" -> JsString("")))) should failWith(Path("mayNonEmpty"))
  }

  object DefaultField extends Contract {
    implicit def strictness = MaybePessimistic
    val inDefault = \![String]("default", Validator.in("default", "one", "two"))
  }

  test("validation of default field") {
    DefaultField.$validate(JsObject(Map.empty)) should be (Failures.empty)
    DefaultField.$validate(JsObject(Map("inDefault" -> JsBool(false)))) should be (Failures(Path("inDefault") -> ValidationText.UNEXPECTED_TYPE))
    DefaultField.$validate(JsObject(Map("inDeafult" -> JsString("two")))) should be (Failures.empty)
    DefaultField.$validate(JsObject(Map("inDefault" -> JsString("three")))) should failWith(Path("inDefault"))
  }

  object NestedExpectedField extends Contract {

    val nested = new \\{
      val expected = \[String]
    }
  }

  test("validation of expected nested contract") {
    NestedExpectedField.$validate(JsObject(Map.empty)) should be (Failures(Path("nested") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(JsObject(Map("nested" -> JsObject(Map.empty)))) should be (Failures(Path("nested", "expected") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(JsObject(Map("nested" -> JsObject(Map("expected" -> JsString("value")))))) should be (Failures.empty)
  }

  object NestedMaybeField extends Contract {
    val nested = new \\?{
      val expected = \[String]
    }
  }

  test("validation of maybe nested contract") {
    NestedMaybeField.$validate(JsObject(Map.empty)) should be (Failures.empty)
    NestedMaybeField.$validate(JsObject(Map("nested" -> JsObject(Map.empty)))) should be (Failures(Path("nested", "expected") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(JsObject(Map("nested" -> JsObject(Map("expected" -> JsString("value")))))) should be (Failures.empty)
  }

  object ToSanitize extends Contract {
    val sanitize = \?[String](Validator.internal)
    val value = \?[Boolean]
    val nested = new \\ {
      val sanitize = \?[String](Validator.internal)
      val value = \?[Int]
    }
  }


  test("Sanitize data") {
    val j = JsObject(Map("sanitize" -> JsString("value"), "value" -> JsBool(true), "nested" -> JsObject(Map("sanitize" -> JsString("value"), "value" -> JsNumber(123)))))

    ToSanitize.$sanitize(j) should
      be (JsObject(Map("value" -> JsBool(true), "nested" -> JsObject(Map("value" -> JsNumber(123))))))
  }
}
