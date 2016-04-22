package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractValidationTests extends FunSuite with Matchers with FailureMatchers {

  implicit def strictness = MaybePessimistic
  import Dsentric._
  import PessimisticCodecs._

  object Empty extends Contract

  test("validation of contract type")  {

    Empty.$validate(JObject.empty) should be (Failures.empty)
    Empty.$validate(JObject("key" := "value")) should be (Failures.empty)
  }


  object ExpectedField extends Contract {

    val expGT = \[Int](Validator.>(5))
  }

  test("validation of expected field") {

    ExpectedField.$validate(JObject.empty) should be (Failures(Path("expGT") -> ValidationText.EXPECTED_VALUE))
    ExpectedField.$validate(JObject("expGT" := false)) should be (Failures(Path("expGT") -> ValidationText.UNEXPECTED_TYPE))
    ExpectedField.$validate(JObject("expGT" := 7)) should be (Failures.empty)
    ExpectedField.$validate(JObject("expGT" := 3)) should failWith(Path("expGT"))
  }

  object MaybeField extends Contract {
    implicit def strictness = MaybePessimistic
    val mayNonEmpty = \?[String](Validator.nonEmptyOrWhiteSpace)

  }

  test("validation of optional field") {
    MaybeField.$validate(JObject.empty) should be (Failures.empty)
    MaybeField.$validate(JObject("mayNonEmpty" := false)) should be (Failures(Path("mayNonEmpty") -> ValidationText.UNEXPECTED_TYPE))
    MaybeField.$validate(JObject("mayNonEmpty" := "TEST")) should be (Failures.empty)
    MaybeField.$validate(JObject("mayNonEmpty" := "")) should failWith(Path("mayNonEmpty"))
  }

  object DefaultField extends Contract {
    implicit def strictness = MaybePessimistic
    val inDefault = \![String]("default", Validator.in("default", "one", "two"))
  }

  test("validation of default field") {
    DefaultField.$validate(JObject.empty) should be (Failures.empty)
    DefaultField.$validate(JObject("inDefault" := false)) should be (Failures(Path("inDefault") -> ValidationText.UNEXPECTED_TYPE))
    DefaultField.$validate(JObject("inDeafult" := "two")) should be (Failures.empty)
    DefaultField.$validate(JObject("inDefault" := "three")) should failWith(Path("inDefault"))
  }

  object NestedExpectedField extends Contract {

    val nested = new \\{
      val expected = \[String]
    }
  }

  test("validation of expected nested contract") {
    NestedExpectedField.$validate(JObject.empty) should be (Failures(Path("nested") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(JObject("nested" := JObject.empty)) should be (Failures(Path("nested", "expected") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(JObject("nested" := JObject("expected" := "value"))) should be (Failures.empty)
  }

  object NestedMaybeField extends Contract {
    val nested = new \\?{
      val expected = \[String]
    }
  }

  test("validation of maybe nested contract") {
    NestedMaybeField.$validate(JObject.empty) should be (Failures.empty)
    NestedMaybeField.$validate(JObject("nested" := JObject.empty)) should be (Failures(Path("nested", "expected") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(JObject("nested" := JObject("expected" := "value"))) should be (Failures.empty)
  }

//  object ToSanitize extends Contract {
//    val sanitize = \?[String](Validator.internal)
//    val value = \?[Boolean]
//    val nested = new \\ {
//      val sanitize = \?[String](Validator.internal)
//      val value = \?[Int]
//    }
//  }
//
//
//  test("Sanitize data") {
//    val j = JObject("sanitize" := "value", "value" := true, "nested" := JObject("sanitize" := "value", "value" := 123))
//
//    ToSanitize.$sanitize(j) should
//      be (JObject("value" := true, "nested" := JObject("value" := 123)))
//  }
}
