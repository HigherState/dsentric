package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractValidationTests extends FunSuite with Matchers with FailureMatchers {

  implicit def strictness = MaybePessimistic
  import Dsentric._
  import PessimisticCodecs._

  object Empty extends Contract

  test("validation of contract type")  {

    Empty.$validate(DObject.empty) should be (Failures.empty)
    Empty.$validate(DObject("key" := "value")) should be (Failures.empty)
  }


  object ExpectedField extends Contract {

    val expGT = \[Int](Validators.>(5))
  }

  test("validation of expected field") {

    ExpectedField.$validate(DObject.empty) should be (Failures(Path("expGT") -> ValidationText.EXPECTED_VALUE))
    ExpectedField.$validate(DObject("expGT" := false)) should be (Failures(Path("expGT") -> ValidationText.UNEXPECTED_TYPE))
    ExpectedField.$validate(DObject("expGT" := 7)) should be (Failures.empty)
    ExpectedField.$validate(DObject("expGT" := 3)) should failWith(Path("expGT"))
  }

  object MaybeField extends Contract {
    implicit def strictness = MaybePessimistic
    val mayNonEmpty = \?[String](Validators.nonEmptyOrWhiteSpace)

  }

  test("validation of optional field") {
    MaybeField.$validate(DObject.empty) should be (Failures.empty)
    MaybeField.$validate(DObject("mayNonEmpty" := false)) should be (Failures(Path("mayNonEmpty") -> ValidationText.UNEXPECTED_TYPE))
    MaybeField.$validate(DObject("mayNonEmpty" := "TEST")) should be (Failures.empty)
    MaybeField.$validate(DObject("mayNonEmpty" := "")) should failWith(Path("mayNonEmpty"))
  }

  object DefaultField extends Contract {
    implicit def strictness = MaybePessimistic
    val inDefault = \![String]("default", Validators.in("default", "one", "two"))
  }

  test("validation of default field") {
    DefaultField.$validate(DObject.empty) should be (Failures.empty)
    DefaultField.$validate(DObject("inDefault" := false)) should be (Failures(Path("inDefault") -> ValidationText.UNEXPECTED_TYPE))
    DefaultField.$validate(DObject("inDeafult" := "two")) should be (Failures.empty)
    DefaultField.$validate(DObject("inDefault" := "three")) should failWith(Path("inDefault"))
  }

  object NestedExpectedField extends Contract {

    val nested = new \\{
      val expected = \[String]
    }
  }

  test("validation of expected nested contract") {
    NestedExpectedField.$validate(DObject.empty) should be (Failures(Path("nested") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(DObject("nested" := DObject.empty)) should be (Failures(Path("nested", "expected") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(DObject("nested" := DObject("expected" := "value"))) should be (Failures.empty)
  }

  object NestedMaybeField extends Contract {
    val nested = new \\?{
      val expected = \[String]
    }
  }

  test("validation of maybe nested contract") {
    NestedMaybeField.$validate(DObject.empty) should be (Failures.empty)
    NestedMaybeField.$validate(DObject("nested" := DObject.empty)) should be (Failures(Path("nested", "expected") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(DObject("nested" := DObject("expected" := "value"))) should be (Failures.empty)
  }

  test("Nested validation") {
    implicit def strictness = MaybePessimistic
    object NestValid extends Contract {
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
    NestValid.$validate(json1) should be (Failures.empty)
    val json2 = DObject("value1" := "V", "nest1" := DObject("value2" := "V", "value3" := "V"), "nest2" := DObject("nest3" := DObject("value4" := "V"), "value5" := "V"))
    NestValid.$validate(json2) should be (Failures.empty)

    NestValid.$validate(DObject("value1" := "V", "nest1" := DObject("value3" := 3))) should
      be (Vector("nest1"\"value2" -> "Value was expected.", "nest1"\"value3" -> "Value is not of the expected type."))

    NestValid.$validate(DObject("value1" := "V", "nest2" := DObject.empty)) should
      be (Vector(Path("nest1") -> "Value was expected." , "nest2"\"nest3" -> "Value was expected.", "nest2"\"value5" -> "Value was expected."))
  }

  object ToSanitize extends Contract {
    val sanitize = \?[String](Validators.internal)
    val value = \?[Boolean]
    val nested = new \\ {
      val sanitize = \?[String](Validators.internal)
      val value = \?[Int]
    }
  }


  test("Sanitize data") {
    val j = DObject("sanitize" := "value", "value" := true, "nested" := DObject("sanitize" := "value", "value" := 123))

    ToSanitize.$sanitize(j) should
      be (DObject("value" := true, "nested" := DObject("value" := 123)))
  }

  object ContractArray extends Contract {
    val array = \:(ExpectedField)
  }

  test("Contract array validation") {
    ContractArray.$validate(DObject.empty) should be (Failures(Path("array") -> ValidationText.EXPECTED_VALUE))
    ContractArray.$validate(DObject("array" -> DArray.empty)) should be (Failures.empty)

    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) should be (Failures.empty)
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))) should be (Failures.empty)
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 4)))) should be (Failures("array" \ 0 \ "expGT" -> "Value 4 is not greater than 5."))
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 4)))) should be (Failures("array" \ 1 \ "expGT" -> "Value 4 is not greater than 5."))
  }

  object ContractArrayNonEmpty extends Contract {
    val array  = \:(ExpectedField, Validators.nonEmpty)
  }

  test("Contract array nonEmpty validation") {
    ContractArrayNonEmpty.$validate(DObject.empty) should be (Failures(Path("array") -> ValidationText.EXPECTED_VALUE))
    ContractArrayNonEmpty.$validate(DObject("array" -> DArray.empty)) should be (Failures(Path("array") -> "Value must not be empty."))
    ContractArrayNonEmpty.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) should be (Failures.empty)
  }

  object ContractMaybeArray extends Contract {
    val array = \:?(ExpectedField)
  }

  test("Contract maybe array validation") {
    ContractMaybeArray.$validate(DObject.empty) should be (Failures.empty)
    ContractMaybeArray.$validate(DObject("array" -> DArray.empty)) should be (Failures.empty)

    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) should be (Failures.empty)
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))) should be (Failures.empty)
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 4)))) should be (Failures("array" \ 0 \ "expGT" -> "Value 4 is not greater than 5."))
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 4)))) should be (Failures("array" \ 1 \ "expGT" -> "Value 4 is not greater than 5."))
  }
}
