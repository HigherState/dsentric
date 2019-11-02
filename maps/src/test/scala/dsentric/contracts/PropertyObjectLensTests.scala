package dsentric.contracts

import dsentric.failure.{ExpectedFailure, IncorrectTypeFailure}
import dsentric.{DObject, Dsentric, FailureMatchers, PessimisticCodecs}
import org.scalatest.{EitherValues, FunSpec, Matchers}

class PropertyObjectLensTests extends FunSpec with Matchers with FailureMatchers with EitherValues {
  import Dsentric._
  import PessimisticCodecs._
  import dsentric.Implicits._

  object ExpectedObject extends Contract {
    val empty = new \\{}
    val expected = new \\ {
      val property = \[Int]
    }
    val maybe = new \\ {
      val property = \?[Boolean]
    }
    val default = new \\ {
      val property = \![String]("default")
    }
    val nestedExpected = new \\ {
      val nested = new \\ {
        val property = \[Int]
      }
    }
    val nestedDefault = new \\ {
      val nested = new \\ {
        val property = \![String]("default")
      }
    }
  }

  describe("Expected Object lens") {
    it("Should succeed with empty object if empty object is empty") {
      val base = DObject.empty
      ExpectedObject.empty.$get(base).right.value shouldBe DObject.empty
    }
    it("Should fail if empty object is wrong type") {
      val base = DObject("empty" := "wrongType")
      ExpectedObject.empty.$get(base).left.value should contain (IncorrectTypeFailure(ExpectedObject.empty))
    }
    it("Should succeed if empty object has additional values") {
      val base = DObject("empty" ::= ("value" := 1) )
      ExpectedObject.empty.$get(base).right.value shouldBe DObject("value" := 1)
    }
    it("Should fail if nested expected property not found") {
      val base = DObject("expected" := DObject.empty)
      ExpectedObject.expected.$get(base).left.value should contain (ExpectedFailure(ExpectedObject.expected.property))
      val base2 = DObject.empty
      ExpectedObject.expected.$get(base2).left.value should contain (ExpectedFailure(ExpectedObject.expected.property))
    }
    it("Should fail if nested expected property is of wrong type") {
      val base = DObject("expected" ::= ("property" := "value") )
      ExpectedObject.expected.$get(base).left.value should contain (IncorrectTypeFailure(ExpectedObject.expected.property))
    }
    it("Should succeed if nested expected property is present") {
      val base = DObject("expected" ::= ("property" := 123) )
      ExpectedObject.expected.$get(base).right.value shouldBe DObject("property" := 123)
    }
    it("Should fail if nested maybe property is of wrong typ") {
      val base = DObject("maybe" ::= ("property" := "value") )
      ExpectedObject.maybe.$get(base).left.value should contain (IncorrectTypeFailure(ExpectedObject.maybe.property))
    }
    it("Should succeed if nested maybe property is empty") {
      val base = DObject("maybe" := DObject.empty)
      ExpectedObject.maybe.$get(base).right.value shouldBe DObject.empty
      val base2 = DObject.empty
      ExpectedObject.maybe.$get(base2).right.value shouldBe DObject.empty
    }
    it("Should succeed if nested maybe property is set") {
      val base = DObject("maybe" ::= ("property" := false))
      ExpectedObject.maybe.$get(base).right.value shouldBe DObject("property" := false)
    }
    it("Should fail if nested default property is of wrong type") {
      val base = DObject("default" ::= ("property" := 123) )
      ExpectedObject.default.$get(base).left.value should contain (IncorrectTypeFailure(ExpectedObject.default.property))
    }
    it("Should succeed if nested default value is empty with default value") {
      val base = DObject("default" := DObject.empty)
      ExpectedObject.default.$get(base).right.value shouldBe DObject("property" := "default")
      val base2 = DObject.empty
      ExpectedObject.default.$get(base2).right.value shouldBe DObject("property" := "default")
    }
    it("nested object should fail if nested expected value is missing") {
      val base = DObject.empty
      ExpectedObject.nestedExpected.$get(base).left.value should contain (ExpectedFailure(ExpectedObject.nestedExpected.nested.property))
      val base2 = DObject("nestedExpected" := DObject.empty)
      ExpectedObject.nestedExpected.$get(base2).left.value should contain (ExpectedFailure(ExpectedObject.nestedExpected.nested.property))
      val base3 = DObject("nestedExpected" ::= ("nested" := DObject.empty))
      ExpectedObject.nestedExpected.$get(base3).left.value should contain (ExpectedFailure(ExpectedObject.nestedExpected.nested.property))
    }
    it("nested object should succeed if nested expected value is set") {
      val base = DObject("nestedExpected" ::= ("nested" := DObject("property" := 456)))
      ExpectedObject.nestedExpected.$get(base).right.value shouldBe DObject("nested" := DObject("property" := 456))
    }
    it("nested object should succeed with with nested default value is missing") {
      val base = DObject.empty
      val result = DObject("nested" := DObject("property" := "default"))
      ExpectedObject.nestedDefault.$get(base).right.value shouldBe result
      val base2 = DObject("nestedDefault" := DObject.empty)
      ExpectedObject.nestedDefault.$get(base2).right.value shouldBe result
      val base3 = DObject("nestedDefault" ::= ("nested" := DObject.empty))
      ExpectedObject.nestedDefault.$get(base3).right.value shouldBe result
    }
  }

  object EmptyExpectedObject extends Contract with EmptyOnIncorrectType {
    val empty = new \\{}
    val expected = new \\ {
      val property = \[Int]
    }
    val maybe = new \\ {
      val property = \?[Boolean]
    }
    val default = new \\ {
      val property = \![String]("default")
    }
    val nestedDefault = new \\ {
      val nested = new \\ {
        val property = \![String]("default")
      }
    }
  }

  describe("Expected Object lens with EmptyOnIncorrectType") {
    it("Should return empty object if object is of wrong type") {
      val base = DObject("empty" := "wrongType")
      EmptyExpectedObject.empty.$get(base).right.value shouldBe DObject.empty
    }
    it("Should fail if nested expected property is of wrong type with expected failure") {
      val base = DObject("expected" ::= ("property" := "value") )
      EmptyExpectedObject.expected.$get(base).left.value should contain (ExpectedFailure(ExpectedObject.expected.property))
    }
    it("Should succeed if nested maybe property is of wrong type") {
      val base = DObject("maybe" ::= ("property" := "value"))
      EmptyExpectedObject.maybe.$get(base).right.value shouldBe DObject.empty
      val base2 = DObject("maybe" ::= ("property" := "value", "additional" := 123))
      EmptyExpectedObject.maybe.$get(base2).right.value shouldBe DObject("additional" := 123)
    }
    it("Should succeed if nested default property is of wrong type") {
      val base = DObject("default" ::= ("property" := 123) )
      EmptyExpectedObject.default.$get(base).right.value shouldBe DObject("property" := "default")
    }
    it("nested object should succeed with with nested default value is missing") {
      val base = DObject("nestedDefault" ::= ("nested" := DObject("property" := 1234)))
      val result = DObject("nested" := DObject("property" := "default"))
      EmptyExpectedObject.nestedDefault.$get(base).right.value shouldBe result
    }
  }

  object MaybeObject extends Contract {
    val empty = new \\?{}
    val expected = new \\? {
      val property = \[Int]
    }
    val maybe = new \\? {
      val property = \?[Boolean]
    }
    val default = new \\? {
      val property = \![String]("default")
    }
  }

  describe("Maybe Object lens") {
    it("Should succeed with none if empty object is empty") {
      val base = DObject.empty
      MaybeObject.empty.$get(base).right.value shouldBe None
    }
    it("Should fail if empty object is wrong type") {
      val base = DObject("empty" := "wrongType")
      MaybeObject.empty.$get(base).left.value should contain (IncorrectTypeFailure(ExpectedObject.empty))
    }
    it("Should succeed if empty object has additional values") {
      val base = DObject("empty" ::= ("value" := 1) )
      MaybeObject.empty.$get(base).right.value shouldBe Some(DObject("value" := 1))
    }
    it("Should return None if expected object doesnt exist") {
      val base = DObject.empty
      MaybeObject.expected.$get(base).right.value shouldBe None
    }
    it("Should fail if nested expected property not found in object") {
      val base = DObject("expected" := DObject.empty)
      MaybeObject.expected.$get(base).left.value should contain (ExpectedFailure(ExpectedObject.expected.property))
    }
    it("Should fail if nested expected property is of wrong type") {
      val base = DObject("expected" ::= ("property" := "value") )
      MaybeObject.expected.$get(base).left.value should contain (IncorrectTypeFailure(ExpectedObject.expected.property))
    }
    it("Should succeed if nested expected property is present") {
      val base = DObject("expected" ::= ("property" := 123) )
      MaybeObject.expected.$get(base).right.value shouldBe Some(DObject("property" := 123))
    }
    it("Should fail if nested maybe property is of wrong typ") {
      val base = DObject("maybe" ::= ("property" := "value") )
      MaybeObject.maybe.$get(base).left.value should contain (IncorrectTypeFailure(ExpectedObject.maybe.property))
    }
    it("Should succeed if nested maybe property is empty") {
      val base = DObject("maybe" := DObject.empty)
      MaybeObject.maybe.$get(base).right.value shouldBe Some(DObject.empty)
      val base2 = DObject.empty
      MaybeObject.maybe.$get(base2).right.value shouldBe None
    }
    it("Should succeed if nested maybe property is set") {
      val base = DObject("maybe" ::= ("property" := false))
      MaybeObject.maybe.$get(base).right.value shouldBe Some(DObject("property" := false))
    }
    it("Should fail if nested default property is of wrong type") {
      val base = DObject("default" ::= ("property" := 123) )
      MaybeObject.default.$get(base).left.value should contain (IncorrectTypeFailure(ExpectedObject.default.property))
    }
    it("Should return None of default object doesn't exist") {
      val base = DObject.empty
      MaybeObject.default.$get(base).right.value shouldBe None
    }
    it("Should succeed if nested default value is empty with default value") {
      val base = DObject("default" := DObject.empty)
      MaybeObject.default.$get(base).right.value shouldBe Some(DObject("property" := "default"))
    }
  }

  object EmptyMaybeObject extends Contract with EmptyOnIncorrectType {
    val empty = new \\?{}
    val expected = new \\? {
      val property = \[Int]
    }
    val maybe = new \\? {
      val property = \?[Boolean]
    }
    val default = new \\? {
      val property = \![String]("default")
    }
  }

  describe("Maybe Object lens with EmptyOnIncorrectType") {
    it("Should succeed with none if empty object is empty") {
      val base = DObject.empty
      EmptyMaybeObject.empty.$get(base).right.value shouldBe None
    }
    it("Should succeed with None if object is of wrong type") {
      val base = DObject("empty" := "wrongType")
      EmptyMaybeObject.empty.$get(base).right.value shouldBe None
    }
    it("Should fail with expected if expected value of wrong type") {
      val base = DObject("expected" ::= ("property" := "value") )
      EmptyMaybeObject.expected.$get(base).left.value should contain (ExpectedFailure(ExpectedObject.expected.property))
    }
    it("Should return None if nested maybe property is of wrong type") {
      val base = DObject("maybe" ::= ("property" := "value"))
      EmptyMaybeObject.maybe.$get(base).right.value shouldBe None
    }
    it("Should return with default if nested default property is of wrong type") {
      val base = DObject("default" ::= ("property" := 123) )
      EmptyMaybeObject.default.$get(base).right.value shouldBe Some(DObject("property" := "default"))
    }
  }
}
