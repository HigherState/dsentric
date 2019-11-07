package dsentric.contracts

import dsentric.failure.{ExpectedFailure, IncorrectTypeFailure}
import dsentric.{DObject, Dsentric, FailureMatchers, Path, PessimisticCodecs}
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
    describe("$get") {
      it("Should succeed with empty object if empty object is empty") {
        val base = DObject.empty
        ExpectedObject.empty.$get(base).right.value shouldBe DObject.empty
      }
      it("Should fail if empty object is wrong type") {
        val base = DObject("empty" := "wrongType")
        ExpectedObject.empty.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.empty))
      }
      it("Should succeed if empty object has additional values") {
        val base = DObject("empty" ::= ("value" := 1))
        ExpectedObject.empty.$get(base).right.value shouldBe DObject("value" := 1)
      }
      it("Should fail if nested expected property not found") {
        val base = DObject("expected" := DObject.empty)
        ExpectedObject.expected.$get(base).left.value should contain(ExpectedFailure(ExpectedObject.expected.property))
        val base2 = DObject.empty
        ExpectedObject.expected.$get(base2).left.value should contain(ExpectedFailure(ExpectedObject.expected.property))
      }
      it("Should fail if nested expected property is of wrong type") {
        val base = DObject("expected" ::= ("property" := "value"))
        ExpectedObject.expected.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.expected.property))
      }
      it("Should succeed if nested expected property is present") {
        val base = DObject("expected" ::= ("property" := 123))
        ExpectedObject.expected.$get(base).right.value shouldBe DObject("property" := 123)
      }
      it("Should fail if nested maybe property is of wrong typ") {
        val base = DObject("maybe" ::= ("property" := "value"))
        ExpectedObject.maybe.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.maybe.property))
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
        val base = DObject("default" ::= ("property" := 123))
        ExpectedObject.default.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.default.property))
      }
      it("Should succeed if nested default value is empty with default value") {
        val base = DObject("default" := DObject.empty)
        ExpectedObject.default.$get(base).right.value shouldBe DObject("property" := "default")
        val base2 = DObject.empty
        ExpectedObject.default.$get(base2).right.value shouldBe DObject("property" := "default")
      }
      it("nested object should fail if nested expected value is missing") {
        val base = DObject.empty
        ExpectedObject.nestedExpected.$get(base).left.value should contain(ExpectedFailure(ExpectedObject.nestedExpected.nested.property))
        val base2 = DObject("nestedExpected" := DObject.empty)
        ExpectedObject.nestedExpected.$get(base2).left.value should contain(ExpectedFailure(ExpectedObject.nestedExpected.nested.property))
        val base3 = DObject("nestedExpected" ::= ("nested" := DObject.empty))
        ExpectedObject.nestedExpected.$get(base3).left.value should contain(ExpectedFailure(ExpectedObject.nestedExpected.nested.property))
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
    describe("$set") {
      it("Should fail if object would fail") {
        ExpectedObject.expected.$set(DObject.empty)(DObject.empty).left.value should contain (ExpectedFailure(ExpectedObject.expected.property))
        ExpectedObject.expected.$set(DObject("property" := "fail"))(DObject.empty).left.value should contain (IncorrectTypeFailure(ExpectedObject.expected.property))
        ExpectedObject.nestedExpected.$set(DObject.empty)(DObject.empty).left.value should contain (ExpectedFailure(ExpectedObject.nestedExpected.nested.property))
      }
      it("should succeed if object would succeed") {
        ExpectedObject.expected.$set(DObject("property" := 1))(DObject.empty).right.value shouldBe DObject("expected" := DObject("property" := 1))
        ExpectedObject.maybe.$set(DObject.empty)(DObject.empty).right.value shouldBe DObject("maybe" := DObject.empty)
        ExpectedObject.default.$set(DObject.empty)(DObject.empty).right.value shouldBe DObject("default" := DObject.empty)
      }
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
    describe("$get") {
      it("Should return empty object if object is of wrong type") {
        val base = DObject("empty" := "wrongType")
        EmptyExpectedObject.empty.$get(base).right.value shouldBe DObject.empty
      }
      it("Should fail if nested expected property is of wrong type with expected failure") {
        val base = DObject("expected" ::= ("property" := "value"))
        EmptyExpectedObject.expected.$get(base).left.value should contain(ExpectedFailure(EmptyExpectedObject.expected.property))
      }
      it("Should succeed if nested maybe property is of wrong type") {
        val base = DObject("maybe" ::= ("property" := "value"))
        EmptyExpectedObject.maybe.$get(base).right.value shouldBe DObject.empty
        val base2 = DObject("maybe" ::= ("property" := "value", "additional" := 123))
        EmptyExpectedObject.maybe.$get(base2).right.value shouldBe DObject("additional" := 123)
      }
      it("Should succeed if nested default property is of wrong type") {
        val base = DObject("default" ::= ("property" := 123))
        EmptyExpectedObject.default.$get(base).right.value shouldBe DObject("property" := "default")
      }
      it("nested object should succeed with with nested default value is missing") {
        val base = DObject("nestedDefault" ::= ("nested" := DObject("property" := 1234)))
        val result = DObject("nested" := DObject("property" := "default"))
        EmptyExpectedObject.nestedDefault.$get(base).right.value shouldBe result
      }
    }
    describe("$set") {

      it("should succeed if maybe property of wrong type") {
        EmptyExpectedObject.maybe.$set(DObject("property" := 123, "additional" := "temp"))(DObject.empty).right.value shouldBe DObject("maybe" := DObject("additional" := "temp"))
      }
      it("should succeed if default property of wrong type") {
        EmptyExpectedObject.default.$set(DObject("property" := 123))(DObject.empty).right.value shouldBe DObject("default" := DObject.empty)
      }
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
      MaybeObject.empty.$get(base).left.value should contain (IncorrectTypeFailure(MaybeObject.empty))
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
      MaybeObject.expected.$get(base).left.value should contain (ExpectedFailure(MaybeObject.expected.property))
    }
    it("Should fail if nested expected property is of wrong type") {
      val base = DObject("expected" ::= ("property" := "value") )
      MaybeObject.expected.$get(base).left.value should contain (IncorrectTypeFailure(MaybeObject.expected.property))
    }
    it("Should succeed if nested expected property is present") {
      val base = DObject("expected" ::= ("property" := 123) )
      MaybeObject.expected.$get(base).right.value shouldBe Some(DObject("property" := 123))
    }
    it("Should fail if nested maybe property is of wrong typ") {
      val base = DObject("maybe" ::= ("property" := "value") )
      MaybeObject.maybe.$get(base).left.value should contain (IncorrectTypeFailure(MaybeObject.maybe.property))
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
      MaybeObject.default.$get(base).left.value should contain (IncorrectTypeFailure(MaybeObject.default.property))
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
      EmptyMaybeObject.expected.$get(base).left.value should contain (ExpectedFailure(EmptyMaybeObject.expected.property))
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

  describe("Objects Lens") {
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
        Objects.expectedObjects.$get(base).left.value should contain(IncorrectTypeFailure(Objects.expectedObjects))
      }
      it("Should succeed if all objects succeed") {
        val expecteds = Vector(DObject("property" := "test"), DObject("property" := "test2", "additional" := 123))
        val base = DObject("expectedObjects" := expecteds)
        Objects.expectedObjects.$get(base).right.value shouldBe expecteds
      }
      it("Should fail if any object fails") {
        val expecteds = Vector(DObject("property" := "test"), DObject("property" := false), DObject("property" := "test2", "additional" := 123))
        val base = DObject("expectedObjects" := expecteds)
        Objects.expectedObjects.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObjects.property).rebase(Objects, Path("expectedObjects", 1)))
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
        Objects.expectedObjects.$append(DObject("property" := 123))(base).left.value should contain(IncorrectTypeFailure(ExpectedObjects.property))
      }
      it("Should Fail if append if objects being appended to are wrong") {
        val base = DObject("expectedObjects" := Vector(DObject.empty))
        Objects.expectedObjects.$append(DObject("property" := "value"))(base).left.value should contain(ExpectedFailure(ExpectedObjects.property).rebase(Objects, Path("expectedObjects", 0)))
      }
      it("Should Fail with both if object wrong and objects being appended to is wrong") {
        val base = DObject("expectedObjects" := Vector(DObject.empty))
        Objects.expectedObjects.$append(DObject("property" := 123))(base).left.value should contain allOf(IncorrectTypeFailure(ExpectedObjects.property), ExpectedFailure(ExpectedObjects.property).rebase(Objects, Path("expectedObjects", 0)))
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
        pending
      }
      it("Should fail if any set objects are invalid") {
        pending
      }
      it("Should include any empty objects if allowed") {
        pending
      }
      it("Should replace existing collection") {
        pending
      }
      it("Should replace existing wrong collection") {
        pending
      }
    }
  }

  object EmptyMaybeObjects extends Contract with EmptyOnIncorrectType {
    val property = \?[String]
  }
  object EmptyDefaultObjects extends Contract with EmptyOnIncorrectType {
    val property = \![String]("default")
  }

  object EmptyObjects extends Contract with EmptyOnIncorrectType {
    val maybeObjects = \::[DObject](MaybeObjects)

    val defaultObjects = \::[DObject](DefaultObjects)
  }

  describe("Objects Lens with EmptyOnIncorrectTypes") {
    describe("$get") {
      it("Should return empty if not a vector") {
        pending
      }

      it("Should return with empty object if maybe incorrect type") {
        pending
      }

      it("Should fail if expected incorrect type") {
        pending
      }

      it("Should include any defaults if default incorrect type") {
        pending
      }
    }
    describe("$append") {

      it("Should succeed if append if appended object has any optional incorrect types") {
        pending
      }
      it("Should fail on append if appended object has any expected incorrect types") {
        pending
      }
      it("Should succeed if current collection has any optional incorrect types") {
        pending //will it remove those incorrect types... hmmm.
      }
    }
    describe("$set") {
      it("Should set empty") {
        pending
      }
      it("Should succeed if any set objects hav any optional incorrect types") {
        pending
      }
      it("Should fail if any set objects have any expected incorrect types") {
        pending
      }
    }
  }
}
