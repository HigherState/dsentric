package dsentric.contracts

import dsentric.failure.{ClosedContractFailure, ExpectedFailure, IncorrectTypeFailure}
import dsentric.{DObject, Data, Dsentric, Path, PessimisticCodecs}
import org.scalatest.{EitherValues, FunSpec, Matchers}

class PropertyObjectLensTests extends FunSpec with Matchers with EitherValues {
  import Dsentric._
  import PessimisticCodecs._
  import dsentric.Implicits._

  describe("Expected Object lens") {
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
      val closed = new \\ with ClosedFields {
        val maybe = \?[Int]
      }
    }

    describe("$get") {
      it("Should succeed with empty object if empty object is empty") {
        val base = DObject.empty
        ExpectedObject.empty.$get(base).right.value shouldBe DObject.empty
      }
      it("Should fail if empty object is wrong type") {
        val base = DObject("empty" := "wrongType")
        ExpectedObject.empty.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.empty, "wrongType"))
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
        ExpectedObject.expected.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.expected.property, "value"))
      }
      it("Should succeed if nested expected property is present") {
        val base = DObject("expected" ::= ("property" := 123))
        ExpectedObject.expected.$get(base).right.value shouldBe DObject("property" := 123)
      }
      it("Should fail if nested maybe property is of wrong typ") {
        val base = DObject("maybe" ::= ("property" := "value"))
        ExpectedObject.maybe.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.maybe.property, "value"))
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
        ExpectedObject.default.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.default.property, 123))
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
      it("Should succeed if nested object is closed and only has closed properties") {
        val base = DObject("closed" ::= ("maybe" := 123))
        ExpectedObject.closed.$get(base).right.value shouldBe DObject("maybe" := 123)
      }
      it("Should fail if nested object is closed and contains additional properties") {
        val base = DObject("closed" ::= ("maybe" := 123, "additional" := "fail"))
        ExpectedObject.closed.$get(base).left.value should contain (ClosedContractFailure(ExpectedObject.closed, "additional"))
      }
    }
    describe("$set") {
      it("Should fail if object would fail") {
        ExpectedObject.expected.$set(DObject.empty)(DObject.empty).left.value should contain (ExpectedFailure(ExpectedObject.expected.property))
        ExpectedObject.expected.$set(DObject("property" := "fail"))(DObject.empty).left.value should contain (IncorrectTypeFailure(ExpectedObject.expected.property, "fail"))
        ExpectedObject.nestedExpected.$set(DObject.empty)(DObject.empty).left.value should contain (ExpectedFailure(ExpectedObject.nestedExpected.nested.property))
      }
      it("should succeed if object would succeed") {
        ExpectedObject.expected.$set(DObject("property" := 1))(DObject.empty).right.value shouldBe DObject("expected" := DObject("property" := 1))
        ExpectedObject.maybe.$set(DObject.empty)(DObject.empty).right.value shouldBe DObject("maybe" := DObject.empty)
        ExpectedObject.default.$set(DObject.empty)(DObject.empty).right.value shouldBe DObject("default" := DObject.empty)
      }
      it("Should succeed if object is closed and contains additional properties") {
        ExpectedObject.closed.$set(DObject("maybe" := 123))(DObject.empty).right.value shouldBe DObject("closed" ::= ("maybe" := 123))
      }
      it("Should fail if object is closed and contains additional properties") {
        ExpectedObject.closed.$set(DObject("maybe" := 123, "additional" := "fail"))(DObject.empty).left.value should contain (ClosedContractFailure(ExpectedObject.closed, "additional"))
      }
    }
  }

  describe("Expected Object lens with EmptyOnIncorrectType") {
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
      val closed = new \\ with ClosedFields {
        val property = \[Boolean]
      }
    }

    describe("$get") {
      it("Should return empty object if object is of wrong type") {
        val base = DObject("empty" := "wrongType")
        EmptyExpectedObject.empty.$get(base).right.value shouldBe DObject.empty
      }
      it("Should fail if nested expected property is of wrong type with expected failure") {
        val base = DObject("expected" ::= ("property" := "value"))
        EmptyExpectedObject.expected.$get(base).left.value should contain(ExpectedFailure(EmptyExpectedObject.expected.property))
      }
      it("Should fail if nested object is closed and contains additional properties") {
        EmptyExpectedObject.closed.$get(DObject("closed" ::= ("property" := false, "additional" := 123))).left.value should contain (ClosedContractFailure(EmptyExpectedObject.closed, "additional"))
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
      //Anything using the lens to set should be correct
      it("should fail if maybe property of wrong type") {
        EmptyExpectedObject.maybe.$set(DObject("property" := 123, "additional" := "temp"))(DObject.empty).left.value should contain (IncorrectTypeFailure(EmptyExpectedObject.maybe.property, 123))
      }
      it("should fail if default property of wrong type") {
        EmptyExpectedObject.default.$set(DObject("property" := 123))(DObject.empty).left.value should contain (IncorrectTypeFailure(EmptyExpectedObject.default.property, 123))
      }
      it("Should fail if object is closed and set object contains additional properties") {
        EmptyExpectedObject.closed.$set(DObject("property" := false, "additional" := 123))(DObject.empty).left.value should contain (ClosedContractFailure(EmptyExpectedObject.closed, "additional"))
      }
    }
  }

  describe("Maybe Object lens") {

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
      val closed = new \\? with ClosedFields {
        val property = \?[Int]
      }
    }
    describe("$get") {
      it("Should succeed with none if empty object is empty") {
        val base = DObject.empty
        MaybeObject.empty.$get(base).right.value shouldBe None
      }
      it("Should fail if empty object is wrong type") {
        val base = DObject("empty" := "wrongType")
        MaybeObject.empty.$get(base).left.value should contain(IncorrectTypeFailure(MaybeObject.empty, "wrongType"))
      }
      it("Should succeed if empty object has additional values") {
        val base = DObject("empty" ::= ("value" := 1))
        MaybeObject.empty.$get(base).right.value shouldBe Some(DObject("value" := 1))
      }
      it("Should return None if expected object doesnt exist") {
        val base = DObject.empty
        MaybeObject.expected.$get(base).right.value shouldBe None
      }
      it("Should fail if nested expected property not found in object") {
        val base = DObject("expected" := DObject.empty)
        MaybeObject.expected.$get(base).left.value should contain(ExpectedFailure(MaybeObject.expected.property))
      }
      it("Should fail if nested expected property is of wrong type") {
        val base = DObject("expected" ::= ("property" := "value"))
        MaybeObject.expected.$get(base).left.value should contain(IncorrectTypeFailure(MaybeObject.expected.property, "value"))
      }
      it("Should succeed if nested expected property is present") {
        val base = DObject("expected" ::= ("property" := 123))
        MaybeObject.expected.$get(base).right.value shouldBe Some(DObject("property" := 123))
      }
      it("Should fail if nested maybe property is of wrong typ") {
        val base = DObject("maybe" ::= ("property" := "value"))
        MaybeObject.maybe.$get(base).left.value should contain(IncorrectTypeFailure(MaybeObject.maybe.property, "value"))
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
        val base = DObject("default" ::= ("property" := 123))
        MaybeObject.default.$get(base).left.value should contain(IncorrectTypeFailure(MaybeObject.default.property, 123))
      }
      it("Should return None of default object doesn't exist") {
        val base = DObject.empty
        MaybeObject.default.$get(base).right.value shouldBe None
      }
      it("Should succeed if nested default value is empty with default value") {
        val base = DObject("default" := DObject.empty)
        MaybeObject.default.$get(base).right.value shouldBe Some(DObject("property" := "default"))
      }
      it("Should succeed if nested object is closed and only has closed properties") {
        val base = DObject("closed" ::= ("property" := 123))
        MaybeObject.closed.$get(base).right.value shouldBe Some(DObject("property" := 123))
      }
      it("Should fail if nested object is closed and contains additional properties") {
        val base = DObject("closed" ::= ("property" := 123, "additional" := "fail"))
        MaybeObject.closed.$get(base).left.value should contain (ClosedContractFailure(MaybeObject.closed, "additional"))
      }
    }
    describe("$maybeSet") {
      it("Should fail if object would fail") {
        MaybeObject.expected.$maybeSet(Some(DObject.empty))(DObject.empty).left.value should contain (ExpectedFailure(MaybeObject.expected.property))
        MaybeObject.expected.$maybeSet(Some(DObject("property" := "fail")))(DObject.empty).left.value should contain (IncorrectTypeFailure(MaybeObject.expected.property, "fail"))
      }
      it("should succeed if object would succeed") {
        MaybeObject.expected.$maybeSet(Some(DObject("property" := 1)))(DObject.empty).right.value shouldBe DObject("expected" := DObject("property" := 1))
        MaybeObject.maybe.$maybeSet(Some(DObject.empty))(DObject.empty).right.value shouldBe DObject("maybe" := DObject.empty)
        MaybeObject.default.$maybeSet(Some(DObject.empty))(DObject.empty).right.value shouldBe DObject("default" := DObject.empty)
      }
      it("Should fail if object is closed and contains additional properties") {
        MaybeObject.closed.$maybeSet(Some(DObject("property" := 123, "additional" := "fail")))(DObject.empty).left.value should contain (ClosedContractFailure(MaybeObject.closed, "additional"))
      }
    }
  }

  describe("Maybe Object lens with EmptyOnIncorrectType") {

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

  describe("Objects Lens") {

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
        Objects.expectedObjects.$get(base).left.value should contain(IncorrectTypeFailure(Objects.expectedObjects, "fail"))
      }
      it("Should succeed if all objects succeed") {
        val expecteds = Vector(DObject("property" := "test"), DObject("property" := "test2", "additional" := 123))
        val base = DObject("expectedObjects" := expecteds)
        Objects.expectedObjects.$get(base).right.value shouldBe expecteds
      }
      it("Should fail if any object fails") {
        val expecteds = Vector(DObject("property" := "test"), DObject("property" := false), DObject("property" := "test2", "additional" := 123))
        val base = DObject("expectedObjects" := expecteds)
        Objects.expectedObjects.$get(base).left.value should contain(IncorrectTypeFailure(ExpectedObjects.property, false).rebase(Objects, Path("expectedObjects", 1)))
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
        Objects.expectedObjects.$append(DObject("property" := 123))(base).left.value should contain(IncorrectTypeFailure(ExpectedObjects.property, 123))
      }
      it("Should fail if append if field being appended to is of the wrong type") {
        val base = DObject("expectedObjects" := 1234)
        Objects.expectedObjects.$append(DObject("property" := "value"))(base).left.value should contain(IncorrectTypeFailure(Objects.expectedObjects, 1234))
      }
      it("Should succeed if append if objects being appended to are wrong") {
        val base = DObject("expectedObjects" := Vector(DObject.empty))
        Objects.expectedObjects.$append(DObject("property" := "value"))(base).right.value shouldBe DObject("expectedObjects" := Vector(DObject.empty, DObject("property" := "value")))
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
        val base = DObject.empty
        Objects.expectedObjects.$set(Vector.empty)(base).right.value shouldBe DObject.empty
      }
      it("Should fail if any set objects are invalid") {
        val base = DObject.empty
        val array = Vector(ExpectedObjects.$create(_.property.$set("one")), DObject.empty, ExpectedObjects.$create(_.property.$set("two")))
        Objects.expectedObjects.$set(array)(base).left.value should contain (ExpectedFailure(ExpectedObjects.property).rebase(ExpectedObjects, Path(1)))
      }
      it("Should include any empty objects if allowed") {
        val maybeBase = DObject.empty
        val maybeArray = Vector(MaybeObjects.$create(_.property.$set("one")), DObject.empty, MaybeObjects.$create(_.property.$set("two")))
        Objects.maybeObjects.$set(maybeArray)(maybeBase).right.value shouldBe DObject("maybeObjects" := maybeArray)
      }
      it("Should replace existing collection") {
        val defaultBase = DObject.empty
        val defaultArray = Vector(MaybeObjects.$create(_.property.$set("one")), DObject.empty, MaybeObjects.$create(_.property.$set("two")))
        Objects.defaultObjects.$set(defaultArray)(defaultBase).right.value shouldBe DObject("defaultObjects" := defaultArray)
      }
      it("Should replace existing wrong collection") {
        val base = DObject("expectedObjects" := Vector(123, DObject("property" := "one"), "blah"))
        val array = Vector(ExpectedObjects.$create(_.property.$set("one")), ExpectedObjects.$create(_.property.$set("two")))
        Objects.expectedObjects.$set(array)(base).right.value shouldBe DObject("expectedObjects" := array)
      }
    }
    describe("$map") {
      it("Should succeed if objects are empty") {
        val base = DObject.empty
        Objects.expectedObjects.$map(ExpectedObjects.property.$set("value2"))(base).right.value shouldBe base
      }
      it("Should succeed if objects are valid") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "value"), DObject("property" := "value2")))
        Objects.expectedObjects.$map(ExpectedObjects.property.$modify(s => "_" + s))(base).right.value shouldBe DObject("expectedObjects" := Vector(DObject("property" := "_value"), DObject("property" := "_value2")))
      }
      it("Should fail if any object is invalid") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "value"), DObject("property" := "value2"), DObject.empty))
        Objects.expectedObjects.$map(ExpectedObjects.property.$modify(s => "_" + s))(base).left.value should contain(ExpectedFailure(ExpectedObjects.property).rebase(Objects, Objects.expectedObjects._path \ 2))
      }
      it("Should provide any default values") {
        val base = DObject("defaultObjects" := Vector(DObject("property" := "value1"), DObject("property" := "value2"), DObject.empty))
        Objects.defaultObjects.$map(DefaultObjects.property.$modify(s => "_" + s))(base).right.value shouldBe DObject("defaultObjects" := Vector(DObject("property" := "_value1"), DObject("property" := "_value2"), DObject("property" := "_default")))
      }
    }
  }

  describe("Objects Lens with EmptyOnIncorrectTypes") {

    object EmptyExpectedObjects extends Contract with EmptyOnIncorrectType {
      val property = \[String]
    }
    object EmptyMaybeObjects extends Contract with EmptyOnIncorrectType {
      val property = \?[String]
    }
    object EmptyDefaultObjects extends Contract with EmptyOnIncorrectType {
      val property = \![String]("default")
    }

    object EmptyObjects extends Contract with EmptyOnIncorrectType {
      val maybeObjects = \::[DObject](EmptyMaybeObjects)

      val defaultObjects = \::[DObject](EmptyDefaultObjects)

      val expectedObjects = \::[DObject](EmptyExpectedObjects)
    }

    describe("$get") {
      it("Should return empty if not a vector") {
        val base = DObject("maybeObjects" := 123)
        EmptyObjects.maybeObjects.$get(base).right.value shouldBe Vector.empty
      }
      it("Should ignore a failed element") {
        val base = DObject("maybeObjects" := Vector(DObject("property" := "value"), Data(123)))
        EmptyObjects.maybeObjects.$get(base).right.value shouldBe Vector(DObject("property" := "value"))
      }

      it("Should return with empty object if maybe incorrect type") {
        val base = DObject("maybeObjects" := Vector(DObject("property" := 123)))
        EmptyObjects.maybeObjects.$get(base).right.value shouldBe Vector(DObject.empty)
      }

      it("Should include any defaults if default incorrect type") {
        val base = DObject("defaultObjects" := Vector(DObject("property" := 123)))
        EmptyObjects.defaultObjects.$get(base).right.value shouldBe Vector(DObject("property" := "default"))
      }
    }
    describe("$append") {

      it("Should fail if append if appended object has any optional incorrect types") {
        val base = DObject("maybeObjects" := Vector(DObject("property" := "one")))
        EmptyObjects.maybeObjects.$append(DObject("property" := 123))(base).left.value should contain (IncorrectTypeFailure(EmptyMaybeObjects.property, 123))
      }
      it("Should fail on append if appended object has any expected incorrect types") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "one")))
        EmptyObjects.expectedObjects.$append(DObject("property" := 123))(base).left.value should contain (IncorrectTypeFailure(EmptyExpectedObjects.property, 123))
      }
      it("Should succeed if current collection has any optional incorrect types") {
        val base = DObject("maybeObjects" := Vector(DObject("property" := 123)))
        EmptyObjects.maybeObjects.$append(DObject("property" := "string"))(base).right.value shouldBe DObject("maybeObjects" := Vector(DObject("property" := 123), DObject("property" := "string")))
      }
      it("Should succeed if current is incorrect type") {
        val base = DObject("maybeObjects" := false)
        EmptyObjects.maybeObjects.$append(DObject("property" := "string"))(base).right.value shouldBe DObject("maybeObjects" := Vector(DObject("property" := "string")))
      }
    }
    describe("$set") {
      it("Should fail if any set objects have any optional incorrect types") {
        val base = DObject.empty
        val array = Vector(DObject("property" := "value"), DObject.empty, DObject("property" := 123))
        EmptyObjects.maybeObjects.$set(array)(base).left.value should contain (IncorrectTypeFailure(EmptyMaybeObjects.property, 123).rebase(EmptyMaybeObjects, Path(2)))
      }
    }
    describe("$map") {
      it("Should ignore element if that object is invalid") {
        val base = DObject("expectedObjects" := Vector(DObject("property" := "value"), DObject("property" := "value2"), DObject.empty))
        val r = EmptyObjects.expectedObjects.$map(EmptyExpectedObjects.property.$modify(s => "_" + s))(base).right.value
        r shouldBe DObject("expectedObjects" := Vector(DObject("property" := "_value"), DObject("property" := "_value2"), DObject.empty))
      }
      it("Should ignore if array not valid array") {
        val base = DObject("expectedObjects" := 123)
        val r = EmptyObjects.expectedObjects.$map(EmptyExpectedObjects.property.$modify(s => "_" + s))(base).right.value
        r shouldBe base
      }
    }
  }

  describe("MapObjectsPropertyLens") {

    object Expected extends Contract {
      val property = \[String]
    }
    object Maybe extends Contract {
      val property = \?[Int]
    }
    object Default extends Contract {
      val property = \![Boolean](false)
    }

    object MapObjects extends Contract {
      val expectedMap = \->[String, DObject](Expected)
      val maybeMap = \->[String, DObject](Maybe)
      val defaultMap = \->[String, DObject](Default)
    }

    describe("$get for map") {
      it("should return empty map if no value") {
        val base = DObject.empty
        MapObjects.expectedMap.$get(base).right.value shouldBe Map.empty

        val base2 = DObject("defaultMap" := DObject.empty)
        MapObjects.defaultMap.$get(base2).right.value shouldBe Map.empty
      }
      it("Should return map") {
        val base = DObject("maybeMap" := Map("first" -> DObject("property" := 123), "second" -> DObject("property" := 456)))
        MapObjects.maybeMap.$get(base).right.value shouldBe Map("first" -> DObject("property" := 123), "second" -> DObject("property" := 456))
      }
      it("Should fail if map not correct type") {
        val base = DObject("expectedMap" := 123)
        MapObjects.expectedMap.$get(base).left.value should contain (IncorrectTypeFailure(MapObjects.expectedMap, 123))
      }
      it("Should fail if any object fails") {
        val base = DObject("maybeMap" := Map("first" -> DObject("property" := false), "second" -> DObject("property" := 456)))
        MapObjects.maybeMap.$get(base).left.value should contain (IncorrectTypeFailure(Maybe.property, false).rebase(MapObjects, MapObjects.maybeMap._path \ "first"))
      }
      it("Should provide any default values") {
        val base = DObject("defaultMap" := Map("first" -> DObject("additional" := 23)))
        MapObjects.defaultMap.$get(base).right.value shouldBe Map("first" -> DObject("property" := false, "additional" := 23))
      }
    }
    describe("$get for individual object") {
      it("Should return None if no map object") {
        val base = DObject.empty
        MapObjects.expectedMap.$get("first")(base).right.value shouldBe None
      }
      it("Should return None if no key entry") {
        val base = DObject("maybeMap" := DObject("first" := DObject("property" := 456)))
        MapObjects.maybeMap.$get("second")(base).right.value shouldBe None
      }
      it("Should return object if found") {
        val base = DObject("expectedMap" := DObject("first" := DObject("property" := "value1"), "second" := DObject("property" := "value2")))
        MapObjects.expectedMap.$get("second")(base).right.value shouldBe Some(DObject("property" := "value2"))
      }
      it("Should return failure if object fails") {
        val base = DObject("expectedMap" := DObject("first" := DObject.empty, "second" := DObject("property" := "value2")))
        MapObjects.expectedMap.$get("first")(base).left.value should contain (ExpectedFailure(Expected.property).rebase(MapObjects, MapObjects.expectedMap._path \ "first"))
      }
      it("Should return object with defaults") {
        val base = DObject("defaultMap" := DObject("first" := DObject.empty, "second" := DObject("property" := true)))
        MapObjects.defaultMap.$get("first")(base).right.value shouldBe Some(DObject("property" := false))
      }
    }
    describe("$exists") {
      it("Should return false if empty") {
        val base = DObject.empty
        MapObjects.expectedMap.$exists("first")(base).right.value shouldBe false
      }
      it("Should return true if key present") {
        val base = DObject("maybeMap" := DObject("first" := DObject("property" := 456), "second" := DObject("property" := 456)))
        MapObjects.maybeMap.$exists("second")(base).right.value shouldBe true
      }
      it("Should return true even if object referenced fails") {
        val base = DObject("defaultMap" := DObject("first" := DObject("property" := false), "second" := DObject("property" := 456)))
        MapObjects.defaultMap.$exists("second")(base).right.value shouldBe true
      }
    }
    describe("$set") {
      //Setting empty could remove?
      it("Should clear if set empty object") {
        val base = DObject.empty
        MapObjects.expectedMap.$set(Map.empty)(base).right.value shouldBe DObject.empty
      }
      it("Should set correct map values") {
        val base = DObject.empty
        val set = Map("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value2"))
        MapObjects.expectedMap.$set(set)(base).right.value shouldBe DObject("expectedMap" := set)
      }
      it("Should fail if any objects being set fail") {
        val base = DObject.empty
        val set = Map("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 1234))
        MapObjects.expectedMap.$set(set)(base).left.value should contain (IncorrectTypeFailure(Expected.property, 1234).rebase(Expected, Path("second")))
      }
      it("Should replace map if map is incorrect type") {
        val base = DObject("maybeMap" := DObject("first" := DObject("property" := 456), "second" := DObject("property" := false)))
        val set = Map("three" -> DObject("property" := 423))
        MapObjects.maybeMap.$set(set)(base).right.value shouldBe DObject("maybeMap" := DObject("three" := DObject("property" := 423)))
      }
      it("Should replace map with failed objects") {
        val base = DObject("maybeMap" := "fail")
        val set = Map("three" -> DObject("property" := 423))
        MapObjects.maybeMap.$set(set)(base).right.value shouldBe DObject("maybeMap" := DObject("three" := DObject("property" := 423)))
      }
      it("Should include any empty objects if allowed") {
        val base = DObject.empty
        val set = Map("first" -> DObject("property" := 2), "second" -> DObject("property" := 3), "third" -> DObject.empty)
        MapObjects.maybeMap.$set(set)(base).right.value shouldBe DObject("maybeMap" := DObject("first" := DObject("property" := 2), "second" := DObject("property" := 3), "third" := DObject.empty) )
      }
    }
    describe("$clear") {
      it("Should clear if empty") {
        val base = DObject.empty
        MapObjects.defaultMap.$clear(base) shouldBe DObject.empty
      }
      it("Should clear if object map present") {
        val base = DObject("maybeMap" := DObject("first" := DObject("property" := "value1")), "defaultMap" :=Map("first" -> DObject("property" := false)) )
        MapObjects.defaultMap.$clear(base) shouldBe DObject("maybeMap" := DObject("first" := DObject("property" := "value1")))
      }
      it("Should clear if maps type is invalid") {
        val base = DObject("maybeMap" := false, "defaultMap" :=Map("first" -> DObject("property" := false)))
        MapObjects.maybeMap.$clear(base) shouldBe DObject("defaultMap" := DObject("first" := DObject("property" := false)))
      }
    }
    describe("$remove") {
      it("Should return if object empty") {
        val base = DObject.empty
        MapObjects.defaultMap.$remove("first")(base).right.value shouldBe DObject.empty
      }
      it("Should return of key not found") {
        val base = DObject("expectedMap" := DObject("first" := DObject("property" := "value1"), "second" := DObject("property" := "value2")))
        MapObjects.expectedMap.$remove("third")(base).right.value shouldBe base
      }
      it("Should remove key object if found") {
        val base = DObject("expectedMap" := DObject("first" := DObject("property" := "value1"), "second" := DObject("property" := "value2")))
        MapObjects.expectedMap.$remove("second")(base).right.value shouldBe DObject("expectedMap" := DObject("first" := DObject("property" := "value1")))
      }
      it("Should remove key object even if object fails") {
        val base = DObject("defaultMap" := DObject("first" := DObject("property" := false), "second" := DObject("property" := 456)))
        MapObjects.defaultMap.$remove("first")(base).right.value shouldBe DObject("defaultMap" := DObject("second" := DObject("property" := 456)))
      }
      it("Should fail if maps not correct type") {
        val base = DObject("expectedMap" := 123)
        MapObjects.expectedMap.$remove("first")(base).left.value should contain (IncorrectTypeFailure(MapObjects.expectedMap, 123))
      }
      it("Should clear field if last element removed") {

      }
    }
    describe("$add") {
      it("Should create single element map if no map object") {
        val base = DObject.empty
        val r = MapObjects.defaultMap.$add("first" -> Default.$create(_.property.$set(false)))(base).right.value
        r shouldBe DObject("defaultMap" ::= "first" -> DObject("property" := false))
      }
      it("Should fail if map not correct type") {
        val base = DObject("expectedMap" := 123)
        val r = MapObjects.expectedMap.$add("first" -> Expected.$create(_.property.$set("value")))(base).left.value
        r should contain (IncorrectTypeFailure(MapObjects.expectedMap, 123))
      }
      it("Should add if map exists") {
        val base = DObject("expectedMap" := Map("first" -> Expected.$create(_.property.$set("value1"))))
        val r = MapObjects.expectedMap.$add("second" -> Expected.$create(_.property.$set("value2")))(base).right.value
        r shouldBe DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value2")))
      }
      it("Should add if map objects not all valid") {
        val base = DObject("maybeMap" := Map("first" -> Expected.$create(_.property.$set("value1"))))
        val r = MapObjects.maybeMap.$add("second" -> Maybe.$create(_.property.$set(456)))(base).right.value
        r shouldBe DObject("maybeMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 456)))
      }
      it("Should fail if added object fails") {
        val base = DObject.empty
        val r = MapObjects.defaultMap.$add("first" ::= ("property" := 123))(base).left.value
        r should contain (IncorrectTypeFailure(Default.property, 123))
      }
      it("Should replace object if object already exists") {
        val base = DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value2")))
        val r = MapObjects.expectedMap.$add("second" -> Expected.$create(_.property.$set("value3")))(base).right.value
        r shouldBe DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value3")))
      }
    }
    describe("$map") {
      it("Should succeed if objects are empty") {
        val base = DObject.empty
        val r = MapObjects.expectedMap.$map(Expected.property.$modify(_ + "1"))(base).right.value
        r shouldBe DObject.empty
      }
      it("Should succeed if objects are valid") {
        val base = DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := "value2")))
        val r = MapObjects.expectedMap.$map(Expected.property.$modify(_ + "*"))(base).right.value
        r shouldBe DObject("expectedMap" ::= ("first" -> DObject("property" := "value1*"), "second" -> DObject("property" := "value2*")))
      }
      it("Should fail if any object is invalid") {
        val base = DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 123)))
        val r = MapObjects.expectedMap.$map(Expected.property.$modify(_ + "*"))(base).left.value
        r should contain (IncorrectTypeFailure(Expected.property, 123).rebase(MapObjects, MapObjects.expectedMap._path \ "second"))
      }
      it("Should provide any default values") {
        val base = DObject("defaultMap" ::= ("first" -> DObject("property" := true), "second" -> DObject.empty))
        val r = MapObjects.defaultMap.$map(a => a + ("property2" := 123))(base).right.value
        r shouldBe DObject("defaultMap" ::= ("first" -> DObject("property" := true, "property2" := 123), "second" -> DObject("property" := false, "property2" := 123)))
      }
    }
  }

  describe("MapObjectsPropertyLens with EmptyOnIncorrectType") {

    object EmptyExpected extends Contract with EmptyOnIncorrectType {
      val property = \[String]
    }
    object EmptyMaybe extends Contract with EmptyOnIncorrectType {
      val property = \?[Int]
    }
    object EmptyDefault extends Contract with EmptyOnIncorrectType {
      val property = \![Boolean](false)
    }

    object EmptyMapObjects extends Contract with EmptyOnIncorrectType {
      val expectedMap = \->[String, DObject](EmptyExpected)
      val maybeMap = \->[String, DObject](EmptyMaybe)
      val defaultMap = \->[String, DObject](EmptyDefault)
    }

    describe("$get for map") {
      it("Should return Empty if map not correct type") {
        val base = DObject("expectedMap" := 123)
        EmptyMapObjects.expectedMap.$get(base).right.value shouldBe Map.empty
      }
      it("Should drop key value if the object fails") {
        val base = DObject("maybeMap" := Map("first" -> DObject("property" := 321), "second" := 345))
        EmptyMapObjects.maybeMap.$get(base).right.value shouldBe Map("first" -> DObject("property" := 321))
      }
    }
    describe("$get for individual object") {
      it("Should empty failure if object fails") {
        val base = DObject("expectedMap" := DObject("first" := DObject.empty, "second" := DObject("property" := "value2")))
        EmptyMapObjects.expectedMap.$get("first")(base).right.value shouldBe None
        val base2 = DObject("maybeMap" := DObject("first" := DObject("property" := "value"), "second" := 123))
        EmptyMapObjects.expectedMap.$get("second")(base2).right.value shouldBe None
      }
    }
    describe("$set") {
      it("Should fail if any objects being set fail") {
        val base = DObject.empty
        val set = Map("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 1234))
        EmptyMapObjects.expectedMap.$set(set)(base).left.value should contain (IncorrectTypeFailure(EmptyExpected.property, 1234).rebase(EmptyExpected, Path("second")))
      }

    }
    describe("$remove") {
      it("Should ignore if maps not correct type") {
        val base = DObject("expectedMap" := 123)
        EmptyMapObjects.expectedMap.$remove("first")(base).right.value shouldBe base
      }
    }
    describe("$add") {
      it("Should create map of one object if map not correct type") {
        val base = DObject("expectedMap" := 123)
        val r = EmptyMapObjects.expectedMap.$add("first" -> EmptyExpected.$create(_.property.$set("value")))(base).right.value
        r shouldBe DObject("expectedMap" := Map("first" ::= ("property" := "value")))
      }
      it("Should fail if added object fails") {
        val base = DObject.empty
        val r = EmptyMapObjects.defaultMap.$add("first" ::= ("property" := 123))(base).left.value
        r should contain (IncorrectTypeFailure(EmptyDefault.property, 123))
      }
    }
    describe("$map") {
      it("Should ignore any object that are invalid") {
        val base = DObject("expectedMap" ::= ("first" -> DObject("property" := "value1"), "second" -> DObject("property" := 123)))
        val r = EmptyMapObjects.expectedMap.$map(EmptyExpected.property.$modify(_ + "*"))(base).right.value
        r shouldBe DObject("expectedMap" ::= ("first" -> DObject("property" := "value1*"), "second" -> DObject("property" := 123)))
      }
      it("Should ignore if map value is not a valid map") {
        val base = DObject("expectedMap" := 1234)
        val r = EmptyMapObjects.expectedMap.$map(EmptyExpected.property.$modify(_ + "*"))(base).right.value
        r shouldBe base
      }
    }
  }
}