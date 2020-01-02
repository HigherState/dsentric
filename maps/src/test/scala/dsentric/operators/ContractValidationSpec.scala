package dsentric.operators

import dsentric._
import dsentric.contracts.ClosedFields
import dsentric.failure._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ContractValidationSpec extends AnyFunSpec with Matchers {

  import Dsentric._
  import PessimisticCodecs._

  describe("Contract validation") {
    describe("Contract structure") {
      object Empty extends Contract

      it("Should validate an empty Contract") {
        Empty.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
        Empty.$ops.validate(DObject("key" := "value")) shouldBe ValidationFailures.empty
      }
      it("Should validate deltas") {
        Empty.$ops.validate(DObject("key" := "value"), DObject("key" := 123)) shouldBe ValidationFailures.empty
        Empty.$ops.validate(DObject("key" := DNull), DObject("key" := 123)) shouldBe ValidationFailures.empty
      }
    }
    describe("Closed contract structure") {
      object EmptyClosed extends Contract with ClosedFields

      it("Should validate an empty contract") {
        EmptyClosed.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail on any other field") {
        EmptyClosed.$ops.validate(DObject("field" := false)) should contain (ClosedContractFailure(EmptyClosed, Path.empty, "field"))
      }
      it("Should fail on delta value") {
        EmptyClosed.$ops.validate(DObject("field" := false), DObject.empty) should contain (ClosedContractFailure(EmptyClosed, Path.empty, "field"))
      }
      it("Should succeed on removing a field") {
        EmptyClosed.$ops.validate(DObject("field" := DNull), DObject("field" := true)) shouldBe ValidationFailures.empty
      }
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
        ExpectedField.$ops.validate(DObject("exp" := false)) should contain(IncorrectTypeFailure(ExpectedField.exp, false))
      }
      it("Should succeed if field exists and is of correct type") {
        ExpectedField.$ops.validate(DObject("exp" := "test")) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta and field is empty") {
        ExpectedField.$ops.validate(DObject.empty, DObject.empty) should contain(ExpectedFailure(ExpectedField.exp))
      }
      it("Should fail if delta is incorrect type") {
        ExpectedField.$ops.validate(DObject("exp" := false), DObject("exp" := "test")) should contain(IncorrectTypeFailure(ExpectedField.exp, false))
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
        val myb = \?[Long]
      }
      it("Should succeed if field not found") {
        MaybeField.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if field is of wrong type") {
        MaybeField.$ops.validate(DObject("myb" := false)) should contain(IncorrectTypeFailure(MaybeField.myb, false))
      }
      it("Should succeed if field exists and is of correct type") {
        MaybeField.$ops.validate(DObject("myb" := 434)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta and field is empty") {
        MaybeField.$ops.validate(DObject.empty, DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta is incorrect type") {
        MaybeField.$ops.validate(DObject("myb" := false), DObject("myb" := 1324)) should contain(IncorrectTypeFailure(MaybeField.myb, false))
      }
      it("Should succeed if delta is correct type") {
        MaybeField.$ops.validate(DObject("myb" := 4123), DObject("myb" := 432)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is correct type and state is incorrect") {
        MaybeField.$ops.validate(DObject("myb" := 1234), DObject("myb" := false)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is empty and state is incorrect") {
        MaybeField.$ops.validate(DObject.empty, DObject("myb" := false)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is null") {
        MaybeField.$ops.validate(DObject("myb" := DNull), DObject("myb" := "test")) shouldBe ValidationFailures.empty
      }
    }
    describe("Maybefield with validator") {
      object MaybeValidator extends Contract {
        val mybGT = \?[Int](Validators.>(5))
      }
      it("Should succeed if value is valid") {
        MaybeValidator.$ops.validate(DObject("mybGT" := 6)) shouldBe ValidationFailures.empty
      }
      it("Should fail if value is invalid") {
        MaybeValidator.$ops.validate(DObject("mybGT" := 3)) should contain(NumericalFailure(MaybeValidator, Path("mybGT"), 3, 5, "greater than"))
      }
      it("Should succeed if delta is valid and state is invalid") {
        MaybeValidator.$ops.validate(DObject("mybGT" := 6), DObject("mybGT" := 3)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is valid and state is valid") {
        MaybeValidator.$ops.validate(DObject("mybGT" := 6), DObject("mybGT" := 7)) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta is invalid") {
        MaybeValidator.$ops.validate(DObject("mybGT" := 3), DObject("mybGT" := 7)) should contain(NumericalFailure(MaybeValidator, Path("mybGT"), 3, 5, "greater than"))
      }
    }
  }

  describe("Default field validation") {
    describe("Default field structure") {
      object DefaultField extends Contract {
        val dfl = \![Long](4353)
      }
      it("Should succeed if field not found") {
        DefaultField.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if field is of wrong type") {
        DefaultField.$ops.validate(DObject("dfl" := false)) should contain(IncorrectTypeFailure(DefaultField.dfl, false))
      }
      it("Should succeed if field exists and is of correct type") {
        DefaultField.$ops.validate(DObject("dfl" := 5312)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta and field is empty") {
        DefaultField.$ops.validate(DObject.empty, DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta is incorrect type") {
        DefaultField.$ops.validate(DObject("dfl" := false), DObject("dfl" := 1324)) should contain(IncorrectTypeFailure(DefaultField.dfl, false))
      }
      it("Should succeed if delta is correct type") {
        DefaultField.$ops.validate(DObject("dfl" := 123), DObject("dfl" := 5122)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is correct type and state is incorrect") {
        DefaultField.$ops.validate(DObject("dfl" := 5321), DObject("dfl" := false)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is empty and state is incorrect") {
        DefaultField.$ops.validate(DObject.empty, DObject("dfl" := false)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is null") {
        DefaultField.$ops.validate(DObject("dfl" := DNull), DObject("dfl" := "test")) shouldBe ValidationFailures.empty
      }
    }
    describe("Default field with validator") {
      object DefaultValidator extends Contract {
        val dflGT = \?[Int](Validators.>(5))
      }
      it("Should succeed if value is valid") {
        DefaultValidator.$ops.validate(DObject("dflGT" := 6)) shouldBe ValidationFailures.empty
      }
      it("Should fail if value is invalid") {
        DefaultValidator.$ops.validate(DObject("dflGT" := 3)) should contain(NumericalFailure(DefaultValidator, Path("dflGT"), 3, 5, "greater than"))
      }
      it("Should succeed if delta is valid and state is invalid") {
        DefaultValidator.$ops.validate(DObject("dflGT" := 6), DObject("dflGT" := 3)) shouldBe ValidationFailures.empty
      }
      it("Should succeed if delta is valid and state is valid") {
        DefaultValidator.$ops.validate(DObject("dflGT" := 6), DObject("dflGT" := 7)) shouldBe ValidationFailures.empty
      }
      it("Should fail if delta is invalid") {
        DefaultValidator.$ops.validate(DObject("dflGT" := 3), DObject("dflGT" := 7)) should contain(NumericalFailure(DefaultValidator, Path("dflGT"), 3, 5, "greater than"))
      }
    }
  }

  describe("Expected object validation") {

    describe("Nested object structure") {
      object ExpectedEmpty extends Contract {
        val nested = new \\ {}
      }
      object ExpectedExpected extends Contract {
        val nested = new \\ {
          val exp = \[String]
        }
      }
      object ExpectedMaybe extends Contract {
        val nested = new \\ {
          val myb = \?[String]
        }
      }
      object ExpectedClosed extends Contract {
        val nested = new \\ with ClosedFields {
          val myb = \?[String]
        }
      }
      it("Should be valid if object is empty and no expected properties") {
        ExpectedEmpty.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should be valid if object is empty and only maybe properties") {
        ExpectedMaybe.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if object is empty and has expected properties") {
        ExpectedExpected.$ops.validate(DObject.empty) should contain(ExpectedFailure(ExpectedExpected.nested.exp))
      }
      it("Should fail if object is not an object") {
        ExpectedExpected.$ops.validate(DObject("nested" := 123)) should contain(IncorrectTypeFailure(ExpectedExpected.nested, 123))
      }
      it("Should fail if closed and has extra properties") {
        ExpectedClosed.$ops.validate(DObject("nested" ::= ("extra" := 123))) should contain(ClosedContractFailure(ExpectedClosed, ExpectedClosed.nested._path, "extra"))
      }
      it("Should be valid if closed and contains only included valid properties") {
        ExpectedClosed.$ops.validate(DObject("nested" ::= ("myb" := "value"))) shouldBe ValidationFailures.empty
      }
      describe("with deltas") {
        it("Should succeed if nested is null and object has no expected properties") {
          ExpectedEmpty.$ops.validate(DObject("nested" := DNull), DObject("nested" ::= ("value" := 123))) shouldBe ValidationFailures.empty
          ExpectedMaybe.$ops.validate(DObject("nested" := DNull), DObject("nested" ::= ("myb" := "value"))) shouldBe ValidationFailures.empty
        }
        it("Should fail if nested is null and object has expected properties") {
          ExpectedExpected.$ops.validate(DObject("nested" := DNull), DObject("nested" ::= ("exp" := "value"))) should contain(ExpectedFailure(ExpectedExpected.nested.exp))
        }
        it("Should fail if contents fail") {
          ExpectedExpected.$ops.validate(DObject("nested" ::= ("exp" := DNull)), DObject("nested" ::= ("exp" := "value"))) should contain(ExpectedFailure(ExpectedExpected.nested.exp))
        }
        it("Should succeed if contents succeed") {
          ExpectedExpected.$ops.validate(DObject("nested" ::= ("exp" := "value2")), DObject("nested" ::= ("exp" := "value"))) shouldBe ValidationFailures.empty
        }
      }
    }
    describe("Nested object validation") {
      object ExpectedValid extends Contract {
        val noRemoval = new \\(Validators.noKeyRemoval) {
          val myb = \?[String](Validators.nonEmptyOrWhiteSpace)
        }
        val oneOrTwo = new \\(Validators.minLength(1) && Validators.maxLength(2)) {}
      }
      it("Should succeed if object validation succeeds") {
        ExpectedValid.$ops.validate(DObject("noRemoval" ::= ("myb" := "value"), "oneOrTwo" ::= ("value" := false)))
      }
      it("Should fail if empty expected object would fail and no object value provided") {
        ExpectedValid.$ops.validate(DObject.empty) should contain(MinimumLengthFailure(ExpectedValid, ExpectedValid.oneOrTwo._path, 1, 0))
      }
      it("Should fail if object validation fails") {
        ExpectedValid.$ops.validate(DObject("oneOrTwo" ::= ("value" := false, "value2" := 123, "value3" := "v"))) should contain (MaximumLengthFailure(ExpectedValid, ExpectedValid.oneOrTwo._path, 2, 3))
      }
      it("Should fail if nested property fails") {
        ExpectedValid.$ops.validate(DObject("noRemoval" ::= ("myb" := ""), "oneOrTwo" ::= ("value" := false))) should contain (NonEmptyOrWhitespaceFailure(ExpectedValid, ExpectedValid.noRemoval.myb._path))
      }
      describe("with deltas") {
        it("Should fail on Null if empty object would fail") {
          ExpectedValid.$ops.validate(DObject("oneOrTwo" := DNull), DObject("oneOrTwo" ::= ("value" := false))) should contain (MinimumLengthFailure(ExpectedValid, ExpectedValid.oneOrTwo._path, 1, 0))
          ExpectedValid.$ops.validate(DObject("noRemoval" := DNull), DObject("noRemoval" ::= ("myb" := "value"))) should contain (KeyRemovalFailure(ExpectedValid, ExpectedValid.noRemoval._path, "myb"))
        }
        it("Should fail on delta if final state fails") {
          ExpectedValid.$ops.validate(DObject("oneOrTwo" ::= ("value3" := 123)), DObject("oneOrTwo" ::= ("value" := false, "value2" := "b"))) should contain (MaximumLengthFailure(ExpectedValid, ExpectedValid.oneOrTwo._path, 2, 3))
          ExpectedValid.$ops.validate(DObject("noRemoval" ::= ("remove" := DNull)), DObject("noRemoval" ::= ("myb" := "value", "remove" := 3))) should contain (KeyRemovalFailure(ExpectedValid, ExpectedValid.noRemoval._path, "remove"))
        }
        it("Should succeeed if delta makes initial bad state, correct") {
          ExpectedValid.$ops.validate(DObject("oneOrTwo" ::= ("value3" := DNull)), DObject("oneOrTwo" ::= ("value" := false, "value2" := 123, "value3" := "vb"))) shouldBe ValidationFailures.empty
        }
      }
    }
  }

  describe("Maybe object validation") {

    describe("Nested object structure") {
      object MaybeEmpty extends Contract {
        val nested = new \\? {}
      }
      object MaybeExpected extends Contract {
        val nested = new \\? {
          val exp = \[String]
        }
      }
      object MaybeMaybe extends Contract {
        val nested = new \\? {
          val myb = \?[String]
        }
      }
      object MaybeClosed extends Contract {
        val nested = new \\? with ClosedFields {
          val myb = \?[String]
        }
      }
      it("Should be valid if object is empty and no expected properties") {
        MaybeEmpty.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should be valid if object is empty and only maybe properties") {
        MaybeMaybe.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should be valid if object is empty and has expected properties") {
        MaybeExpected.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if object is not an object") {
        MaybeExpected.$ops.validate(DObject("nested" := 123)) should contain(IncorrectTypeFailure(MaybeExpected.nested, 123))
      }
      it("Should fail if closed and has extra properties") {
        MaybeClosed.$ops.validate(DObject("nested" ::= ("extra" := 123))) should contain(ClosedContractFailure(MaybeClosed, MaybeClosed.nested._path, "extra"))
      }
      it("Should be valid if closed and contains only included valid properties") {
        MaybeClosed.$ops.validate(DObject("nested" ::= ("myb" := "value"))) shouldBe ValidationFailures.empty
      }
      describe("with deltas") {
        it("Should succeed if nested is null and object has no expected properties") {
          MaybeEmpty.$ops.validate(DObject("nested" := DNull), DObject("nested" ::= ("value" := 123))) shouldBe ValidationFailures.empty
          MaybeMaybe.$ops.validate(DObject("nested" := DNull), DObject("nested" ::= ("myb" := "value"))) shouldBe ValidationFailures.empty
        }
        it("Should succeed if nested is null and object has expected properties") {
          MaybeExpected.$ops.validate(DObject("nested" := DNull), DObject("nested" ::= ("exp" := "value"))) shouldBe ValidationFailures.empty
        }
        it("Should fail if contents fail") {
          MaybeExpected.$ops.validate(DObject("nested" ::= ("exp" := DNull)), DObject("nested" ::= ("exp" := "value"))) should contain(ExpectedFailure(MaybeExpected.nested.exp))
        }
        it("Should succeed if contents succeed") {
          MaybeExpected.$ops.validate(DObject("nested" ::= ("exp" := "value2")), DObject("nested" ::= ("exp" := "value"))) shouldBe ValidationFailures.empty
        }
      }
    }
    describe("Nested object validation") {
      object MaybeValid extends Contract {
        val noRemoval = new \\?(Validators.noKeyRemoval) {
          val myb = \?[String](Validators.nonEmptyOrWhiteSpace)
        }
        val oneOrTwo = new \\?(Validators.minLength(1) && Validators.maxLength(2)) {}

        val reserved = new \\?(Validators.reserved)
      }
      it("Should succeed if object validation succeeds") {
        MaybeValid.$ops.validate(DObject("noRemoval" ::= ("myb" := "value"), "oneOrTwo" ::= ("value" := false)))
      }
      it("Should succeed if empty expected object would fail and no object value provided") {
        MaybeValid.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should fail if object validation fails") {
        MaybeValid.$ops.validate(DObject("oneOrTwo" ::= ("value" := false, "value2" := 123, "value3" := "v"))) should contain (MaximumLengthFailure(MaybeValid, MaybeValid.oneOrTwo._path, 2, 3))
      }
      it("Should fail if nested property fails") {
        MaybeValid.$ops.validate(DObject("noRemoval" ::= ("myb" := ""), "oneOrTwo" ::= ("value" := false))) should contain (NonEmptyOrWhitespaceFailure(MaybeValid, MaybeValid.noRemoval.myb._path))
      }
      it("Should fail if reserved element exists") {
        MaybeValid.$ops.validate(DObject("reserved" ::= ("value" := 1))) should contain (ReservedFailure(MaybeValid, MaybeValid.reserved._path))
      }
      describe("with deltas") {
        it("Should succeed on Null even if empty object would fail") {
          MaybeValid.$ops.validate(DObject("oneOrTwo" := DNull), DObject("oneOrTwo" ::= ("value" := false))) shouldBe ValidationFailures.empty
          MaybeValid.$ops.validate(DObject("noRemoval" := DNull), DObject("noRemoval" ::= ("myb" := "value"))) shouldBe ValidationFailures.empty
        }
        it("Should fail on delta if final state fails") {
          MaybeValid.$ops.validate(DObject("oneOrTwo" ::= ("value3" := 123)), DObject("oneOrTwo" ::= ("value" := false, "value2" := "b"))) should contain (MaximumLengthFailure(MaybeValid, MaybeValid.oneOrTwo._path, 2, 3))
          MaybeValid.$ops.validate(DObject("noRemoval" ::= ("remove" := DNull)), DObject("noRemoval" ::= ("myb" := "value", "remove" := 3))) should contain (KeyRemovalFailure(MaybeValid, MaybeValid.noRemoval._path, "remove"))
        }
        it("Should succeeed if delta makes initial bad state, correct") {
          MaybeValid.$ops.validate(DObject("oneOrTwo" ::= ("value3" := DNull)), DObject("oneOrTwo" ::= ("value" := false, "value2" := 123, "value3" := "vb"))) shouldBe ValidationFailures.empty
        }
      }
    }
  }

  describe("Objects validation") {
    describe("Objects structure") {
      object ObjectContract extends Contract with ClosedFields {
        val exp = \[Int]
      }
      object Objects extends Contract {
        val objects = \::(ObjectContract)
      }
      it("Should succeed if objects is empty") {
        Objects.$ops.validate(DObject.empty) shouldBe ValidationFailures.empty
      }
      it("Should succeed if object is an empty vector") {
        Objects.$ops.validate(DObject("objects" := Vector.empty[DObject])) shouldBe ValidationFailures.empty
      }
      it("Should fail if objects is not a vector of objects") {
        Objects.$ops.validate(DObject("objects" := Vector("one", "two"))) should contain (IncorrectTypeFailure(Objects, Objects.objects._path \ 0, Objects.objects._valueCodec, "one"))
      }
      it("Should succeed if objects contains valid objects") {
        Objects.$ops.validate(DObject("objects" := Vector(DObject("exp" := 1), DObject("exp" := 2)))) shouldBe ValidationFailures.empty
      }
      it("Should fail if contained objects are invalid") {
        Objects.$ops.validate(DObject("objects" := Vector(DObject("exp" := 1), DObject.empty))) should contain (ExpectedFailure(Objects, Objects.objects._path \ 1 \ "exp"))
        Objects.$ops.validate(DObject("objects" := Vector(DObject("exp" := 1, "additional" := "failed")))) should contain (ClosedContractFailure(Objects, Objects.objects._path \ 0, "additional"))
      }
      describe("with deltas") {
        it("Should succeed if objects is null") {
          Objects.$ops.validate(DObject("objects" := DNull), DObject("objects" := Vector(DObject("exp" := 1), DObject("exp" := 2)))) shouldBe ValidationFailures.empty
        }
        it("Should fail if contents fail") {
          Objects.$ops.validate(
            DObject("objects" := Vector(DObject("exp" := "value"))),
            DObject("objects" := Vector(DObject("exp" := 1), DObject("exp" := 2)))
          ) should contain (IncorrectTypeFailure(Objects, Objects.objects._path \ 0 \ "exp", ObjectContract.exp._codec, "value"))
        }
        it("Should succeed if contents succeed") {
          Objects.$ops.validate(
            DObject("objects" := Vector(DObject("exp" := 3))),
            DObject("objects" := Vector(DObject("exp" := 1), DObject("exp" := 2)))
          ) shouldBe ValidationFailures.empty
        }
      }
    }
    describe("Objects validation") {
      object ObjectContract extends Contract {
        val exp = \[Int](Validators.>(3))
        val maybe = \?[String](Validators.nonEmptyOrWhiteSpace)
      }
      object LengthObjects extends Contract {
        val objects = \::(ObjectContract, Validators.nonEmpty)
      }
      object ReservedObjects extends Contract {
        val objects = \::(ObjectContract, Validators.reserved)
      }
      it("Should return validation failure if objects validation is invalid") {
        LengthObjects.$ops.validate(DObject.empty) should contain (MinimumLengthFailure(LengthObjects, LengthObjects.objects._path, 1, 0))
        LengthObjects.$ops.validate(DObject("objects" := Vector.empty[DObject])) should contain (MinimumLengthFailure(LengthObjects, LengthObjects.objects._path, 1, 0))
      }
      it("Should return reserved failure") {
        ReservedObjects.$ops.validate(DObject("objects" := Vector(DObject("exp" := 5)))) should contain(ReservedFailure(ReservedObjects, ReservedObjects.objects._path))
      }
      it("Should return validation failures for objects") {
        val base = DObject("objects" := Vector(DObject("exp" := 3, "maybe" := "\t")))
        val failures = LengthObjects.$ops.validate(base)
        failures should contain (NumericalFailure(LengthObjects, LengthObjects.objects._path \ 0 \ "exp", 3, 3, "greater than"))
        failures should contain (NonEmptyOrWhitespaceFailure(LengthObjects, LengthObjects.objects._path \ 0 \ "maybe"))
      }

      describe("With deltas") {
        it("Should fail if contents fail") {
          LengthObjects.$ops.validate(
            DObject("objects" := Vector(DObject("exp" := 2))),
            DObject("objects" := Vector(DObject("exp" := 5), DObject("exp" := 7, "maybe" := "value")))
          ) should contain (NumericalFailure(LengthObjects, LengthObjects.objects._path \ 0 \ "exp", 2, 3, "greater than"))
        }
        it("Should succeed if content succeeds") {
          LengthObjects.$ops.validate(
            DObject("objects" := Vector(DObject("exp" := 5), DObject("exp" := 7, "maybe" := "value"))),
            DObject("objects" := Vector(DObject("exp" := 2)))
          ) shouldBe ValidationFailures.empty
        }
      }
    }
  }

  describe("Map objects validation") {
    object ObjectContract extends Contract {
      val exp = \[Int](Validators.>(3))
      val maybe = \?[String](Validators.nonEmptyOrWhiteSpace)
    }
    object LengthMapObjects extends Contract {

      val objects = \->(ObjectContract, Validators.nonEmpty)
    }
    object ReservedMapObjects extends Contract {
      val objects = \->(ObjectContract, Validators.reserved)
    }
    it("Should return validation failure if objects validation is invalid") {
      LengthMapObjects.$ops.validate(DObject.empty) should contain (MinimumLengthFailure(LengthMapObjects, LengthMapObjects.objects._path, 1, 0))
      LengthMapObjects.$ops.validate(DObject("objects" := Map.empty[String, DObject])) should contain (MinimumLengthFailure(LengthMapObjects, LengthMapObjects.objects._path, 1, 0))
    }
    it("Should return reserved failure") {
      ReservedMapObjects.$ops.validate(DObject("objects" := Map("one" -> DObject("exp" := 5)))) should contain(ReservedFailure(ReservedMapObjects, ReservedMapObjects.objects._path))
    }
    it("Should return validation failures for objects") {
      val base = DObject("objects" := Map("one" -> DObject("exp" := 3, "maybe" := "\t")))
      val failures = LengthMapObjects.$ops.validate(base)
      failures should contain (NumericalFailure(LengthMapObjects, LengthMapObjects.objects._path \ "one" \ "exp", 3, 3, "greater than"))
      failures should contain (NonEmptyOrWhitespaceFailure(LengthMapObjects, LengthMapObjects.objects._path \ "one" \ "maybe"))
    }

    describe("With deltas") {
      it("Should fail if contents fail") {
        LengthMapObjects.$ops.validate(
          DObject("objects" := Map("one" -> DObject("exp" := 2))),
          DObject("objects" := Map("one" -> DObject("exp" := 5), "two" -> DObject("exp" := 7, "maybe" := "value")))
        ) should contain (NumericalFailure(LengthMapObjects, LengthMapObjects.objects._path \ "one" \ "exp", 2, 3, "greater than"))
      }
      it("Should succeed if content succeeds") {
        LengthMapObjects.$ops.validate(
          DObject("objects" := Map("one" -> DObject("exp" := 5), "two" -> DObject("exp" := 7, "maybe" := "value"))),
          DObject("objects" := Map("one" -> DObject("exp" := 2)))
        ) shouldBe ValidationFailures.empty
      }
    }

  }

}
