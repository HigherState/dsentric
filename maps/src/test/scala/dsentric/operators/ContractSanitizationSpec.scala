package dsentric.operators

import dsentric._
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ContractSanitizationSpec extends AnyFunSpec with Matchers with EitherValues {

  import Dsentric._
  import PessimisticCodecs._
  import Sanitizers._

  describe("Contract sanitization") {
    describe("When empty") {
      object Empty extends Contract

      it("Should leave empty empty") {
        Empty.$ops.sanitize(DObject.empty) shouldBe DObject.empty
      }
      it("Should leave unsanitized property values alone") {
        Empty.$ops.sanitize(DObject("unsanitized" := false)) shouldBe DObject("unsanitized" := false)
      }
    }

    describe("Maybe Properties") {
      object MaybeSanitize extends Contract {
        val internalProp = \?[String](internal)
        val maskMaybe = \?[DObject](mask("()"))
        val maskEmptyMaybe = \?[String](maskEmpty("Hidden"))
        val maskFunctionMaybe = \?[String](maskFunction((s: String) => Some(s.length), Some(0)))
      }

      it("Should set santize empty results if empty") {
        MaybeSanitize.$ops.sanitize(DObject.empty) shouldBe DObject("maskEmptyMaybe" := "Hidden", "maskFunctionMaybe" := 0)
      }
      it("Should remove internal property") {
        MaybeSanitize.$ops.sanitize(MaybeSanitize.$create(_.internalProp.$set("value"))) shouldBe DObject("maskEmptyMaybe" := "Hidden", "maskFunctionMaybe" := 0)
      }
      it("Should replace set mask values") {
        MaybeSanitize.$ops.sanitize(MaybeSanitize.$create(c => c.maskMaybe.$set(DObject("value" := 1)) ~ c.maskEmptyMaybe.$set("value"))) shouldBe
          DObject("maskMaybe" := "()", "maskEmptyMaybe" := "Hidden", "maskFunctionMaybe" := 0)
      }
      it("Should apply function mask") {
        MaybeSanitize.$ops.sanitize(MaybeSanitize.$create(c => c.maskFunctionMaybe.$set("String value"))) shouldBe DObject("maskEmptyMaybe" := "Hidden", "maskFunctionMaybe" := 12)
      }
    }

    describe("Default Properties") {
      object DefaultSanitize extends Contract {
        val maskDefault = \![Int](0, mask("***"))
        val maskFunctionDefault = \![String]("value", maskFunction((s: String) => Some(s.length), Some(0)))
      }

      it("Empty default values should not set mask") {
        DefaultSanitize.$ops.sanitize(DObject.empty) shouldBe DObject("maskFunctionDefault" := 0)
      }
      it("Default values should mask") {
        DefaultSanitize.$ops.sanitize(DefaultSanitize.$create(_.maskDefault.$set(2))) shouldBe DObject("maskDefault" := "***", "maskFunctionDefault" := 0)
      }
    }

    describe("Expected object validation") {
      object ExpectedObjectSanitize extends Contract {
        val nested = new \\ {
          val internalMaybe = \?[Boolean](internal)
          val maskExpected = \[String](mask("***"))
          val maskEmptyMaybe = \?[Boolean](maskEmpty("***"))
        }
      }

      it("Should set empty values if object is empty") {
        ExpectedObjectSanitize.$ops.sanitize(DObject.empty) shouldBe DObject("nested" ::= ("maskEmptyMaybe" := "***"))
      }
      it("Should remove internal and set when required") {
        val obj = ExpectedObjectSanitize.$create { c =>
          c.nested.internalMaybe.$set(false) ~
            c.nested.maskExpected.$set("value") ~
            c.nested.maskEmptyMaybe.$set(true)
        }
        ExpectedObjectSanitize.$ops.sanitize(obj) shouldBe DObject("nested" ::= ("maskExpected" := "***", "maskEmptyMaybe" := "***"))
      }
    }

    describe("Maybe object validation") {
      object MaybeObjectSanitize extends Contract {
        val nested = new \\? {
          val internalMaybe = \?[Boolean](internal)
          val maskExpected = \[String](mask("***"))
          val maskEmptyMaybe = \?[Boolean](maskEmpty("***"))
        }
      }

      it("Should not sanitize an object if not found") {
        MaybeObjectSanitize.$ops.sanitize(DObject.empty) shouldBe DObject.empty
      }
      it("Should remove internal and set when required") {
        val obj = MaybeObjectSanitize.$create { c =>
          c.nested.internalMaybe.$set(false) ~
            c.nested.maskExpected.$set("value") ~
            c.nested.maskEmptyMaybe.$set(true)
        }
        MaybeObjectSanitize.$ops.sanitize(obj) shouldBe DObject("nested" ::= ("maskExpected" := "***", "maskEmptyMaybe" := "***"))
      }
    }

    describe("Objects sanitization") {
      describe("Empty object") {
        object EmptyContract extends Contract

        object EmptyObjectsSanitize extends Contract {
          val objects = \::(EmptyContract)
          val objectsMask = \::(EmptyContract, mask("***"))
        }

        it("Should be empty if empty") {
          EmptyObjectsSanitize.$ops.sanitize(DObject.empty) shouldBe DObject.empty
        }
        it("Should return objects unchanged") {
          val ob1 = DObject("value" := 3)
          val ob2 = DObject("value" := 5)
          val ob3 = DObject("another" := false)
          val obj = EmptyObjectsSanitize.$createValid(_.objects.$set(Vector(ob1, ob2, ob3))).right.value

          EmptyObjectsSanitize.$ops.sanitize(obj) shouldBe obj
        }
        it("Should sanitize objects mask") {
          val ob1 = DObject("value" := 3)
          val ob2 = DObject("value" := 5)
          val ob3 = DObject("another" := false)
          val obj = EmptyObjectsSanitize.$createValid(_.objectsMask.$set(Vector(ob1, ob2, ob3))).right.value
          EmptyObjectsSanitize.$ops.sanitize(obj) shouldBe DObject("objectsMask" := "***")
        }
      }
      describe("With sanitizing properties") {
        object MaybeMaskContract extends Contract {
          val maybeMask = \?[String](mask("***"))
          val maybeInternal = \?[Long](internal)
        }

        object ObjectsSanitize extends Contract {
          val objects = \::(MaybeMaskContract)
        }

        it("Should sanitize objects that require sanitizing") {
          val ob1 = DObject("value" := 123)
          val ob2 = MaybeMaskContract.$create(_.maybeInternal.$set(1234L))
          val ob3 = MaybeMaskContract.$create(_.maybeMask.$set("value"))
          val ob4 = MaybeMaskContract.$create(c => c.maybeInternal.$set(1234L) ~ c.maybeMask.$set("value") ~+ ("value" := 123))
          val obj = ObjectsSanitize.$createValid(_.objects.$set(Vector(ob1, ob2, ob3, ob4))).right.get
          val result = ObjectsSanitize.$createValid(_.objects.$set(Vector(ob1, DObject.empty, DObject("maybeMask" := "***"), DObject("maybeMask" := "***", "value" := 123)))).right.get
          ObjectsSanitize.$ops.sanitize(obj) shouldBe result
        }
      }
    }

    describe("Map objects sanitization") {
      describe("Empty map") {
        object EmptyContract extends Contract

        object EmptyMapSanitize extends Contract {
          val objects = \->(EmptyContract)
          val objectsMask = \->(EmptyContract, mask("***"))
        }

        it("Should be empty if empty") {
          EmptyMapSanitize.$ops.sanitize(DObject.empty) shouldBe DObject.empty
        }
        it("Should return objects unchanged") {
          val ob1 = DObject("value" := 3)
          val ob2 = DObject("value" := 5)
          val ob3 = DObject("another" := false)
          val obj = EmptyMapSanitize.$createValid(_.objects.$set(Map("one" -> ob1, "two" -> ob2, "three" -> ob3))).right.get

          EmptyMapSanitize.$ops.sanitize(obj) shouldBe obj
        }
        it("Should sanitize objects mask") {
          val ob1 = DObject("value" := 3)
          val ob2 = DObject("value" := 5)
          val ob3 = DObject("another" := false)
          val obj = EmptyMapSanitize.$createValid(_.objectsMask.$set(Map("one" -> ob1, "two" -> ob2, "three" -> ob3))).right.get
          EmptyMapSanitize.$ops.sanitize(obj) shouldBe DObject("objectsMask" := "***")
        }
      }
      describe("With sanitizing properties") {
        object MaybeMaskContract extends Contract {
          val maybeMask = \?[String](mask("***"))
          val maybeInternal = \?[Long](internal)
        }

        object MapSanitize extends Contract {
          val objects = \->(MaybeMaskContract)
        }

        it("Should sanitize objects that require sanitizing") {
          val ob1 = DObject("value" := 123)
          val ob2 = MaybeMaskContract.$create(_.maybeInternal.$set(1234L))
          val ob3 = MaybeMaskContract.$create(_.maybeMask.$set("value"))
          val ob4 = MaybeMaskContract.$create(c => c.maybeInternal.$set(1234L) ~ c.maybeMask.$set("value") ~+ ("value" := 123))
          val obj = MapSanitize.$createValid(_.objects.$set(Map("one" -> ob1, "two" -> ob2, "three" -> ob3, "four" -> ob4))).right.get
          val result = MapSanitize.$createValid(_.objects.$set(Map("one" -> ob1, "three" -> DObject("maybeMask" := "***"), "four" -> DObject("maybeMask" := "***", "value" := 123)))).right.get
          MapSanitize.$ops.sanitize(obj) shouldBe result
        }
      }
    }
  }

}
