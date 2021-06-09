package dsentric.contracts

import dsentric.codecs.DContractCodec
import dsentric.failure.{ExpectedFailure, IncorrectTypeFailure}
import dsentric.{DNull, DObject, Delta, Path}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ReduceDeltaSpec extends AnyFunSpec with Matchers with EitherValues {
  import dsentric.Dsentric._
  import dsentric.Implicits._

  describe("Value Property behaviour") {
    import dsentric.codecs.std.DCodecs._

    describe("Expected property") {
      object Expected extends Contract {
        val property = \[Int]
      }
      it("Should return empty if current and delta are empty") {
        val base = DObject.empty
        val delta = Delta.empty
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta are empty") {
        val base = DObject("property" := 123)
        val delta = Delta.empty
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := 123)
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if set against empty value") {
        val base = DObject.empty
        val delta = Delta("property" := 123)
        Expected.$reduceDelta(base, delta).value shouldBe delta
        Expected.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing value") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := 456)
        Expected.$reduceDelta(base, delta).value shouldBe delta
        Expected.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing incorrect type value") {
        val base = DObject("property" := "failed")
        val delta = Delta("property" := 456)
        Expected.$reduceDelta(base, delta).value shouldBe delta
        Expected.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if delta removing a value which doesn't exist") {
        val base = DObject.empty
        val delta = Delta("property" := DNull)
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return failure if delta removing an existing value") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := DNull)
        Expected.$reduceDelta(base, delta).left.value should contain (ExpectedFailure(Expected.property))
        Expected.$reduceDelta(base, delta, true).left.value should contain (ExpectedFailure(Expected.property))
      }
      it("Should return failure if delta removing an existing incorrect type value") {
        val base = DObject("property" := "failure")
        val delta = Delta("property" := DNull)
        Expected.$reduceDelta(base, delta).left.value should contain (ExpectedFailure(Expected.property))
        Expected.$reduceDelta(base, delta, true).left.value should contain (ExpectedFailure(Expected.property))
      }
      it("should return failure if setting incorrect type") {
        val base = DObject.empty
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Expected.property, "fail"))
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base = DObject("property" := "fail")
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Expected.property, "fail"))
      }
      it("Should return empty delta if setting incorrect type and dropBadTypes is true") {
        val base = DObject.empty
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("should return failure if modifying to an incorrect type") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Expected.property, "fail"))
      }
      it("Should return empty delta if modifying to an incorrect type and dropBadTypes is true") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if setting to an empty object") {
        val base = DObject.empty
        val delta = Delta("property" := DObject.empty)
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("Should return empty if modifying to an empty object") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := DObject.empty)
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
    }

    describe("Maybe property") {
      object Maybe extends Contract {
        val property = \?[Int]
      }
      it("Should return empty if current and delta are empty") {
        val base = DObject.empty
        val delta = Delta.empty
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta are empty") {
        val base = DObject("property" := 123)
        val delta = Delta.empty
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := 123)
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if set against empty value") {
        val base = DObject.empty
        val delta = Delta("property" := 123)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing value") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := 456)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing incorrect type value") {
        val base = DObject("property" := "failed")
        val delta = Delta("property" := 456)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if delta removing a value which doesn't exist") {
        val base = DObject.empty
        val delta = Delta("property" := DNull)
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if delta removing an existing value") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := DNull)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if delta removing an existing incorrect type value") {
        val base = DObject("property" := "failure")
        val delta = Delta("property" := DNull)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("should return failure if setting incorrect type") {
        val base = DObject.empty
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Maybe.property, "fail"))
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base = DObject("property" := "fail")
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Maybe.property, "fail"))
      }
      it("Should return empty delta if setting incorrect type and dropBadTypes is true") {
        val base = DObject.empty
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("should return failure if modifying to an incorrect type") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Maybe.property, "fail"))
      }
      it("Should return empty delta if modifying to an incorrect type and dropBadTypes is true") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if setting to an empty object") {
        val base = DObject.empty
        val delta = Delta("property" := DObject.empty)
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("Should return empty if modifying to an empty object") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := DObject.empty)
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
    }

    describe("Default property") {
      object Default extends Contract {
        val property = \![Int](0)
      }
      it("Should return empty if current and delta are empty") {
        val base = DObject.empty
        val delta = Delta.empty
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta are empty") {
        val base = DObject("property" := 123)
        val delta = Delta.empty
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := 123)
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if set against empty value") {
        val base = DObject.empty
        val delta = Delta("property" := 123)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing value") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := 456)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing incorrect type value") {
        val base = DObject("property" := "failed")
        val delta = Delta("property" := 456)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if delta removing a value which doesn't exist") {
        val base = DObject.empty
        val delta = Delta("property" := DNull)
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if delta removing an existing value") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := DNull)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if delta removing an existing incorrect type value") {
        val base = DObject("property" := "failure")
        val delta = Delta("property" := DNull)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("should return failure if setting incorrect type") {
        val base = DObject.empty
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Default.property, "fail"))
      }
      it("Should return empty delta if setting incorrect type and dropBadTypes is true") {
        val base = DObject.empty
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("should return failure if modifying to an incorrect type") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Default.property, "fail"))
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base = DObject("property" := "fail")
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(Default.property, "fail"))
      }
      it("Should return empty delta if modifying to an incorrect type and dropBadTypes is true") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if setting to an empty object") {
        val base = DObject.empty
        val delta = Delta("property" := DObject.empty)
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("Should return empty if modifying to an empty object") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := DObject.empty)
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("Should still apply the delta value even it it matches the default value and value is empty") {
        val base = DObject.empty
        val delta = Delta("property" := 0)
        Default.$reduceDelta(base, delta).value shouldBe delta
      }
      it("Should still apply the delta value even it it matches the default value and value is set") {
        val base = DObject("property" := 123)
        val delta = Delta("property" := 0)
        Default.$reduceDelta(base, delta).value shouldBe delta
      }
    }

    describe("Contract Codec Type") {
      object Nested extends Contract {
        val expected = \[String]
        val maybe = \?[Boolean]
      }
      object ContractCodec extends Contract {
        val property = \[DObject](DContractCodec(Nested))
      }
      it("Should return base if current empty and delta satisfies contract conditions") {
        val base = DObject.empty
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := false))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if current empty and delta does not satisfy contract conditions") {
        val base = DObject.empty
        val delta = Delta("property" ::= ("maybe" := false))
        ContractCodec.$reduceDelta(base, delta).left.value should contain (ExpectedFailure(ContractCodec, Path("property", "expected")))
      }
      it("Should return empty if current empty if delta does not satisfy contract conditions under dropBadTypes is true") {
        val base = DObject.empty
        val delta = Delta("property" ::= ("maybe" := false))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := true))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return reduced delta if current empty and contract can satisfy under dropBadTypes is true") {
        val base = DObject.empty
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := 1234))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta("property" ::= ("expected" := "value"))
      }
      it("Should return delta if changing current values") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := "value2", "maybe" := false))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if removing maybe contract current values ") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if removing expected contract current values") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := DNull))
        ContractCodec.$reduceDelta(base, delta).left.value should contain (ExpectedFailure(ContractCodec, Path("property", "expected")))
      }
      it("Should return empty delta if removing expected contract current values under dropBadTypes is true") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := DNull))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return failure if valid delta results in removing Expected object (Unusual case)") {
        val base = DObject("property" ::= ("maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).left.value should contain (ExpectedFailure(ContractCodec.property))
      }
      it("Should return delta if valid delta results in leaving object still invalid") {
        val base = DObject("property" ::= ("expected" := 123, "maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base = DObject("property" ::= ("expected" := 123, "maybe" := "fail"))
        val delta = Delta("property" ::= ("maybe" := "fail"))
        ContractCodec.$reduceDelta(base, delta).left.value should contain (IncorrectTypeFailure(ContractCodec, Path("property", "maybe"), Nested.maybe._codec, "fail"))
      }
    }

    describe("Map Codec Type") {

    }
    describe("Collection Codec Type") {

    }
    describe("Coproduct Codec Type") {

    }
  }

  describe("Object Property behaviour") {

  }

  describe("Additional Property behaviour") {

  }
}
