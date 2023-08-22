package dsentric.contracts

import dsentric.codecs.{
  DContractCodec,
  DCoproductCodec,
  DKeyContractCollectionCodec,
  DParameterisedContractCodec,
  DTypeContractCodec,
  DValueCodec
}
import dsentric.failure.{
  ClosedContractFailure,
  ContractTypeResolutionFailure,
  CoproductTypeValueFailure,
  ExpectedFailure,
  ImmutableFailure,
  IncorrectKeyTypeFailure,
  IncorrectTypeFailure,
  MaskFailure,
  MissingElementFailure,
  ReservedFailure
}
import dsentric.schema.ObjectDefinition
import dsentric.{DNull, DObject, DObjectOps, Data, Delta, Path, RawObject}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import shapeless.HList

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
        val base  = DObject.empty
        val delta = Delta.empty
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta are empty") {
        val base  = DObject("property" := 123)
        val delta = Delta.empty
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := 123)
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if set against empty value") {
        val base  = DObject.empty
        val delta = Delta("property" := 123)
        Expected.$reduceDelta(base, delta).value shouldBe delta
        Expected.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing value") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := 456)
        Expected.$reduceDelta(base, delta).value shouldBe delta
        Expected.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing incorrect type value") {
        val base  = DObject("property" := "failed")
        val delta = Delta("property" := 456)
        Expected.$reduceDelta(base, delta).value shouldBe delta
        Expected.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if delta removing a value which doesn't exist") {
        val base  = DObject.empty
        val delta = Delta("property" := DNull)
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return failure if delta removing an existing value") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := DNull)
        Expected.$reduceDelta(base, delta).left.value should contain(ExpectedFailure(Expected.property))
        Expected.$reduceDelta(base, delta, true).left.value should contain(ExpectedFailure(Expected.property))
      }
      it("Should return failure if delta removing an existing incorrect type value") {
        val base  = DObject("property" := "failure")
        val delta = Delta("property" := DNull)
        Expected.$reduceDelta(base, delta).left.value should contain(ExpectedFailure(Expected.property))
        Expected.$reduceDelta(base, delta, true).left.value should contain(ExpectedFailure(Expected.property))
      }
      it("should return failure if setting incorrect type") {
        val base  = DObject.empty
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Expected.property, "fail"))
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base  = DObject("property" := "fail")
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Expected.property, "fail"))
      }
      it("Should return empty delta if setting incorrect type and dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("should return failure if modifying to an incorrect type") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Expected.property, "fail"))
      }
      it("Should return empty delta if modifying to an incorrect type and dropBadTypes is true") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Expected.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if setting to an empty object") {
        val base  = DObject.empty
        val delta = Delta("property" := DObject.empty)
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("Should return empty if modifying to an empty object") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := DObject.empty)
        Expected.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
    }

    describe("Maybe property") {
      object Maybe extends Contract {
        val property = \?[Int]
      }
      it("Should return empty if current and delta are empty") {
        val base  = DObject.empty
        val delta = Delta.empty
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta are empty") {
        val base  = DObject("property" := 123)
        val delta = Delta.empty
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := 123)
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if set against empty value") {
        val base  = DObject.empty
        val delta = Delta("property" := 123)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing value") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := 456)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing incorrect type value") {
        val base  = DObject("property" := "failed")
        val delta = Delta("property" := 456)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if delta removing a value which doesn't exist") {
        val base  = DObject.empty
        val delta = Delta("property" := DNull)
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if delta removing an existing value") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := DNull)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if delta removing an existing incorrect type value") {
        val base  = DObject("property" := "failure")
        val delta = Delta("property" := DNull)
        Maybe.$reduceDelta(base, delta).value shouldBe delta
        Maybe.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("should return failure if setting incorrect type") {
        val base  = DObject.empty
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Maybe.property, "fail"))
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base  = DObject("property" := "fail")
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Maybe.property, "fail"))
      }
      it("Should return empty delta if setting incorrect type and dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("should return failure if modifying to an incorrect type") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Maybe.property, "fail"))
      }
      it("Should return empty delta if modifying to an incorrect type and dropBadTypes is true") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Maybe.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if setting to an empty object") {
        val base  = DObject.empty
        val delta = Delta("property" := DObject.empty)
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("Should return empty if modifying to an empty object") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := DObject.empty)
        Maybe.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
    }

    describe("Default property") {
      object Default extends Contract {
        val property = \![Int](0)
      }
      it("Should return empty if current and delta are empty") {
        val base  = DObject.empty
        val delta = Delta.empty
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta are empty") {
        val base  = DObject("property" := 123)
        val delta = Delta.empty
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := 123)
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if set against empty value") {
        val base  = DObject.empty
        val delta = Delta("property" := 123)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing value") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := 456)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if changing existing incorrect type value") {
        val base  = DObject("property" := "failed")
        val delta = Delta("property" := 456)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if delta removing a value which doesn't exist") {
        val base  = DObject.empty
        val delta = Delta("property" := DNull)
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if delta removing an existing value") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := DNull)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if delta removing an existing incorrect type value") {
        val base  = DObject("property" := "failure")
        val delta = Delta("property" := DNull)
        Default.$reduceDelta(base, delta).value shouldBe delta
        Default.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("should return failure if setting incorrect type") {
        val base  = DObject.empty
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Default.property, "fail"))
      }
      it("Should return empty delta if setting incorrect type and dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("should return failure if modifying to an incorrect type") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Default.property, "fail"))
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base  = DObject("property" := "fail")
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta).left.value should contain(IncorrectTypeFailure(Default.property, "fail"))
      }
      it("Should return empty delta if modifying to an incorrect type and dropBadTypes is true") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := "fail")
        Default.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if setting to an empty object") {
        val base  = DObject.empty
        val delta = Delta("property" := DObject.empty)
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("Should return empty if modifying to an empty object") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := DObject.empty)
        Default.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("Should still apply the delta value even it it matches the default value and value is empty") {
        val base  = DObject.empty
        val delta = Delta("property" := 0)
        Default.$reduceDelta(base, delta).value shouldBe delta
      }
      it("Should still apply the delta value even it it matches the default value and value is set") {
        val base  = DObject("property" := 123)
        val delta = Delta("property" := 0)
        Default.$reduceDelta(base, delta).value shouldBe delta
      }
    }

    describe("Contract Codec Type") {
      object Nested        extends Contract {
        val expected = \[String]
        val maybe    = \?[Boolean]
      }
      object ContractCodec extends Contract {
        val property = \[DObject](DContractCodec(Nested))
      }
      it("Should return delta if current empty and delta satisfies contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := false))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if current empty and delta does not satisfy contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("maybe" := false))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "expected"))
        )
      }
      it("Should return empty if current empty if delta does not satisfy contract conditions under dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("maybe" := false))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base  = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := true))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return reduced delta if current empty and contract can satisfy under dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := 1234))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta("property" ::= ("expected" := "value"))
      }
      it("Should return delta if changing current values") {
        val base  = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := "value2", "maybe" := false))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if removing maybe contract current values ") {
        val base  = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if removing not existing keys") {
        val base  = DObject("property" ::= ("expected" := "value"))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return failure if removing expected contract current values") {
        val base  = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := DNull))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "expected"))
        )
      }
      it("Should return empty if removing expected contract on empty object (Unusual case)") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        val base2 = DObject("property" := DObject.empty)
        ContractCodec.$reduceDelta(base2, delta).value shouldBe Delta.empty
      }
      it("Should return empty delta if removing expected contract current values under dropBadTypes is true") {
        val base  = DObject("property" ::= ("expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := DNull))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if valid delta results in removing Expected object ") {
        val base  = DObject("property" ::= ("maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if valid delta results in leaving object still invalid") {
        val base  = DObject("property" ::= ("expected" := 123, "maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base  = DObject("property" ::= ("expected" := 123, "maybe" := "fail"))
        val delta = Delta("property" ::= ("maybe" := "fail"))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ContractCodec, Path("property", "maybe"), Nested.maybe._codec, "fail")
        )
      }
    }
    describe("Contract Parameterised Codec Type") {
      case class Parametric(id: Long, _type: String, value: Map[String, Any])
          extends DObject
          with DObjectOps[Parametric] {
        protected def wrap(value: RawObject): Parametric = Parametric(id, _type, value)
      }
      object Parametric extends ContractFor[Parametric] with Closed {
        val expected = \[String]
        val maybe    = \?[Boolean]
        val default  = \![Int](0)
      }

      object ContractCodec extends Contract {
        val property = \[Parametric](DParameterisedContractCodec(Parametric))
      }
      it("Should return delta if current empty and delta satisfies contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := false))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if current empty and delta does not satisfy contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("id" := 482, "_type" := "test", "maybe" := false))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "expected"))
        )
      }
      it("Should return failure if current empty and delta does not satisfy parameter conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("_type" := "test", "expected" := "value"))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "id"))
        )
      }
      it("Should return empty if current empty if delta does not satisfy contract conditions under dropBadTypes is true") {
        val base   = DObject.empty
        val delta  = Delta("property" ::= ("id" := 482, "expected" := "value", "maybe" := false))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
        val delta2 = Delta("property" ::= ("id" := 482, "_type" := "test", "expected" := 482392, "maybe" := false))
        ContractCodec.$reduceDelta(base, delta2, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base  = DObject(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "expected" := "value", "maybe" := true)
        )
        val delta = Delta(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "expected" := "value", "maybe" := true)
        )
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return reduced delta if current empty and contract can satisfy under dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := 1234))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value")
        )
      }
      it("Should return delta if changing current values") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := "value2", "maybe" := false))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if removing maybe contract current values ") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if removing not existing keys") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value"))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return failure if removing expected contract current values") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := DNull))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "expected"))
        )
      }
      it("Should return failure if removing param value") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("_type" := DNull))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "_type"))
        )
      }
      it("Should return failure if setting param value to wrong type") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("id" := "fail"))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ContractCodec, Path("property", "id"), longCodec, "fail")
        )
      }
      it("Should return failure if removing param value even if not originally set") {
        val base  = DObject("property" ::= ("id" := 482, "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("_type" := DNull))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "_type"))
        )
      }
      it("Should return failure if delta doesnt fix broken params") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := 482, "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := "value2"))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ContractCodec, Path("property", "_type"), stringCodec, 482L)
        )
      }
      it("Should return empty if removing expected contract on empty object (Unusual case)") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        val base2 = DObject("property" := DObject.empty)
        ContractCodec.$reduceDelta(base2, delta).value shouldBe Delta.empty
      }
      it("Should return empty if removing param on empty object (Unusual case)") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("id" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        val base2 = DObject("property" := DObject.empty)
        ContractCodec.$reduceDelta(base2, delta).value shouldBe Delta.empty
      }
      it("Should return empty delta if removing expected contract current values under dropBadTypes is true") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := 482, "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("expected" := DNull))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty delta if removing param current values under dropBadTypes is true") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := 482, "expected" := "value", "maybe" := true))
        val delta = Delta("property" ::= ("id" := DNull))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if valid delta results in removing Expected object ") {
        val base  = DObject("property" ::= ("maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if valid delta results in leaving object still invalid") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := 123, "maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := 123, "maybe" := "fail"))
        val delta = Delta("property" ::= ("maybe" := "fail"))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ContractCodec, Path("property", "maybe"), booleanCodec, "fail")
        )
      }
      it("should return failure if setting incorrect type on param even if its same type as current") {
        val base  = DObject("property" ::= ("id" := 482, "_type" := false, "expected" := 123))
        val delta = Delta("property" ::= ("_type" := false))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ContractCodec, Path("property", "_type"), stringCodec, false)
        )
      }
    }

    describe("Contract Key Collection Codec Type") {

      case class WithKey(key: String, value: Map[String, Any]) extends DObject {
        protected def wrap(value: RawObject): DObject = this.copy(value = value)
      }
      case class WithKeys(values: Vector[WithKey])
      case class AtleastTwoWithKeys(values: Vector[WithKey])

      object WithKeyContract extends ContractFor[WithKey] with Open {
        val expected = \[String]
        val maybe    = \?[Boolean]
        val default  = \![Int](0)
      }
      object BlankContract   extends ContractFor[WithKey] with Open
      implicit val D: DKeyContractCollectionCodec[WithKeys, WithKey]            = DKeyContractCollectionCodec[WithKeys, WithKey](
        WithKeyContract,
        (k,d) => Some(WithKey(k,d)),
        w => w.key -> w.value,
        a => Some(WithKeys(a)),
        _.values
      )
      implicit val D2: DKeyContractCollectionCodec[AtleastTwoWithKeys, WithKey] =
        DKeyContractCollectionCodec[AtleastTwoWithKeys, WithKey](
          BlankContract,
          (k,d) => Some(WithKey(k,d)),
          w => w.key -> w.value,
          a => if (a.size <= 1) None else Some(AtleastTwoWithKeys(a)),
          _.values
        )

      object ContractCodec extends Contract {
        val property = \[WithKeys]
      }

      object ContractAtLeastTwoCodec extends Contract {
        val property = \?[AtleastTwoWithKeys]
      }

      it("Should return delta if current empty and delta satisfies contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("one" ::= ("expected" := "value", "maybe" := false)))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if current empty and delta does not satisfy contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("one" ::= ("maybe" := false)))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "one", "expected"))
        )
      }
      it("Should return empty if current empty if delta does not satisfy contract conditions under dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("one" ::= ("maybe" := false)))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base  = DObject("property" ::= ("one" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("property" ::= ("one" ::= ("expected" := "value", "maybe" := true)))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return reduced delta if current empty and contract can satisfy under dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("one" ::= ("expected" := "value", "maybe" := 1234)))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta(
          "property" ::= ("one" ::= ("expected" := "value"))
        )
      }
      it("Should return delta if changing current values") {
        val base  = DObject("property" ::= ("one" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("property" ::= ("one" ::= ("expected" := "value2", "maybe" := false)))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if removing maybe contract current values ") {
        val base  = DObject("property" ::= ("one" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("property" ::= ("one" ::= ("maybe" := DNull)))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if removing not existing keys") {
        val base  = DObject("property" ::= ("one" ::= ("expected" := "value")))
        val delta = Delta("property" ::= ("one" ::= ("maybe" := DNull)))
        ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return failure if removing expected contract current values") {
        val base  = DObject("property" ::= ("one" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("property" ::= ("one" ::= ("expected" := DNull)))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "one", "expected"))
        )
      }
      it("Should return empty if removing expected contract on empty object (Unusual case)") {
        //val base  = DObject.empty
        val delta = Delta("property" ::= ("one" ::= ("expected" := DNull)))
        //ContractCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        val base2 = DObject("property" ::= ("one" := DObject.empty))
        ContractCodec.$reduceDelta(base2, delta).value shouldBe Delta.empty
      }
      it("Should return empty delta if removing expected contract current values under dropBadTypes is true") {
        val base  = DObject("property" ::= ("one" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("property" ::= ("one" ::= ("expected" := DNull)))
        ContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return failure if valid delta results in removing object of Expected property") {
        val base  = DObject("property" ::= ("one" ::= ("maybe" := true)))
        val delta = Delta("property" ::= ("one" ::= ("maybe" := DNull)))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(ExpectedFailure(ContractCodec.property))
        ContractCodec.$reduceDelta(base, delta, true).left.value should contain(ExpectedFailure(ContractCodec.property))
      }
      it("Should return delta if valid delta results in leaving object still invalid") {
        val base  = DObject("property" ::= ("one" ::= ("expected" := 123, "maybe" := true)))
        val delta = Delta("property" ::= ("one" ::= ("maybe" := DNull)))
        ContractCodec.$reduceDelta(base, delta).value shouldBe delta
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base  = DObject("property" ::= ("one" ::= ("expected" := 123, "maybe" := "fail")))
        val delta = Delta("property" ::= ("one" ::= ("maybe" := "fail")))
        ContractCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ContractCodec, Path("property", "one", "maybe"), WithKeyContract.maybe._codec, "fail")
        )
      }
      it("should return failure if result no longer satisfies type") {
        val base  =
          DObject("property" ::= ("one" ::= ("expected" := 123, "maybe" := "fail"), "two" ::= ("expected" := 4546)))
        val delta = Delta("property" ::= ("two" := DNull))
        ContractAtLeastTwoCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(
            ContractAtLeastTwoCodec,
            Path("property"),
            ContractAtLeastTwoCodec.property._codec,
            DObject("one" ::= ("expected" := 123, "maybe" := "fail")).value
          )
        )
      }
    }
    describe("Map Codec Type") {
      object Nested   extends Contract {
        val expected = \[String]
        val maybe    = \?[Boolean]
      }
      object MapCodec extends Contract {
        val property = \[Map[Length4String, Int]]

        val codecProperty = \?[Map[String, DObject]](Nested)
      }
      it("Should return delta if current empty and delta satisfies contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("key1" := 1234, "key2" := 5678))
        MapCodec.$reduceDelta(base, delta).value shouldBe delta
        MapCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if current empty and delta does not satisfy contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("key25" := 1234, "key2" := "bob"))
        MapCodec.$reduceDelta(base, delta).left.value should contain allOf (
          IncorrectKeyTypeFailure(MapCodec, Path("property"), Length4String.fixedLength4StringCodec, "key25"),
          IncorrectTypeFailure(MapCodec, Path("property", "key2"), intCodec, "bob")
        )
      }
      it("Should return empty if current empty if delta does not satisfy contract conditions under dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("key2" := "bob"))
        MapCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current set and delta is the same") {
        val base  = DObject("property" ::= ("key1" := 1234, "key2" := 5678))
        val delta = Delta("property" ::= ("key1" := 1234, "key2" := 5678))
        MapCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        MapCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if delta removed maybe property") {
        val base  = DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("codecProperty" := DNull)
        MapCodec.$reduceDelta(base, delta).value shouldBe delta
        MapCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return reduced delta if current empty and contract can satisfy under dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("key25" := 1234, "key2" := "bob", "key4" := 567))
        MapCodec.$reduceDelta(base, delta, true).value shouldBe Delta("property" ::= ("key4" := 567))
      }
      it("Should return delta if changing current values") {
        val base  = DObject("property" ::= ("key1" := 567, "key2" := 0, "key3" := 7891))
        val delta = Delta("property" ::= ("key1" := 778, "key2" := DNull, "key4" := 56))
        MapCodec.$reduceDelta(base, delta).value shouldBe delta
        MapCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if removing not existing keys") {
        val base  = DObject("property" ::= ("key1" := 567, "key2" := 0))
        val delta = Delta("property" ::= ("key3" := DNull))
        MapCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        MapCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if clearing down maybe map") {
        val base  = DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value")))
        val delta = Delta("codecProperty" ::= ("obj1" := DNull))
        MapCodec.$reduceDelta(base, delta).value shouldBe delta
        MapCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if valid delta results in removing Expected object (Unusual case)") {
        val base  = DObject("property" ::= ("maybe" := true))
        val delta = Delta("property" ::= ("maybe" := DNull))
        MapCodec.$reduceDelta(base, delta).left.value should contain(ExpectedFailure(MapCodec.property))
        MapCodec.$reduceDelta(base, delta, true).left.value should contain(ExpectedFailure(MapCodec.property))
      }
      it("Should not return delta if valid delta results in leaving object still invalid") {
        val base  = DObject("property" ::= ("key25" := 1234, "key2" := "bob"))
        val delta = Delta("property" ::= ("key4" := 45434))
        MapCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(MapCodec.property, Map("key25" -> 1234, "key2" -> "bob", "key4" -> 45434))
        )
      }
      it("Should allow removal of invalid key even though even key is used to remove") {
        val base  = DObject("property" ::= ("key25" := 1234, "key2" := 41234))
        val delta = Delta("property" ::= ("key25" := DNull))
        MapCodec.$reduceDelta(base, delta).value shouldBe delta
      }
      it("Should ignore invalid key if its getting cleared out") {
        val base  = DObject("property" ::= ("key2" := 1234, "key2" := "bob"))
        val delta = Delta("property" ::= ("key35" := DNull, "key45" ::= ("nested" := DObject.empty)))
        MapCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
      }
      it("should return failure if setting incorrect type even if its same type as current") {
        val base   = DObject("property" ::= ("key2" := "bob"))
        val delta  = Delta("property" ::= ("key2" := "bob"))
        MapCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(MapCodec, Path("property", "key2"), intCodec, "bob")
        )
        val base2  = DObject("property" ::= ("key25" := 1234))
        val delta2 = Delta("property" ::= ("key25" := 1234))
        MapCodec.$reduceDelta(base2, delta2).left.value should contain(
          IncorrectKeyTypeFailure(MapCodec, Path("property"), Length4String.fixedLength4StringCodec, "key25")
        )
      }
      it("Should fail if nested contract fails to validate") {
        val base   = DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value", "maybe" := true)))
        val delta  = Delta("codecProperty" ::= ("obj1" ::= ("expected" := DNull)))
        MapCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(MapCodec, Path("codecProperty", "obj1", "expected"))
        )
        val delta2 = Delta("codecProperty" ::= ("obj1" ::= ("maybe" := "fail")))
        MapCodec.$reduceDelta(base, delta2).left.value should contain(
          IncorrectTypeFailure(MapCodec, Path("codecProperty", "obj1", "maybe"), Nested.maybe._codec, "fail")
        )
      }
      it("Should return empty if nested contract expected fails under drop under bad type ") {
        val base  = DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("codecProperty" ::= ("obj1" ::= ("expected" := DNull)))
        MapCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if nested contract type fails under drop under bad type ") {
        val base  = DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("codecProperty" ::= ("obj1" ::= ("maybe" := 1234)))
        MapCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if removing expected content from empty nested contract (Unusual case)") {
        val base  = DObject.empty
        val delta = Delta("codecProperty" ::= ("key1" ::= ("expected" := DNull)))
        MapCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        val base2 = DObject("codecProperty" ::= ("key1" := DObject.empty))
        MapCodec.$reduceDelta(base2, delta).value shouldBe Delta.empty
      }
    }
    describe("Collection Codec Type") {
      object Nested          extends Contract {
        val expected = \[String]
        val maybe    = \?[Boolean]
      }
      object CollectionCodec extends Contract {
        val property = \[List[Int]]

        val codecProperty = \?[List[DObject]](Nested)
      }
      it("Should return delta if current empty and delta satisfies contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" := List(1, 2, 3, 4))
        CollectionCodec.$reduceDelta(base, delta).value shouldBe delta
        CollectionCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if current empty and delta does not satisfy contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" := "a")
        CollectionCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(CollectionCodec, Path("property"), CollectionCodec.property._codec, "a")
        )
      }
      it("Should return failure if current empty and collection element fails type") {
        val base  = DObject.empty
        val delta = Delta("property" := List(Data(12), Data("b")))
        CollectionCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(CollectionCodec, Path("property", 1), intCodec, "b")
        )
      }
      it("Should return delta if setting to empty collection") {
        val base  = DObject("property" := List(1, 2, 3, 4))
        val delta = Delta("property" := List.empty[Int])
        CollectionCodec.$reduceDelta(base, delta).value shouldBe delta
        CollectionCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if delta removed maybe property") {
        val base  = DObject("codecProperty" := List(DObject("expected" := "value")))
        val delta = Delta("codecProperty" := DNull)
        CollectionCodec.$reduceDelta(base, delta).value shouldBe delta
        CollectionCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }

      it("Should return failure if setting to incorrect type, even if its the same as original") {
        val base   = DObject("property" := "a")
        val delta  = Delta("property" := "a")
        CollectionCodec.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(CollectionCodec, Path("property"), CollectionCodec.property._codec, "a")
        )
        val base2  = DObject("property" := List(Data(12), Data("b")))
        val delta2 = Delta("property" := List(Data(12), Data("b")))
        CollectionCodec.$reduceDelta(base2, delta2).left.value should contain(
          IncorrectTypeFailure(CollectionCodec, Path("property", 1), intCodec, "b")
        )
      }

      it("Should fail if element is null and thats not a valid type") {
        val base  = DObject("property" := List(12, 45))
        val delta = Delta("property" := List(Data(54), DNull))
        CollectionCodec.$reduceDelta(base, delta).left.value should contain(
          MissingElementFailure(CollectionCodec, CollectionCodec.property._codec, Path("property", 1))
        )
      }
      it("Should fail if nested contract fails to validate") {
        val base  = DObject("codecProperty" := List.empty[DObject])
        val delta = Delta("codecProperty" := List(DObject("maybe" := false)))
        CollectionCodec.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(CollectionCodec, Path("codecProperty", 0, "expected"))
        )
      }
      it("Should return empty delta if contract fails to validate under drop bad types ") {
        val base  = DObject("codecProperty" := List.empty[DObject])
        val delta = Delta("codecProperty" := List(DObject("expected" := DNull)))
        CollectionCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
    }
    describe("Coproduct Codec Type") {
      object Nested         extends Contract {
        val expected = \[String]
        val maybe    = \?[Boolean]
      }
      object CoproductCodec extends Contract {
        val property = \[Either[Int, String]]

        val codecProperty = \?[Either[Long, DObject]](Nested)
      }
      it("Should return delta if current empty and delta satisfies a condition") {
        val base   = DObject.empty
        val delta  = Delta("property" := 1)
        val delta2 = Delta("property" := "bob")
        CoproductCodec.$reduceDelta(base, delta).value shouldBe delta
        CoproductCodec.$reduceDelta(base, delta, true).value shouldBe delta
        CoproductCodec.$reduceDelta(base, delta2).value shouldBe delta2
        CoproductCodec.$reduceDelta(base, delta2, true).value shouldBe delta2
      }
      it("Should return failure if current empty and delta does not satisfy contract conditions") {
        val base  = DObject.empty
        val delta = Delta("property" := false)
        CoproductCodec.$reduceDelta(base, delta).left.value should contain(
          CoproductTypeValueFailure(
            CoproductCodec,
            CoproductCodec.property._codec.asInstanceOf[DCoproductCodec[Either[Int, String], HList]],
            Path("property"),
            List(
              IncorrectTypeFailure(CoproductCodec, Path("property"), intCodec, false),
              IncorrectTypeFailure(CoproductCodec, Path("property"), stringCodec, false)
            ),
            false
          )
        )
      }
      it("Should return empty if current empty if delta does not satisfy contract conditions under dropBadTypes is true") {
        val base  = DObject.empty
        val delta = Delta("property" := false)
        CoproductCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if removing maybe property") {
        val base  = DObject("codecProperty" := 123L)
        val delta = Delta("codecProperty" := DNull)
        CoproductCodec.$reduceDelta(base, delta).value shouldBe delta
      }
      it("Should return empty if current set and delta is the same") {
        val base   = DObject("property" := 1234)
        val delta  = Delta("property" := 1234)
        CoproductCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        CoproductCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
        val base2  = DObject("property" := "a")
        val delta2 = Delta("property" := "a")
        CoproductCodec.$reduceDelta(base2, delta2).value shouldBe Delta.empty
        CoproductCodec.$reduceDelta(base2, delta2, true).value shouldBe Delta.empty
      }
      it("Should return delta if changing current values of same type") {
        val base   = DObject("property" := 2)
        val delta  = Delta("property" := 4)
        CoproductCodec.$reduceDelta(base, delta).value shouldBe delta
        CoproductCodec.$reduceDelta(base, delta, true).value shouldBe delta
        val base2  = DObject("property" := "a")
        val delta2 = Delta("property" := "b")
        CoproductCodec.$reduceDelta(base2, delta2).value shouldBe delta2
        CoproductCodec.$reduceDelta(base2, delta2, true).value shouldBe delta2
      }
      it("Should return delta if changing current values of different types") {
        val base  = DObject("property" := 2)
        val delta = Delta("property" := "bob")
        CoproductCodec.$reduceDelta(base, delta).value shouldBe delta
        CoproductCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if clearing down contract") {
        val base  = DObject("codecProperty" ::= ("maybe" := "value"))
        val delta = Delta("codecProperty" ::= ("maybe" := DNull))
        CoproductCodec.$reduceDelta(base, delta).value shouldBe delta
        CoproductCodec.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if removing expected content from empty contract (Unusual case)") {
        val base  = DObject.empty
        val delta = Delta("codecProperty" ::= ("expected" := DNull))
        CoproductCodec.$reduceDelta(base, delta).value shouldBe Delta.empty
        val base2 = DObject("codecProperty" := DObject.empty)
        CoproductCodec.$reduceDelta(base2, delta).value shouldBe Delta.empty
      }
    }
    describe("Type Contract Codec Type") {

      object Type1             extends Contract {
        val _type     = \[String]("type")(DValueCodec.literal("type1"))
        val property1 = \[String]
        val property2 = \?[Int]
      }
      object Type2             extends Contract {
        val _type     = \[String]("type")(DValueCodec.literal("type2"))
        val property1 = \[Int]
        val property2 = \?[String]
      }
      object TypeContractCodec extends Contract {
        val property = \[DObject](DTypeContractCodec(ObjectDefinition.empty) {
          case Type1._type(_) => Type1
          case Type2._type(_) => Type2
        })
      }
      it("Should return delta if current empty and satisfies Delta Conditions") {
        val base   = DObject.empty
        val delta1 = Delta("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        TypeContractCodec.$reduceDelta(base, delta1).value shouldBe delta1
        TypeContractCodec.$reduceDelta(base, delta1, true).value shouldBe delta1
        val delta2 = Delta("property" ::= ("type" := "type2", "property1" := 123, "property2" := "value"))
        TypeContractCodec.$reduceDelta(base, delta2).value shouldBe delta2
        TypeContractCodec.$reduceDelta(base, delta2, true).value shouldBe delta2
      }
      it("Should return failure if current empty and type contract not satisfied") {
        val base   = DObject.empty
        val delta1 = Delta("property" ::= ("type" := "type1", "property1" := false, "property2" := 123))
        TypeContractCodec.$reduceDelta(base, delta1).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property1"), stringCodec, false)
        )
        val delta2 = Delta("property" ::= ("type" := "type2", "property1" := false, "property2" := "value"))
        TypeContractCodec.$reduceDelta(base, delta2).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property1"), intCodec, false)
        )
      }

      it("Should return empty if current empty and type contract not satisfied with Drop Bad Types is true") {
        val base   = DObject.empty
        val delta1 = Delta("property" ::= ("type" := "type1", "property1" := false, "property2" := 123))
        TypeContractCodec.$reduceDelta(base, delta1, true).value shouldBe Delta.empty
        val delta2 = Delta("property" ::= ("type" := "type2", "property1" := false, "property2" := "value"))
        TypeContractCodec.$reduceDelta(base, delta2, true).value shouldBe Delta.empty
      }
      it("Should return failure if current empty and type identifier not resolving") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("type" := "type3", "property1" := false, "property2" := 123))
        TypeContractCodec.$reduceDelta(base, delta).left.value should contain(
          ContractTypeResolutionFailure(
            TypeContractCodec,
            Path("property"),
            Map("type" -> "type3", "property1" -> false, "property2" -> 123)
          )
        )
      }
      it("Should return empty if current empty and type identifier not resolving with Drop Bad Types is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("type" := "type3", "property1" := false, "property2" := 123))
        TypeContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return reduce delta if current empty") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("type" := "type1", "property1" := "value", "property2" := DNull))
        TypeContractCodec.$reduceDelta(base, delta, true).value shouldBe Delta(
          "property" ::= ("type" := "type1", "property1" := "value")
        )
      }
      it("Should return delta if updating current type correctly") {
        val base1  = DObject("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        val delta1 = Delta("property" ::= ("property2" := -347))
        TypeContractCodec.$reduceDelta(base1, delta1).value shouldBe delta1
        TypeContractCodec.$reduceDelta(base1, delta1, true).value shouldBe delta1
        val base2  = DObject("property" ::= ("type" := "type2", "property1" := 123, "property2" := "value"))
        val delta2 = Delta("property" ::= ("property2" := "value2"))
        TypeContractCodec.$reduceDelta(base2, delta2).value shouldBe delta2
        TypeContractCodec.$reduceDelta(base2, delta2, true).value shouldBe delta2
      }
      it("Should return failure if updating current type with failure") {
        val base1  = DObject("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        val delta1 = Delta("property" ::= ("property2" := "value2"))
        TypeContractCodec.$reduceDelta(base1, delta1).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property2"), intCodec, "value2")
        )
        val base2  = DObject("property" ::= ("type" := "type2", "property1" := 123, "property2" := "value"))
        val delta2 = Delta("property" ::= ("property2" := -346))
        TypeContractCodec.$reduceDelta(base2, delta2).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property2"), stringCodec, -346)
        )
      }
      it("Should return success if changing type to correct new type") {
        val base1  = DObject("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        val delta1 = Delta("property" ::= ("type" := "type2", "property1" := 1234, "property2" := DNull))
        TypeContractCodec.$reduceDelta(base1, delta1).value shouldBe delta1
        val base2  = DObject("property" ::= ("type" := "type2", "property1" := 123, "property2" := "value"))
        val delta2 = Delta("property" ::= ("type" := "type1", "property1" := "value", "property2" := 5456))
        TypeContractCodec.$reduceDelta(base2, delta2).value shouldBe delta2
      }
      it("Should return success if changing type to correct new type and old type resolution was invalid") {
        val base  = DObject("property" ::= ("type" := "type3", "property1" := "value", "property2" := 123))
        val delta = Delta("property" ::= ("type" := "type2", "property1" := 1234, "property2" := DNull))
        TypeContractCodec.$reduceDelta(base, delta).value shouldBe delta
      }
      it("Should return failure if changing type to new type but with failing properties") {
        val base1  = DObject("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        val delta1 = Delta("property" ::= ("type" := "type2", "property1" := 1234, "property2" := false))
        TypeContractCodec.$reduceDelta(base1, delta1).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property2"), stringCodec, false)
        )
      }
      it("Should return failure if changing type to new type but leaving in failed properties which now fail") {
        val base1  = DObject("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        val delta1 = Delta("property" ::= ("type" := "type2", "property1" := 1234))
        TypeContractCodec.$reduceDelta(base1, delta1).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property2"), stringCodec, 123)
        )
      }
      it("Should return reduced delta if changing type to new type with failing maybe properties in with Drop Bad Types is true") {
        val base1  = DObject("property" ::= ("type" := "type1", "property1" := "value"))
        val delta1 = Delta("property" ::= ("type" := "type2", "property1" := 1234, "property2" := 123))
        TypeContractCodec.$reduceDelta(base1, delta1, true).value shouldBe Delta(
          "property" ::= ("type" := "type2", "property1" := 1234)
        )
      }
      it("Should return empty if changing type to new type with failing properties left in with Drop Bad Types is true (Unusual case)") {
        val base1  = DObject("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        val delta1 = Delta("property" ::= ("type" := "type2", "property1" := 1234))
        TypeContractCodec.$reduceDelta(base1, delta1, true).value shouldBe Delta.empty
      }

      it("Should return empty if same type and values match") {
        val base1  = DObject("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        val delta1 = Delta("property" ::= ("type" := "type1", "property1" := "value", "property2" := 123))
        TypeContractCodec.$reduceDelta(base1, delta1).value shouldBe Delta.empty
        TypeContractCodec.$reduceDelta(base1, delta1, true).value shouldBe Delta.empty
      }
    }

    describe("Constraints") {
      import dsentric.operators.StandardOperators._

      object Constrained extends Contract {
        val expected = \[String](immutable)
        val maybe    = \?[Int](internal)
        val default  = \![String]("value", mask("******"))

        val expectedObject = new \\(immutable) {
          val property1 = \[Int]
          val property2 = \?[String]
        }
      }

      it("Should return delta if constraint not trigger") {
        val base  = DObject.empty
        val delta = Delta("expected" := "value", "default" := "value")
        Constrained.$reduceDelta(base, delta).value shouldBe delta
      }
      it("Should return failure if constraint triggered") {
        val base  = DObject("expected" := "value")
        val delta = Delta("expected" := "value2", "default" := "******", "maybe" := 123)
        Constrained.$reduceDelta(base, delta).left.value should contain allOf (
          ImmutableFailure(Constrained, Path("expected")),
          ReservedFailure(Constrained, Path("maybe")),
          MaskFailure(Constrained, Path("default"), "******")
        )
      }

      it("Should return delta if Object constraint not triggered") {
        val base  = DObject.empty
        val delta = Delta("expectedObject" ::= ("property1" := 123, "property2" := "value"))
        Constrained.$reduceDelta(base, delta).value shouldBe delta
      }
      it("Should return failure if Object constraint triggered") {
        val base  = DObject("expectedObject" ::= ("property1" := 123, "property2" := "value"))
        val delta = Delta("expectedObject" ::= ("property2" := "value2"))
        Constrained.$reduceDelta(base, delta).left.value should contain(
          ImmutableFailure(Constrained, Path("expectedObject"))
        )
      }
    }
  }

  describe("Object Property behaviour") {
    import dsentric.codecs.std.DCodecs._

    describe("Expected property") {
      object ExpectedObject extends Contract {
        val property    = new \\ {
          val expected = \[String]
          val maybe    = \?[Int]
        }
        val notExpected = new \\ {
          val maybe  = \?[String]
          val maybe2 = \?[Int]
        }
      }
      it("Should return empty if empty and delta empty") {
        val base  = DObject.empty
        val delta = Delta.empty
        ExpectedObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if object empty and delta object empty") {
        val base  = DObject("property" := DObject.empty)
        val delta = Delta("property" := DObject.empty)
        ExpectedObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if empty and valid property delta") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := 123))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe delta
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if property is empty and valid property delta") {
        val base  = DObject("property" := DObject.empty)
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := 123))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe delta
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure if empty and delta is a remove delta") {
        val base  = DObject("property" := DObject.empty)
        val delta = Delta("property" := DNull)
        ExpectedObject.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(ExpectedObject, Path("property"))
        )
        ExpectedObject.$reduceDelta(base, delta, true).left.value should contain(
          ExpectedFailure(ExpectedObject, Path("property"))
        )
      }
      it("Should return empty if empty and has expected properties and delta is a remove delta") {
        val base  = DObject.empty
        val delta = Delta("property" := DNull)
        ExpectedObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if empty and has no expected properties and delta is a remove delta") {
        val base  = DObject("notExpected" ::= ("maybe" := 1234))
        val delta = Delta("notExpected" ::= ("maybe" -> DNull))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe delta
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if current fails to have expected properties and delta is a reducing delta") {
        val base  = DObject("property" ::= ("maybe" := 1234))
        val delta = Delta("property" ::= ("maybe" -> DNull))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe delta
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if empty but has expected properties but delta is reducing") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" -> DNull))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current and delta are the same") {
        val base  = DObject("property" ::= ("expected" := "value"), "notExpected" ::= ("maybe2" := 123))
        val delta = Delta("property" ::= ("expected" := "value"), "notExpected" ::= ("maybe2" := 123))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if delta is successful delta") {
        val base  = DObject("property" ::= ("expected" := "value"))
        val delta = Delta("property" ::= ("expected" := "value2", "maybe" := 141))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe delta
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return reduced delta if delta is successful delta but contains matching properties") {
        val base  = DObject("property" ::= ("expected" := "value"))
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := 141, "additional" := DNull))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe Delta("property" ::= ("maybe" := 141))
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe Delta("property" ::= ("maybe" := 141))
      }
      it("Should return delta if current object has bad values but delta updates without touching those values") {
        val base  = DObject("property" ::= ("expected" := 1451))
        val delta = Delta("property" ::= ("maybe" := 141))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe delta
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if current object has bad values but delta updates fixing those values") {
        val base  = DObject("property" ::= ("expected" := 1451))
        val delta = Delta("property" ::= ("expected" := "fixed"))
        ExpectedObject.$reduceDelta(base, delta).value shouldBe delta
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure  if current object has bad values and delta matches the current object") {
        val base  = DObject("property" ::= ("expected" := 1451))
        val delta = Delta("property" ::= ("expected" := 1451))
        ExpectedObject.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ExpectedObject, Path("property", "expected"), stringCodec, 1451)
        )
      }
      it("Should return failure if current empty and properties have incorrect expected types") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := 1451))
        ExpectedObject.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ExpectedObject, Path("property", "expected"), stringCodec, 1451)
        )
      }
      it("Should return failure if current empty and properties have incorrect expected types with DropBadTypes = true (unusual case, contradicts Contract Codec approach)") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := 1451))
        ExpectedObject.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(ExpectedObject, Path("property", "expected"), stringCodec, 1451)
        )
        ExpectedObject.$reduceDelta(base, delta, true).left.value should contain(
          IncorrectTypeFailure(ExpectedObject, Path("property", "expected"), stringCodec, 1451)
        )
      }
      it("Should return reduced delta if current empty and properties have incorrect maybe types with DropBadTypes = true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := false))
        ExpectedObject.$reduceDelta(base, delta, true).value shouldBe Delta("property" ::= ("expected" := "value"))
      }
    }
    describe("Maybe property") {
      object MaybeObject extends Contract {
        val property    = new \\? {
          val expected = \[String]
          val maybe    = \?[Int]
        }
        val notExpected = new \\? {
          val maybe  = \?[String]
          val maybe2 = \?[Int]
        }
      }
      it("Should return empty if empty and delta empty") {
        val base  = DObject.empty
        val delta = Delta.empty
        MaybeObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if object empty and delta object empty") {
        val base  = DObject("property" := DObject.empty)
        val delta = Delta("property" := DObject.empty)
        MaybeObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if empty and valid property delta") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := 123))
        MaybeObject.$reduceDelta(base, delta).value shouldBe delta
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if property is empty and valid property delta") {
        val base  = DObject("property" := DObject.empty)
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := 123))
        MaybeObject.$reduceDelta(base, delta).value shouldBe delta
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if empty and delta is a remove delta") {
        val base  = DObject("property" := DObject.empty)
        val delta = Delta("property" := DNull)
        MaybeObject.$reduceDelta(base, delta).value shouldBe delta
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if empty and has expected properties and delta is a remove delta") {
        val base  = DObject.empty
        val delta = Delta("property" := DNull)
        MaybeObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if empty and has no expected properties and delta is a remove delta") {
        val base  = DObject("notExpected" ::= ("maybe" := 1234))
        val delta = Delta("notExpected" ::= ("maybe" -> DNull))
        MaybeObject.$reduceDelta(base, delta).value shouldBe delta
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if current fails to have expected properties and delta is a reducing delta") {
        val base  = DObject("property" ::= ("maybe" := 1234))
        val delta = Delta("property" ::= ("maybe" -> DNull))
        MaybeObject.$reduceDelta(base, delta).value shouldBe delta
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if empty but has expected properties but delta is reducing") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" -> DNull))
        MaybeObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if current and delta are the same") {
        val base  = DObject("property" ::= ("expected" := "value"), "notExpected" ::= ("maybe2" := 123))
        val delta = Delta("property" ::= ("expected" := "value"), "notExpected" ::= ("maybe2" := 123))
        MaybeObject.$reduceDelta(base, delta).value shouldBe Delta.empty
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if delta is successful delta") {
        val base  = DObject("property" ::= ("expected" := "value"))
        val delta = Delta("property" ::= ("expected" := "value2", "maybe" := 141))
        MaybeObject.$reduceDelta(base, delta).value shouldBe delta
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return reduced delta if delta is successful delta but contains matching properties") {
        val base  = DObject("property" ::= ("expected" := "value"))
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := 141, "additional" := DNull))
        MaybeObject.$reduceDelta(base, delta).value shouldBe Delta("property" ::= ("maybe" := 141))
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe Delta("property" ::= ("maybe" := 141))
      }
      it("Should return delta if current object has bad values but delta updates without touching those values") {
        val base  = DObject("property" ::= ("expected" := 1451))
        val delta = Delta("property" ::= ("maybe" := 141))
        MaybeObject.$reduceDelta(base, delta).value shouldBe delta
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if current object has bad values but delta updates fixing those values") {
        val base  = DObject("property" ::= ("expected" := 1451))
        val delta = Delta("property" ::= ("expected" := "fixed"))
        MaybeObject.$reduceDelta(base, delta).value shouldBe delta
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return failure  if current object has bad values and delta matches the current object") {
        val base  = DObject("property" ::= ("expected" := 1451))
        val delta = Delta("property" ::= ("expected" := 1451))
        MaybeObject.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(MaybeObject, Path("property", "expected"), stringCodec, 1451)
        )
      }
      it("Should return failure if current empty and properties have incorrect expected types") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := 1451))
        MaybeObject.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(MaybeObject, Path("property", "expected"), stringCodec, 1451)
        )
      }
      it("Should return failure if current empty and properties have incorrect expected types with DropBadTypes = true (unusual case, contradicts Contract Codec approach)") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := 1451))
        MaybeObject.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(MaybeObject, Path("property", "expected"), stringCodec, 1451)
        )
        MaybeObject.$reduceDelta(base, delta, true).left.value should contain(
          IncorrectTypeFailure(MaybeObject, Path("property", "expected"), stringCodec, 1451)
        )
      }
      it("Should return reduced delta if current empty and properties have incorrect maybe types with DropBadTypes = true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("expected" := "value", "maybe" := false))
        MaybeObject.$reduceDelta(base, delta, true).value shouldBe Delta("property" ::= ("expected" := "value"))
      }
    }
  }

  describe("Additional Property behaviour") {
    import dsentric.codecs.std.DCodecs._
    describe("Closed object") {
      object Closed extends Contract {
        val property = new \\? {
          val maybe = \?[Int]
        }
      }
      it("Should fail if delta contains additional properties on an empty object") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("additional" := "value"))
        Closed.$reduceDelta(base, delta).left.value should contain(
          ClosedContractFailure(Closed, Path("property"), "additional")
        )
      }
      it("Should return empty if delta on an empty object contains additional properties and DropBadType is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("additional" := "value"))
        Closed.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if delta removes an additional property that doesnt exist on an empty object") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("additional" := DNull))
        Closed.$reduceDelta(base, delta).value shouldBe Delta.empty
        Closed.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should fail if delta contains additional properties on an existing object") {
        val base  = DObject("property" ::= ("maybe" := 123))
        val delta = Delta("property" ::= ("additional" := "value"))
        Closed.$reduceDelta(base, delta).left.value should contain(
          ClosedContractFailure(Closed, Path("property"), "additional")
        )
      }
      it("Should return empty if delta contains additional properties on an existing object with DropBadType = true") {
        val base  = DObject("property" ::= ("maybe" := 123))
        val delta = Delta("property" ::= ("additional" := "value"))
        Closed.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return delta if delta removes additional properties from current") {
        val base  = DObject("property" ::= ("additional" := "value"))
        val delta = Delta("property" ::= ("additional" := DNull))
        Closed.$reduceDelta(base, delta).value shouldBe delta
        Closed.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if delta removes additional properties which doesnt exist") {
        val base  = DObject("property" ::= ("maybe" := 123))
        val delta = Delta("property" ::= ("additional" := DNull))
        Closed.$reduceDelta(base, delta).value shouldBe Delta.empty
        Closed.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
    }
    describe("Open object") {
      object Nested     extends Contract {
        val expected = \[String]
        val maybe    = \?[Boolean]
      }
      object Additional extends Contract {
        val property      = new \\\?[Length4String, Long]()() {
          val maybe = \?[String]
        }
        val codecProperty = new \\\?[String, DObject](DContractCodec(Nested))() {
          val maybe = \?[Int]
        }
      }
      it("Should return fail if additional properties have invalid key on empty object") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("failed" := 1234))
        Additional.$reduceDelta(base, delta).left.value should contain(
          IncorrectKeyTypeFailure(Additional, Path("property"), Length4String.fixedLength4StringCodec, "failed")
        )
      }
      it("Should return empty if additional properties have invalid key on empty object with DropBadType is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("failed" := 1234))
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return fail if additional properties have invalid key") {
        val base  = DObject("property" ::= ("maybe" := "value"))
        val delta = Delta("property" ::= ("failed" := 1234))
        Additional.$reduceDelta(base, delta).left.value should contain(
          IncorrectKeyTypeFailure(Additional, Path("property"), Length4String.fixedLength4StringCodec, "failed")
        )
      }
      it("Should return empty if additional properties have invalid key with DropBadType is true") {
        val base  = DObject("property" ::= ("maybe" := "value"))
        val delta = Delta("property" ::= ("failed" := 1234))
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return fail if additional properties have invalid value on empty object") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("add1" := "failed"))
        Additional.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(Additional, Path("property", "add1"), longCodec, "failed")
        )
      }
      it("Should return empty if additional properties have invalid value on empty object with DropBadType is true") {
        val base  = DObject.empty
        val delta = Delta("property" ::= ("add1" := "failed"))
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return fail if additional properties have invalid value") {
        val base  = DObject("property" ::= ("maybe" := "value"))
        val delta = Delta("property" ::= ("add1" := "failed"))
        Additional.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(Additional, Path("property", "add1"), longCodec, "failed")
        )
      }
      it("Should return empty if additional properties have invalid value with DropBadType is true") {
        val base  = DObject("property" ::= ("maybe" := 3))
        val delta = Delta("property" ::= ("add1" := "failed"))
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }

      it("Should return empty if removing from empty, even if key is invalid") {
        val base   = DObject.empty
        val delta  = Delta("property" ::= ("add1" := DNull))
        Additional.$reduceDelta(base, delta).value shouldBe Delta.empty
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
        val delta2 = Delta("property" ::= ("invalid" := DNull))
        Additional.$reduceDelta(base, delta2).value shouldBe Delta.empty
        Additional.$reduceDelta(base, delta2, true).value shouldBe Delta.empty
      }
      it("Should return empty if removing a non existing key even if key is invalid") {
        val base   = DObject("property" ::= ("maybe" := "value"))
        val delta  = Delta("property" ::= ("add1" := DNull))
        Additional.$reduceDelta(base, delta).value shouldBe Delta.empty
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
        val delta2 = Delta("property" ::= ("invalid" := DNull))
        Additional.$reduceDelta(base, delta2).value shouldBe Delta.empty
        Additional.$reduceDelta(base, delta2, true).value shouldBe Delta.empty
      }
      it("Should return delta if removing an existing key") {
        val base  = DObject("property" ::= ("maybe" := "value", "add1" := 1412L))
        val delta = Delta("property" ::= ("add1" := DNull))
        Additional.$reduceDelta(base, delta).value shouldBe delta
        Additional.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if removing an existing key even if key is invalid") {
        val base  = DObject("property" ::= ("maybe" := "value", "invalid" := 1412L))
        val delta = Delta("property" ::= ("invalid" := DNull))
        Additional.$reduceDelta(base, delta).value shouldBe delta
        Additional.$reduceDelta(base, delta, true).value shouldBe delta
      }

      it("Should return delta if adding a correct key pair") {
        val base  = DObject("property" ::= ("maybe" := "value"))
        val delta = Delta("property" ::= ("add1" := 1234))
        Additional.$reduceDelta(base, delta).value shouldBe delta
        Additional.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return delta if replacing an existing key pair") {
        val base  = DObject("property" ::= ("maybe" := "value", "add1" := 54334))
        val delta = Delta("property" ::= ("add1" := 1234))
        Additional.$reduceDelta(base, delta).value shouldBe delta
        Additional.$reduceDelta(base, delta, true).value shouldBe delta
      }
      it("Should return empty if same key pair already exists") {
        val base  = DObject("property" ::= ("maybe" := "value", "add1" := 1234))
        val delta = Delta("property" ::= ("add1" := 1234))
        Additional.$reduceDelta(base, delta).value shouldBe Delta.empty
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return fail if trying to set defined property with additional property type") {
        val base  = DObject("property" ::= ("maybe" := "value", "add1" := 1234))
        val delta = Delta("property" ::= ("maybe" := 1234))
        Additional.$reduceDelta(base, delta).left.value should contain(
          IncorrectTypeFailure(Additional, Path("property", "maybe"), stringCodec, 1234)
        )
      }
      it("Should return fail if setting same invalid additional property key value which already exists") {
        val base   = DObject("property" ::= ("maybe" := "value", "invalid" := 1234, "add1" := "failed"))
        val delta  = Delta("property" ::= ("invalid" := 1234))
        Additional.$reduceDelta(base, delta).left.value should contain(
          IncorrectKeyTypeFailure(Additional, Path("property"), Length4String.fixedLength4StringCodec, "invalid")
        )
        val delta2 = Delta("property" ::= ("add1" := "failed"))
        Additional.$reduceDelta(base, delta2).left.value should contain(
          IncorrectTypeFailure(Additional, Path("property", "add1"), longCodec, "failed")
        )
      }

      it("Should fail if nested contract fails to validate") {
        val base   = DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value", "maybe" := true)))
        val delta  = Delta("codecProperty" ::= ("obj1" ::= ("expected" := DNull)))
        Additional.$reduceDelta(base, delta).left.value should contain(
          ExpectedFailure(Additional, Path("codecProperty", "obj1", "expected"))
        )
        val delta2 = Delta("codecProperty" ::= ("obj1" ::= ("maybe" := "fail")))
        Additional.$reduceDelta(base, delta2).left.value should contain(
          IncorrectTypeFailure(Additional, Path("codecProperty", "obj1", "maybe"), Nested.maybe._codec, "fail")
        )
      }
      it("Should return empty if nested contract expected fails under drop under bad type ") {
        val base  = DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("codecProperty" ::= ("obj1" ::= ("expected" := DNull)))
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if nested contract type fails under drop under bad type ") {
        val base  = DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value", "maybe" := true)))
        val delta = Delta("codecProperty" ::= ("obj1" ::= ("maybe" := 1234)))
        Additional.$reduceDelta(base, delta, true).value shouldBe Delta.empty
      }
      it("Should return empty if removing expected content from empty nested contract (Unusual case)") {
        val base  = DObject.empty
        val delta = Delta("codecProperty" ::= ("key1" ::= ("expected" := DNull)))
        Additional.$reduceDelta(base, delta).value shouldBe Delta.empty
        val base2 = DObject("codecProperty" ::= ("key1" := DObject.empty))
        Additional.$reduceDelta(base2, delta).value shouldBe Delta.empty
      }
    }
  }
}
