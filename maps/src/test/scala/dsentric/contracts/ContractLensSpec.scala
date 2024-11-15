package dsentric.contracts

import dsentric.{DObject, DObjectOps, Delta, Dsentric, PathEnd}
import dsentric.failure.ClosedContractFailure

import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ContractLensSpec extends AnyFunSpec with Matchers with EitherValues {

  import Dsentric._
  import dsentric.codecs.std.DCodecs._
  import dsentric.Implicits._

  object ClosedContract extends Contract           {
    val expected = \[String]
    val maybe    = \?[Int]
  }
  object OpenContract   extends Contract with Open {
    val expected = \[String]
    val maybe    = \?[Int]
  }

  sealed trait CustomContract extends DObject with DObjectOps[CustomContract]

  trait Attributes extends SubContract {
    val bool        = \[Boolean]
    val maybeString = \?[String]
    val defaultInt  = \![Int](6)
  }

  trait Attributes2 extends SubContractFor[CustomContract] {
    val bool = \[Boolean]
  }

  object AnotherContract extends Contract {
    val attributes = new \\ with Attributes
  }

  object AnotherContract2 extends ContractFor[CustomContract] {
    val attributes = new \\ with Attributes2
  }

  sealed trait MoreAttributes[D <: CustomContract] extends SubContractFor[D] {
    def moreAttributes: SubContractFor[D] & ExpectedObjectProperty[D]
  }

  sealed trait MoreDynamicAttributes[D <: CustomContract] extends MoreAttributes[D] {
    val moreDynamicAttributes = \?[Map[String, DObject]]
  }

  trait HasMoreDynamicAttributes extends MoreDynamicAttributes[CustomContract] { this: SubTypeTraitContract => }

  sealed trait TraitContract extends ContractFor[CustomContract]

  trait SubTypeTraitContract extends TraitContract {}

  object TestFixture extends SubTypeTraitContract with HasMoreDynamicAttributes {
    val moreAttributes: \\ & Attributes2 = new \\ with Attributes2
  }

  describe("$verify") {
    describe("Closed for additional properties") {
      it("Should return empty list if no additional properties") {
        val base = DObject("expected" := "value")
        ClosedContract.$verify(base) shouldBe Symbol("right")
      }
      it("Should return ClosedContractFailure if additional properties") {
        val base = DObject("expected" := "value", "additional" := 1)
        ClosedContract.$verify(base).left.value should contain(
          ClosedContractFailure(ClosedContract, PathEnd, "additional")
        )
      }
    }
    describe("Additional properties") {
      it("Should return empty list if additional properties") {
        val base = DObject("expected" := "value", "additional" := 1)
        OpenContract.$verify(base) shouldBe Symbol("right")
        OpenContract.expected.$get(base).toOption shouldBe Some("value")
      }
    }

    describe("SubContract") {
      it("allows selectable fields") {
        val base = Delta("attributes" := Delta("bool" := true, "defaultInt" := 5))
        AnotherContract.attributes.bool.$get(base).toOption.flatten shouldBe Some(true)
      }
    }

    describe("SubContractFor") {
      it("allows selectable fields") {
        val base  = Delta("attributes" := Delta("bool" := true))
        val base2 = Delta("moreAttributes" := Delta("bool" := true))
        AnotherContract2.attributes.bool.$get(base).toOption.flatten shouldBe Some(true)
        TestFixture.moreAttributes.bool.$get(base2).toOption.flatten shouldBe Some(true)
      }
    }
  }
  //  describe("$get") {
  //    describe("Closed for additional properties") {
  //      it("Should return object if no additional properties") {
  //        val base = DObject("expected" := "value")
  //        ClosedContract.$get(base).value shouldBe base
  //      }
  //      it("Should fail with ClosedContractFailure if additional properties") {
  //        val base = DObject("expected" := "value", "additional" := 1)
  //        ClosedContract.$get(base).left.value should contain(ClosedContractFailure(ClosedContract, PathEnd, "additional"))
  //      }
  //    }
  //    describe("Additional properties") {
  //      it("Should return object if additional properties") {
  //        val base = DObject("expected" := "value", "additional" := 1)
  //        OpenContract.$get(base).value shouldBe base
  //      }
  //    }
  //  }

}