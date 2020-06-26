package dsentric.contracts

import dsentric.failure.ClosedContractFailure
import dsentric.{DObject, Dsentric, PathEnd, PessimisticCodecs}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

@deprecated("Update when EitherValues is updated and undeprecate.", "")
class ContractLensSpec extends AnyFunSpec with Matchers with EitherValues {

  import Dsentric._
  import PessimisticCodecs._
  import dsentric.Implicits._

  object ClosedContract extends Contract {
    val expected = \[String]
    val maybe = \?[Int]
  }
  object OpenContract extends Contract with AdditionalProperties {
    val expected = \[String]
    val maybe = \?[Int]
  }

  describe("$verify") {
    describe("Closed for additional properties") {
      it("Should return empty list if no additional properties") {
        val base = DObject("expected" := "value")
        ClosedContract.$verify(base) shouldBe empty
      }
      it("Should return ClosedContractFailure if additional properties") {
        val base = DObject("expected" := "value", "additional" := 1)
        ClosedContract.$verify(base) should contain (ClosedContractFailure(ClosedContract, PathEnd, "additional"))
      }
    }
    describe("Additional properties") {
      it("Should return empty list if additional properties") {
        val base = DObject("expected" := "value", "additional" := 1)
        OpenContract.$verify(base) shouldBe empty
      }
    }
  }
  describe("$get") {
    describe("Closed for additional properties") {
      it("Should return object if no additional properties") {
        val base = DObject("expected" := "value")
        ClosedContract.$get(base).right.value shouldBe base
      }
      it("Should fail with ClosedContractFailure if additional properties") {
        val base = DObject("expected" := "value", "additional" := 1)
        ClosedContract.$get(base).left.value should contain(ClosedContractFailure(ClosedContract, PathEnd, "additional"))
      }
    }
    describe("Additional properties") {
      it("Should return object if additional properties") {
        val base = DObject("expected" := "value", "additional" := 1)
        OpenContract.$get(base).right.value shouldBe base
      }
    }
  }

}
