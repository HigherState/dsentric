package dsentric.contracts

import dsentric.failure.ContractFieldFailure
import dsentric.{DObject, Dsentric, Path}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AdditionalPropertiesLensSpec extends AnyFunSpec with Matchers with EitherValues {

  import Dsentric._
  import dsentric.codecs.std.DCodecs._
  import dsentric.Implicits._

  object Simple extends Contract with Open {
    val default = \![Int](999)
  }

  object SimpleAdditionalProperties extends Contract with Open

  object TypedAdditionalProperties extends DefinedAdditionalProperties[Length4String, Boolean] with Contract

  object ContractAdditionalProperties extends DefinedAdditionalProperties[Length4String, DObject](Simple) with Contract

  object NestedAdditionalProperties extends Contract with Open {
    val maybeAdditional = new \\? with Open

    val expectedAdditional = new \\ with Open

    val expected = \[Long]
  }


  describe("$put") {
    it("Should fail if key is defined in the contract") {
      NestedAdditionalProperties.$put("maybeAdditional" -> DObject("test" := "test"))(DObject.empty)
        .left.value should contain (ContractFieldFailure(NestedAdditionalProperties, Path.empty, "maybeAdditional"))
    }
    it("Should add if key doesnt exist") {
      SimpleAdditionalProperties.$put("newOne" := 1234)(DObject.empty).value shouldBe DObject("newOne" := 1234)
    }
    it("Should add if key doesnt exist and unique key value type") {
      TypedAdditionalProperties.$put(Length4String("size") -> false)(DObject.empty).value shouldBe DObject("size" := false)
    }
    it("Should replace if key exists") {
      SimpleAdditionalProperties.$put("add" := "Bob")(DObject("add" := 56.5)).value shouldBe DObject("add" := "Bob")
    }
    it("Should create object if nested and doesn't exist") {
      NestedAdditionalProperties.maybeAdditional.$put("nested" := "value")(DObject.empty)
        .value shouldBe DObject("maybeAdditional" ::= ("nested" := "value"))
    }
    it("Should create object if nested and doesnt exist and has required properties") {
      NestedAdditionalProperties.$put("notRequired" := true)(DObject.empty).value shouldBe DObject("notRequired" := true)
    }
    it("Should create object if nested and object originally of wrong type") {
      NestedAdditionalProperties.expectedAdditional.$put("nested" := "value")(DObject("expectedAdditional" := false))
        .value shouldBe DObject("expectedAdditional" ::= ("nested" := "value"))
    }
    it("Should create key pair even if Value does satisfy Contract") {
      ContractAdditionalProperties.$put(Length4String("four") -> DObject("default" := false))(DObject.empty)
        .value shouldBe DObject("four" ::= ("default" := false))
    }
    it("Should add into an existing nested object") {
      NestedAdditionalProperties.expectedAdditional.$put("nested" := "value")(DObject("expectedAdditional" ::= ("current" := 1234)))
        .value shouldBe DObject("expectedAdditional" ::= ("current" := 1234, "nested" := "value"))
    }
  }

  describe("$putMany") {
    it("Should fail if a key is defined in the contract") {
      NestedAdditionalProperties.$putMany(Seq("maybeAdditional" -> DObject("test" := "test")))(DObject.empty)
        .left.value should contain only (ContractFieldFailure(NestedAdditionalProperties, Path.empty, "maybeAdditional"))
    }
    it("Should fail if multiple keys are defined in the contract") {
      val props =
        Seq(
          "maybeAdditional" -> DObject("test" := "test"),
          "isNew" -> DObject("test" := "test"),
          "expectedAdditional" -> DObject("test" := "test")
        )
      NestedAdditionalProperties.$putMany(props)(DObject.empty)
        .left.value should contain allOf (ContractFieldFailure(NestedAdditionalProperties, Path.empty, "maybeAdditional"), ContractFieldFailure(NestedAdditionalProperties, Path.empty, "expectedAdditional"))
    }
    it("Should put many if keys dont exist") {
      val props =
        Seq(
          "newOne" := 1234,
          "anotherOne" := false
        )
      SimpleAdditionalProperties.$putMany(props)(DObject.empty).value shouldBe DObject(props:_*)
    }
    it("Should replace any existing keys") {
      val props =
        Seq(
          "newOne" := 1234,
          "anotherOne" := false,
          "replacedOne" := "bob"
        )
      SimpleAdditionalProperties.$putMany(props)(DObject("replacedOne" := "Mary")).value shouldBe DObject(props:_*)
    }
    it("Should create object if nested and doesn't exists") {
      val props =
        Seq(
          "newOne" := 1234,
          "anotherOne" := false
        )
      NestedAdditionalProperties.maybeAdditional.$putMany(props)(DObject.empty).value shouldBe DObject("maybeAdditional".::=(props*))
    }
    it("Should add into an existing nested object") {
      val props =
        Seq(
          "newOne" := 1234,
          "anotherOne" := false
        )
      NestedAdditionalProperties.expectedAdditional.$putMany(props)(DObject("expectedAdditional" ::= ("current" := 1234)))
        .value shouldBe DObject("expectedAdditional" ::= ("current" := 1234, "newOne" := 1234, "anotherOne" := false))
    }
  }

  describe("$drop") {
    it("Should fail if key in contract") {
      NestedAdditionalProperties.$drop("maybeAdditional")(DObject.empty)
        .left.value should contain (ContractFieldFailure(NestedAdditionalProperties, Path.empty, "maybeAdditional"))
    }
    it("Should do nothing if empty") {
      SimpleAdditionalProperties.$drop("key")(DObject.empty).value shouldBe DObject.empty
    }
    it("Should drop if key exists") {
      SimpleAdditionalProperties.$drop(("dropMe"))(DObject("dropMe" := "dropped")).value shouldBe DObject.empty
    }
    it("Should drop only existing key") {
      val current = DObject("expected" := 1234L, "additional" := false, "dropMe" := 5678)
      NestedAdditionalProperties.$drop("dropMe")(current).value shouldBe DObject("expected" := 1234L, "additional" := false)
    }
    it("Should clear out object if dropped key last entry") {
      val current = DObject("expected" := 1234L, "maybeAdditional" ::= ("dropMe" := 123))
      NestedAdditionalProperties.maybeAdditional.$drop("dropMe")(current).value shouldBe DObject("expected" := 1234L)
    }
    it("Should do nothing if not applied to object") {
      val current = DObject("expected" := 1234L, "expectedAdditional" := false)
      NestedAdditionalProperties.expectedAdditional.$drop("dropMe")(current).value shouldBe current
    }
  }

  describe("$dropAll") {
    it("Should fo nothing not applied to object") {
        val current = DObject("expected" := 1234L, "expectedAdditional" := false)
        NestedAdditionalProperties.expectedAdditional.$dropAll(current) shouldBe current
      }
    it("Should clear out object if only contains additional keys") {
      val current = DObject("expected" := 1234L, "maybeAdditional" ::= ("dropMe" := 123, "dropMe2" := "test"))
      NestedAdditionalProperties.maybeAdditional.$dropAll(current) shouldBe DObject("expected" := 1234L)
    }
    it("Should not clear out field values") {
      Simple.$dropAll(DObject("dropMe" := "dropped", "dropMe2" := "test", "default" := 456)) shouldBe DObject("default" := 456)
    }
  }
  describe("$modify") {
    it("Should do nothing if key not found") {
      pending
    }
    it("Should fail if value is of wrong type") {
      pending
    }
    it("Should modify if value found") {
      pending
    }
  }
}
