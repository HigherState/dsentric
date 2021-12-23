package dsentric.contracts

import dsentric.DObject
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GetSpec extends AnyFunSpec with Matchers with EitherValues {
  import dsentric.Dsentric._

  describe("Nested object bug") {
    import dsentric.codecs.std.DCodecs._
    object ExpectedNested extends Contract {
      import dsentric.operators.StandardOperators._
      val name     = \?[String]
      val id       = \[Long](immutable)
      val internal = new \\ {
        val set1 = \![Set[String]](Set.empty[String])
        val set2 = \[Set[String]]
      }
    }

    it("should correctly validate with Expected Nested Object") {
      val l1 = ExpectedNested.$create { l =>
        l.name.$set("Name") ~
        l.id.$set(1234L) ~
        l.internal.set2.$set(Set("one", "two"))
      }
      ExpectedNested.$get(l1).value shouldBe (l1 +\ (ExpectedNested.internal.set1._path := Set.empty[String]))
    }

    object MaybeNested extends Contract {
      import dsentric.operators.StandardOperators._
      val name     = \?[String]
      val id       = \[Long](immutable)
      val internal = new \\? {
        val set1 = \![Set[String]](Set.empty[String])
        val set2 = \[Set[String]]
      }
    }

    it("should correctly validate with Maybe Nested Object") {
      val l1 = MaybeNested.$create { l =>
        l.name.$set("Name") ~
        l.id.$set(1234L) ~
        l.internal.set2.$set(Set("one", "two"))
      }
      MaybeNested.$get(l1).value shouldBe (l1 +\ (MaybeNested.internal.set1._path := Set.empty[String]))
    }
  }

  describe("constraint bug") {
    import dsentric.codecs.std.DCodecs._
    object SimpleConstraint extends Contract {
      import dsentric.operators.StandardOperators._
      val reserve = \?[Int](reserved)
    }

    it("Should return if reserved is set") {
      SimpleConstraint.$get(DObject("reserve" := 123)).value shouldBe DObject("reserve" := 123)
    }

    object MaybeNestedConstraint extends Contract {
      import dsentric.operators.StandardOperators._
      val nested = new \\? {
        val reserve = \?[Int](reserved)
      }
    }

    it("Should return if reserved is set in maybe object property") {
      MaybeNestedConstraint.$get(DObject("nested" ::= ("reserve" := 123))).value shouldBe DObject(
        "nested" ::= ("reserve" := 123)
      )
      MaybeNestedConstraint.nested.$get(DObject("nested" ::= ("reserve" := 123))).value should contain(
        DObject("reserve" := 123)
      )
    }
  }
}
