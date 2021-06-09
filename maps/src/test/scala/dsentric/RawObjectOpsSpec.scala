package dsentric

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


class RawObjectOpsSpec extends AnyFunSpec with Matchers {

  describe("rightDifferenceReduceMap") {
    describe("flat Objects") {
      val single = Map("one" -> 1, "two" -> "string", "three" -> Map("three-one" -> false))
      it("Should show the difference between to single level objects") {
        val delta = Map("one" -> 2, "four" -> Map("one" -> 1))
        RawObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe Some(delta)
      }
      it("Should remove null differences if there isnt an existing value") {
        val delta = Map("one" -> 3, "four" -> DNull)
        RawObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe Some(Map("one" -> 3))
      }
      it("Should keep null differences if there is an existing value") {
        val delta = Map("one" -> DNull, "four" -> 4)
        RawObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe Some(Map("one" -> DNull, "four" -> 4))
      }
      it("Should remove empty object differences if there isnt an existing value") {
        val delta = Map("four" -> Map.empty)
        RawObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe None
      }
      it("Should remove empty object differences if there is an existing map") {
        val delta = Map("three" -> Map.empty)
        RawObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe None
      }
      it("Should keep empty object differences if there is an existing non-map value") {
        val delta = Map("two" -> Map.empty)
        RawObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe Some(delta)
      }
    }
    describe("nested") {
      val nested = Map("one" -> 1, "two" -> Map("two-one" -> Map("two-one-one" -> 3), "two-two" -> false))
      it("Should reduce delta to empty if nested object if no changes") {
        val delta = Map("two" -> Map("two-three" -> DNull))
        RawObjectOps.rightDifferenceReduceMap(nested -> delta) shouldBe None
      }
      it("Should reduce delta property to empty if nested object if no changes") {
        val delta = Map("one" -> Map("two-three" -> DNull))
        RawObjectOps.rightDifferenceReduceMap(nested -> delta) shouldBe Some(Map("one" -> Map.empty))
      }
      it("Should keep nested null property if relevant") {
        val delta = Map("two" -> Map("two-one" -> DNull))
        RawObjectOps.rightDifferenceReduceMap(nested -> delta) shouldBe Some(delta)
      }

    }
  }

  describe("rightReduceConcatMap") {

    describe("flat") {
      val single = Map("one" -> 1, "two" -> "string", "three" -> Map("three-one" -> false))
      it("Should replace present values") {
        val delta = Map("one" -> 2, "four" -> Map("one" -> 1))
        RawObjectOps.rightReduceConcatMap(single, delta) shouldBe
          Map("one" -> 2, "two" -> "string", "three" -> Map("three-one" -> false), "four" -> Map("one" -> 1))
      }
      it("Should remove a value if replace value is null") {
        val delta = Map("one" -> DNull)
        RawObjectOps.rightReduceConcatMap(single, delta) shouldBe
          Map("two" -> "string", "three" -> Map("three-one" -> false))
      }
      it("Should ignore null if it doesnt replace a value") {
        val delta = Map("four" -> DNull)
        RawObjectOps.rightReduceConcatMap(single, delta) shouldBe single
      }
      it("Should remove a value if replace value is an empty object and value not an object") {
        val delta = Map("two" -> Map.empty)
        RawObjectOps.rightReduceConcatMap(single, delta) shouldBe
          Map("one" -> 1, "three" -> Map("three-one" -> false))
      }
      it("Should do nothing if replace value is an empty object and value is an empty object") {
        val delta = Map("three" -> Map.empty)
        RawObjectOps.rightReduceConcatMap(single, delta) shouldBe single
      }
      it("Should ignore empty object if it doesn't replace a value") {
        val delta = Map("four" -> Map.empty)
        RawObjectOps.rightReduceConcatMap(single, delta) shouldBe single
      }
    }
    describe("nested")  {
      val nested = Map("one" -> 1, "two" -> Map("two-one" -> Map("two-one-one" -> 3), "two-two" -> false))
      it("Should replace a nested value") {
        val delta = Map("two" -> Map("two-two" -> true ))
        RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe
          Map("one" -> 1, "two" -> Map("two-one" -> Map("two-one-one" -> 3), "two-two" -> true))
      }
      it("Should create a nested value") {
        val delta = Map("two" -> Map("two-three" -> "bob"))
        RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe
          Map("one" -> 1, "two" -> Map("two-one" -> Map("two-one-one" -> 3), "two-two" -> false, "two-three" -> "bob"))
      }
      it("Should remove a nested value") {
        val delta = Map("two" -> Map("two-one" -> DNull))
        RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe
          Map("one" -> 1, "two" -> Map("two-two" -> false))
      }
      it("should remove object if all values removed") {
        val delta = Map("two" -> Map("two-one" -> DNull, "two-two" -> Map.empty))
        RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe Map("one" -> 1)

      }
      it("Should ignore nested null if match not found") {
        val delta = Map("two" -> Map("two-three" -> DNull))
        RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe nested
      }
      it("should ignore nested empty object if match not found") {
        val delta = Map("two" -> Map("two-three" -> Map.empty))
        RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe nested
      }
      describe("New nested object") {
        it("Should create a whole nested object if not matched") {
          val delta = Map("two" -> Map("two-four" -> Map("two-four-one" -> 1, "two-four-two" -> Map("two-four-two-one" -> false))))
          RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe
            Map("one" -> 1, "two" -> Map("two-one" -> Map("two-one-one" -> 3), "two-two" -> false,
              "two-four" -> Map("two-four-one" -> 1, "two-four-two" -> Map("two-four-two-one" -> false))))

        }
        it("Should not create a nested object if only empty of null") {
          val delta = Map("two" -> Map("two-four" -> Map("two-four-one" -> DNull, "two-four-two" -> Map.empty)))
          RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe nested
        }
        it("Should remove any empty of null values if nested object created") {
          val delta = Map("two" -> Map("two-four" -> Map("two-four-one" -> DNull,
            "two-four-two" -> Map("two-four-two-one" -> false, "tow-four-two-two" -> Map.empty))))
          RawObjectOps.rightReduceConcatMap(nested, delta) shouldBe
            Map("one" -> 1, "two" -> Map("two-one" -> Map("two-one-one" -> 3), "two-two" -> false,
              "two-four" -> Map("two-four-two" -> Map("two-four-two-one" -> false))))
        }
      }
    }
  }
}
