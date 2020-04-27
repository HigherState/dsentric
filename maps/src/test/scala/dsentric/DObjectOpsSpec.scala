package dsentric

import org.scalatest.{FunSpec, Matchers}


class DObjectOpsSpec extends FunSpec with Matchers {

  describe("rightDifferenceReduceMap") {
    describe("flat Objects") {
      val single = Map("one" -> 1, "two" -> "string", "three" -> Map("three-one" -> false))
      it("Should show the difference between to single level objects") {
        val delta = Map("one" -> 2, "four" -> Map("one" -> 1))
        DObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe Some(delta)
      }
      it("Should remove null differences if there isnt an existing value") {
        val delta = Map("one" -> 3, "four" -> DNull)
        DObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe Some(Map("one" -> 3))
      }
      it("Should keep null differences if there is an existing value") {
        val delta = Map("one" -> DNull, "four" -> 4)
        DObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe Some(Map("one" -> DNull, "four" -> 4))
      }
      it("Should remove empty object differences if there isnt an existing value") {
        val delta = Map("four" -> Map.empty)
        DObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe None
      }
      it("Should remove empty object differences if there is an existing map") {
        val delta = Map("three" -> Map.empty)
        DObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe None
      }
      it("Should keep empty object differences if there is an existing non-map value") {
        val delta = Map("two" -> Map.empty)
        DObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe Some(delta)
      }
    }
    describe("nested") {
      val nested = Map("one" -> 1, "two" -> Map("two-one" -> Map("two-one-one" -> 3), "two-two" -> false))
      it("Should reduce delta to empty if nested object if no changes") {
        val delta = Map("two" -> Map("two-three" -> DNull))
        DObjectOps.rightDifferenceReduceMap(nested -> delta) shouldBe None
      }
      it("Should reduce delta property to empty if nested object if no changes") {
        val delta = Map("one" -> Map("two-three" -> DNull))
        DObjectOps.rightDifferenceReduceMap(nested -> delta) shouldBe Some(Map("one" -> Map.empty))
      }
      it("Should keep nested null property if relevant") {
        val delta = Map("one" -> Map("two-one" -> DNull))
        DObjectOps.rightDifferenceReduceMap(nested -> delta) shouldBe Some(delta)
      }

    }
  }
}
