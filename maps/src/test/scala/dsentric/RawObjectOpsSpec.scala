package dsentric

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RawObjectOpsSpec extends AnyFunSpec with Matchers {

  describe("rightDifferenceReduceMap") {
    val single = Map("one" -> 1, "two" -> "string", "three" -> false)
    it("Should show the difference between to single level objects") {
      val delta = Map("one" -> 2, "four" -> Map("one" -> 1))
      RawObjectOps.rightDifferenceReduceMap(single -> delta) shouldBe delta
    }
    it("Should remove null differences if there isnt an existing value") {

    }
    it("Should keep null differences if there is an existing value") {

    }
    it("Should remove empty object differences if there isnt an existing value") {

    }
    it("Should remove empty object differences if there is an existing value") {

    }
  }
}
