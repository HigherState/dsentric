package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class DArrayTests extends FunSuite with Matchers {
  import PessimisticCodecs._
  implicit def strictness: Strictness = MaybePessimistic

  test("Intersects is false for empty DArrays") {
    val d1 = DArray(Vector[Any]():_*)
    val d2 = DArray(Vector[Any]():_*)

    d1.intersects(d2) shouldBe false
  }
  test("Intersects is false for disjunct non-empty DArrays") {
    val d1 = DArray(Vector[Any](1,2,3,4):_*)
    val d2 = DArray(Vector[Any](5,6,7,8):_*)

    d1.intersects(d2) shouldBe false
  }
  test("Intersects is true for DArrays with a common element") {
    val d1 = DArray(Vector[Any](1,2,3,4):_*)
    val d2 = DArray(Vector[Any](5,6,7,4):_*)

    d1.intersects(d2) shouldBe true
  }
}
