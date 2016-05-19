package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class DObjectOpsTests extends FunSuite with Matchers {

  import PessimisticCodecs._
  implicit def strictness = MaybePessimistic
  import Dsentric._


  test("Applying single key delta object value on obj") {
    val obj = DObject("one" := 1, "two" := "two")
    DObjectOps.rightReduceConcat(obj, DObject.empty) should equal (obj)
    DObjectOps.rightReduceConcat(obj, DObject("three" := 3)) should equal (obj + ("three" := 3))
    DObjectOps.rightReduceConcat(obj, DObject("two" := "three")) should equal (DObject("one" := 1, "two" := "three"))
    DObjectOps.rightReduceConcat(obj, DObject("one" := dNull)) should equal (DObject("two" := "two"))
    DObjectOps.rightReduceConcat(obj, DObject("two" := DObject.empty)) should equal (DObject("one" := 1))
  }

  test("Applying nested delta object") {
    val obj = DObject("one" := 1, "obj" := DObject("two" := false, "three" := List(1,2,3,4), "four" := DObject("five" := 5)))
    DObjectOps.rightReduceConcat(obj, DObject("obj" := DObject.empty)) should equal (DObject("one" := 1))
    DObjectOps.rightReduceConcat(obj, DObject("obj" := dNull)) should equal (DObject("one" := 1))
    DObjectOps.rightReduceConcat(obj, DObject("obj" := DObject("two" := true))) should equal (DObject("one" := 1, "obj" := DObject("two" := true, "three" := List(1,2,3,4), "four" := DObject("five" := 5))))
    DObjectOps.rightReduceConcat(obj, DObject("obj" := DObject("three" := dNull))) should equal (DObject("one" := 1, "obj" := DObject("two" := false, "four" := DObject("five" := 5))))
    DObjectOps.rightReduceConcat(obj, DObject("obj" := DObject("two" := dNull, "three" := dNull, "four" := dNull))) should equal (DObject("one" := 1))
    DObjectOps.rightReduceConcat(obj, DObject("obj" := DObject("six" := "vi"))) should equal (DObject("one" := 1, "obj" := DObject("two" := false, "three" := List(1,2,3,4), "four" := DObject("five" := 5), "six" := "vi")))
  }

  test("Get difference") {
    val obj = DObject("one" := 1, "obj" := DObject("two" := false, "three" := List(1,2,3,4), "four" := DObject("five" := 5)))
    DObjectOps.rightDifference(obj, obj) should be (DObject.empty)
    DObjectOps.rightDifference(obj, DObject.empty) should be (DObject.empty)
    DObjectOps.rightDifference(obj, DObject("one" := 1)) should be (DObject.empty)
    DObjectOps.rightDifference(obj, DObject("one" := 1, "obj" := DObject("two" := false))) should be (DObject.empty)
    DObjectOps.rightDifference(obj, DObject("obj" := DObject("four" := DObject.empty))) should be (DObject.empty)
    DObjectOps.rightDifference(obj, DObject("obj" := DObject("four" := DObject("five" := 5)))) should be (DObject.empty)

    DObjectOps.rightDifference(obj, DObject("one" := 2)) should be (DObject("one" := 2))
    DObjectOps.rightDifference(obj, DObject("six" := 6)) should be (DObject("six" := 6))
    DObjectOps.rightDifference(obj, DObject("obj" := DObject("four" := DObject("six" := 34.56)))) should be (DObject("obj" := DObject("four" := DObject("six" := 34.56))))
    DObjectOps.rightDifference(obj, DObject("obj" := DObject("two" := true, "three" := List(1,2,3,4)))) should be (DObject("obj" := DObject("two" := true)))
    DObjectOps.rightDifference(obj, DObject("obj" := DObject("three" := List(1,2,3,4,5,6)))) should be (DObject("obj" := DObject("three" := List(1,2,3,4,5,6))))
  }
}
