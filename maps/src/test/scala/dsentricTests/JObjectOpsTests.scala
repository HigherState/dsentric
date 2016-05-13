package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class JObjectOpsTests extends FunSuite with Matchers {

  import PessimisticCodecs._
  implicit def strictness = MaybePessimistic
  import Dsentric._


  test("Applying single key delta object value on obj") {
    val obj = JObject("one" := 1, "two" := "two")
    JObjectOps.rightReduceConcat(obj, JObject.empty) should equal (obj)
    JObjectOps.rightReduceConcat(obj, JObject("three" := 3)) should equal (obj + ("three" := 3))
    JObjectOps.rightReduceConcat(obj, JObject("two" := "three")) should equal (JObject("one" := 1, "two" := "three"))
    JObjectOps.rightReduceConcat(obj, JObject("one" := jNull)) should equal (JObject("two" := "two"))
    JObjectOps.rightReduceConcat(obj, JObject("two" := JObject.empty)) should equal (JObject("one" := 1))
  }

  test("Applying nested delta object") {
    val obj = JObject("one" := 1, "obj" := JObject("two" := false, "three" := List(1,2,3,4), "four" := JObject("five" := 5)))
    JObjectOps.rightReduceConcat(obj, JObject("obj" := JObject.empty)) should equal (JObject("one" := 1))
    JObjectOps.rightReduceConcat(obj, JObject("obj" := jNull)) should equal (JObject("one" := 1))
    JObjectOps.rightReduceConcat(obj, JObject("obj" := JObject("two" := true))) should equal (JObject("one" := 1, "obj" := JObject("two" := true, "three" := List(1,2,3,4), "four" := JObject("five" := 5))))
    JObjectOps.rightReduceConcat(obj, JObject("obj" := JObject("three" := jNull))) should equal (JObject("one" := 1, "obj" := JObject("two" := false, "four" := JObject("five" := 5))))
    JObjectOps.rightReduceConcat(obj, JObject("obj" := JObject("two" := jNull, "three" := jNull, "four" := jNull))) should equal (JObject("one" := 1))
    JObjectOps.rightReduceConcat(obj, JObject("obj" := JObject("six" := "vi"))) should equal (JObject("one" := 1, "obj" := JObject("two" := false, "three" := List(1,2,3,4), "four" := JObject("five" := 5), "six" := "vi")))
  }

  test("Get difference") {
    val obj = JObject("one" := 1, "obj" := JObject("two" := false, "three" := List(1,2,3,4), "four" := JObject("five" := 5)))
    JObjectOps.rightDifference(obj, obj) should be (JObject.empty)
    JObjectOps.rightDifference(obj, JObject.empty) should be (JObject.empty)
    JObjectOps.rightDifference(obj, JObject("one" := 1)) should be (JObject.empty)
    JObjectOps.rightDifference(obj, JObject("one" := 1, "obj" := JObject("two" := false))) should be (JObject.empty)
    JObjectOps.rightDifference(obj, JObject("obj" := JObject("four" := JObject.empty))) should be (JObject.empty)
    JObjectOps.rightDifference(obj, JObject("obj" := JObject("four" := JObject("five" := 5)))) should be (JObject.empty)

    JObjectOps.rightDifference(obj, JObject("one" := 2)) should be (JObject("one" := 2))
    JObjectOps.rightDifference(obj, JObject("six" := 6)) should be (JObject("six" := 6))
    JObjectOps.rightDifference(obj, JObject("obj" := JObject("four" := JObject("six" := 34.56)))) should be (JObject("obj" := JObject("four" := JObject("six" := 34.56))))
    JObjectOps.rightDifference(obj, JObject("obj" := JObject("two" := true, "three" := List(1,2,3,4)))) should be (JObject("obj" := JObject("two" := true)))
    JObjectOps.rightDifference(obj, JObject("obj" := JObject("three" := List(1,2,3,4,5,6)))) should be (JObject("obj" := JObject("three" := List(1,2,3,4,5,6))))
  }
}
