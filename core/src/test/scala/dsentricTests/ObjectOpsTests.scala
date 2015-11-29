package dsentricTests

import dsentric.ObjectOps
import org.scalatest.{Matchers, FunSuite}

class ObjectOpsTests  extends FunSuite with Matchers with ObjectOps {

  test("Applying svalue delta on obj") {
//    val obj = Json("one" := 1, "two" := "two")
//    applyDelta(obj, jString("value")) should equal(jString("value"))
//    applyDelta(obj, jNull) should equal(jNull)
  }

  //  test("Applying single key delta object value on obj") {
  //    val obj = Json("one" := 1, "two" := "two")
  //    applyDelta(obj, jEmptyObject) should equal (obj)
  //    applyDelta(obj, Json("three" := 3)) should equal (("three" := 3) ->: obj)
  //    applyDelta(obj, Json("two" := "three")) should equal (Json("one" := 1, "two" := "three"))
  //    applyDelta(obj, Json("one" -> jNull)) should equal (Json("two" := "two"))
  //    applyDelta(obj, Json("two" -> jEmptyObject)) should equal (Json("one" := 1))
  //  }
  //
  //  test("Applying nested delta object") {
  //    val obj = Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5)))
  //    applyDelta(obj, Json("obj" -> jEmptyObject)) should equal (obj)
  //    applyDelta(obj, Json("obj" -> jNull)) should equal (Json("one" := 1))
  //    applyDelta(obj, Json("obj" -> Json("two" := true))) should equal (Json("one" := 1, "obj" -> Json("two" := true, "three" := List(1,2,3,4), "four" -> Json("five" := 5))))
  //    applyDelta(obj, Json("obj" -> Json("three" -> jNull))) should equal (Json("one" := 1, "obj" -> Json("two" := false, "four" -> Json("five" := 5))))
  //    applyDelta(obj, Json("obj" -> Json("two" -> jNull, "three" -> jNull, "four" -> jNull))) should equal (Json("one" := 1))
  //    applyDelta(obj, Json("obj" -> Json("six" := "vi"))) should equal (Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5), "six" := "vi")))
  //  }
}