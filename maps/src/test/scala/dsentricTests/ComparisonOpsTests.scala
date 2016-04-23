package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ComparisonOpsTests extends FunSuite with Matchers {

  import PessimisticCodecs._
  implicit def strictness = MaybePessimistic
  import Dsentric._

  import ComparisonOps._


  test("Applying single key delta object value on obj") {
    val obj = JObject("one" := 1, "two" := "two")
    obj.applyDelta(JObject.empty) should equal (obj)
    obj.applyDelta(JObject("three" := 3)) should equal (obj + ("three" := 3))
    obj.applyDelta(JObject("two" := "three")) should equal (JObject("one" := 1, "two" := "three"))
    obj.applyDelta(JObject("one".:=(JNull)(jNullCodec))) should equal (JObject("two" := "two"))
    obj.applyDelta(JObject("two" := JObject.empty)) should equal (JObject("one" := 1))
  }

//  test("Applying nested delta object") {
//    val obj = JObject("one" := 1, "obj" := JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5)))
//    applyDelta(obj, JObject("obj" -> jEmptyObject)) should equal (obj)
//    applyDelta(obj, JObject("obj" -> jNull)) should equal (JObject("one" := 1))
//    applyDelta(obj, JObject("obj" -> JObject("two" := true))) should equal (JObject("one" := 1, "obj" -> JObject("two" := true, "three" := List(1,2,3,4), "four" -> JObject("five" := 5))))
//    applyDelta(obj, JObject("obj" -> JObject("three" -> jNull))) should equal (JObject("one" := 1, "obj" -> JObject("two" := false, "four" -> JObject("five" := 5))))
//    applyDelta(obj, JObject("obj" -> JObject("two" -> jNull, "three" -> jNull, "four" -> jNull))) should equal (JObject("one" := 1))
//    applyDelta(obj, JObject("obj" -> JObject("six" := "vi"))) should equal (JObject("one" := 1, "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5), "six" := "vi")))
//  }
//
//  test("Get value") {
//    val obj = JObject("one" := 1, "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5)))
//    getValue(obj, Vector.empty) should be (Some(obj))
//    getValue(obj, Path("two").segments) should be (None)
//    getValue(obj, Path("one").segments) should be (Some(jNumber(1)))
//    getValue(obj, Path("obj", "two").segments) should be (Some(jFalse))
//    getValue(obj, Path("obj", "one").segments) should be (None)
//    getValue(obj, Path("obj", "four").segments) should be (Some(JObject("five" := 5)))
//    getValue(obj, Path("obj", "four", "five").segments) should be (Some(jNumber(5)))
//    getValue(obj, Path("obj", "three", 3).segments) should be (Some(jNumber(4)))
//    getValue(obj, Path("obj", "three", 4).segments) should be (None)
//  }
//
//  test("Set value") {
//    val obj = JObject("one" := 1, "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5)))
//    setValue(Some(obj), Vector.empty, jString("replace")) should be (jString("replace"))
//    setValue(Some(obj), Path("two").segments, jString("set")) should be (JObject("one" := 1, "two" := "set", "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5))))
//    setValue(Some(obj), Path("one").segments, jString("replace")) should be (JObject("one" := "replace", "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5))))
//    setValue(Some(obj), Path("obj", "two").segments, jString("replace")) should be (JObject("one" := 1, "obj" -> JObject("two" := "replace", "three" := List(1,2,3,4), "four" -> JObject("five" := 5))))
//    setValue(Some(obj), Path("obj", "four", "five").segments, jString("replace")) should be (JObject("one" := 1, "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := "replace"))))
//    setValue(Some(obj), Path("obj", "three", 3).segments, jNumberOrString(5)) should be (JObject("one" := 1, "obj" -> JObject("two" := false, "three" := List(1,2,3,5), "four" -> JObject("five" := 5))))
//    setValue(Some(obj), Path("obj", "three", 5).segments, jNumberOrString(5)) should be (JObject("one" := 1, "obj" -> JObject("two" := false, "three" := List(jNumberOrString(1),jNumberOrString(2),jNumberOrString(3),jNumberOrString(4),jNull,jNumberOrString(5)), "four" -> JObject("five" := 5))))
//    setValue(Some(obj), Path("two", "three").segments, jString("set")) should be (JObject("one" := 1, "two" -> JObject("three" := "set"), "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5))))
//    setValue(Some(obj), Path("two", 2).segments, jTrue)  should be (JObject("one" := 1, "two" := List(jNull, jNull, jTrue), "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5))))
//  }
//
//  test("Get difference") {
//    val obj = JObject("one" := 1, "obj" -> JObject("two" := false, "three" := List(1,2,3,4), "four" -> JObject("five" := 5)))
//    difference(obj, obj) should be (None)
//    difference(jEmptyObject, obj) should be (None)
//    difference(JObject("one" := 1), obj) should be (None)
//    difference(JObject("one" := 1, "obj" -> JObject("two" := false)), obj) should be (None)
//    difference(JObject("obj" -> JObject("four" -> jEmptyObject)), obj) should be (None)
//    difference(JObject("obj" -> JObject("four" -> JObject("five" := 5))), obj) should be (None)
//
//    difference(JObject("one" := 2), obj) should be (Some(JObject("one" := 2)))
//    difference(JObject("six" := 6), obj) should be (Some(JObject("six" := 6)))
//    difference(JObject("obj" -> JObject("four" -> JObject("six" := 34.56))), obj) should be (Some(JObject("obj" -> JObject("four" -> JObject("six" := 34.56)))))
//    difference(JObject("obj" -> JObject("two" := true, "three" := List(1,2,3,4))), obj) should be (Some(JObject("obj" -> JObject("two" := true))))
//    difference(JObject("obj" -> JObject("three" := List(1,2,3,4,5,6))), obj) should be (Some(JObject("obj" -> JObject("three" := List(1,2,3,4,5,6)))))
//  }
}
