package dsentricTests

import dsentric.ObjectOps
import org.scalatest.{Matchers, FunSuite}
import J._

class ObjectOpsTests  extends FunSuite with Matchers with ObjectOps {

  test("Applying value delta on obj") {
    val obj = JsObject(Map("one" -> JsNumber(1), "two" -> JsString("two")))
    applyDelta[Json, JsObject](obj, JsString("value"), Some(JsNull)) should equal(JsString("value"))
    applyDelta[Json, JsObject](obj, JsNull, Some(JsNull)) should equal(JsNull)
  }

  test("Applying single key delta object value on obj") {
    val obj = JsObject(Map("one" -> JsNumber(1), "two" -> JsString("two")))
    applyDelta[Json, JsObject](obj, JsObject(Map.empty), Some(JsNull)) should equal (obj)
    applyDelta[Json, JsObject](obj, JsObject(Map("three" -> JsNumber(3))), Some(JsNull)) should equal (JsObject(Map("one" -> JsNumber(1), "two" -> JsString("two"), "three" -> JsNumber(3))))
    applyDelta[Json, JsObject](obj, JsObject(Map("two" -> JsString("three"))), Some(JsNull)) should equal (JsObject(Map("one" -> JsNumber(1), "two" -> JsString("three"))))
    applyDelta[Json, JsObject](obj, JsObject(Map("one" -> JsNull)), Some(JsNull)) should equal (JsObject(Map("two" -> JsString("two"))))
    applyDelta[Json, JsObject](obj, JsObject(Map("two" -> JsObject(Map.empty))), Some(JsNull)) should equal (JsObject(Map("one" -> JsNumber(1))))
  }

  test("Applying nested delta object") {
    val obj = JsObject(Map("one" -> JsNumber(1),
      "obj" -> JsObject(Map(
        "two" -> JsBool(false),
        "three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4))),
        "four" -> JsObject(Map("five" -> JsNumber(5)))))))

    applyDelta[Json, JsObject](obj, JsObject(Map.empty), Some(JsNull)) should equal (obj)
    applyDelta[Json, JsObject](obj, JsObject(Map("obj" -> JsNull)), Some(JsNull)) should equal (JsObject(Map("one" -> JsNumber(1))))
    applyDelta[Json, JsObject](obj, JsObject(Map("obj" -> JsObject(Map("two" -> JsBool(true))))), Some(JsNull)) should equal (
      JsObject(Map("one" -> JsNumber(1),
      "obj" -> JsObject(Map("two" -> JsBool(true), "three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4))), "four" -> JsObject(Map("five" -> JsNumber(5)))))))
    )
    applyDelta[Json, JsObject](obj, JsObject(Map("obj" -> JsObject(Map("three" -> JsNull)))), Some(JsNull)) should equal (
      JsObject(Map("one" -> JsNumber(1),
        "obj" -> JsObject(Map("two" -> JsBool(false),"four" -> JsObject(Map("five" -> JsNumber(5)))))))
      )
    applyDelta[Json, JsObject](obj, JsObject(Map("obj" -> JsObject(Map("two" -> JsNull, "three" -> JsNull, "four" -> JsNull)))), Some(JsNull)) should equal (JsObject(Map("one" -> JsNumber(1))))

    applyDelta[Json, JsObject](obj, JsObject(Map("obj" -> JsObject(Map("six" -> JsString("vi"))))), Some(JsNull)) should equal (
      JsObject(Map("one" -> JsNumber(1),
        "obj" -> JsObject(Map(
          "two" -> JsBool(false),
          "three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4))),
          "four" -> JsObject(Map("five" -> JsNumber(5))),
          "six" -> JsString("vi")))))
      )
    }
}