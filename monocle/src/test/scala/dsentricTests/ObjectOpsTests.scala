package dsentric

import dsentric.ObjectOps
import dsentric.J._
import org.scalatest.{FunSuite, Matchers}

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

  test("Difference tests") {
    val obj = JsObject(Map("one" -> JsNumber(1),
      "obj" -> JsObject(Map(
        "two" -> JsBool(false),
        "three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4))),
        "four" -> JsObject(Map("five" -> JsNumber(5)))))))

    deltaDifference[Json, JsObject](obj, obj, Some(JsNull)) should be (None)
    deltaDifference[Json, JsObject](JsObject(Map.empty), obj, Some(JsNull)) should be (None)
    deltaDifference[Json, JsObject](JsObject(Map("one" -> JsNumber(1))), obj, Some(JsNull)) should be (None)
    deltaDifference[Json, JsObject](JsObject(Map("one" -> JsNumber(1), "obj" -> JsObject(Map("two" -> JsBool(false))))), obj, Some(JsNull)) should be (None)
    deltaDifference[Json, JsObject](JsObject(Map("five" -> JsNull, "obj" -> JsObject(Map("six" -> JsNull)))), obj, Some(JsNull)) should be (None)
    deltaDifference[Json, JsObject](JsObject(Map("obj" -> JsObject(Map("four" -> JsObject(Map.empty))))), obj, Some(JsNull)) should be (None)
    deltaDifference[Json, JsObject](JsObject(Map("obj" -> JsObject(Map("four" -> JsObject(Map("five" -> JsNumber(5))))))), obj, Some(JsNull)) should be (None)

    deltaDifference[Json, JsObject](JsObject(Map("one" -> JsNumber(2))), obj, Some(JsNull)) should be (Some(JsObject(Map("one" -> JsNumber(2)))))
    deltaDifference[Json, JsObject](JsObject(Map("six" -> JsNumber(6))), obj, Some(JsNull)) should be (Some(JsObject(Map("six" -> JsNumber(6)))))
    deltaDifference[Json, JsObject](JsObject(Map("obj" -> JsObject(Map("four" -> JsObject(Map("six" -> JsNumber(34))))))), obj, Some(JsNull)) should be (
      Some(JsObject(Map("obj" -> JsObject(Map("four" -> JsObject(Map("six" -> JsNumber(34))))))))
    )
    deltaDifference[Json, JsObject](JsObject(Map("obj" -> JsObject(Map("two" -> JsBool(true), "three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4))))))), obj, Some(JsNull)) should be (
      Some(JsObject(Map("obj" -> JsObject(Map("two" -> JsBool(true))))))
    )
    deltaDifference[Json, JsObject](JsObject(Map("obj" -> JsObject(Map("three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4),JsNumber(5),JsNumber(6))))))), obj, Some(JsNull)) should be (
      Some(JsObject(Map("obj" -> JsObject(Map("three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4),JsNumber(5),JsNumber(6))))))))
    )
  }

  test("nested map") {
    val obj = JsObject(
      Map(
        "one" -> JsNumber(1),
        "obj" -> JsObject(Map(
          "two" -> JsBool(false),
          "three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4))),
          "four" -> JsObject(Map("five" -> JsNumber(5))))),
        "five" -> JsNumber(0)
      ))

    nestedMap[Int, Int, Json, JsObject](obj){
      case i => i + 1
    } should equal (
      JsObject(
        Map(
          "one" -> JsNumber(2),
          "obj" -> JsObject(Map(
            "two" -> JsBool(false),
            "three" -> JsArray(List(JsNumber(1),JsNumber(2),JsNumber(3),JsNumber(4))),
            "four" -> JsObject(Map("five" -> JsNumber(6))))),
          "five" -> JsNumber(1)
        )
      )
    )
  }
}