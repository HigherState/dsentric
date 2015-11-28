package dsentricTests

import org.scalatest.{Matchers, FunSuite}

class LensTests extends FunSuite with Matchers with FailureMatchers {

  import J._

  object ExpectedField extends Contract {
    val field = \[String]
    val copy = \[String]
  }

  test("expected lens") {
    ExpectedField.field.$set("test")(JsObject(Map.empty)) should be (JsObject(Map("field" -> JsString("test"))))
    ExpectedField.field.$set("test2")(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    ExpectedField.field.$get(JsObject(Map.empty)) should be (None)
    ExpectedField.field.$get(JsObject(Map("field" -> JsString("test")))) should be (Some("test"))

    ExpectedField.field.$maybeSet(None)(JsObject(Map.empty)) should be (JsObject(Map.empty))
    ExpectedField.field.$maybeSet(Some("test"))(JsObject(Map.empty)) should be (JsObject(Map("field" -> JsString("test"))))
    ExpectedField.field.$maybeSet(Some("test2"))(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    ExpectedField.field.$modify(_ + "2")(JsObject(Map.empty)) should be (JsObject(Map.empty))
    ExpectedField.field.$modify(_ + "2")(JsObject(Map("field" -> JsBool(false)))) should be (JsObject(Map("field" -> JsBool(false))))
    ExpectedField.field.$modify(_ + "2")(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    ExpectedField.copy.$copy(ExpectedField.field)(JsObject(Map.empty)) should be (JsObject(Map.empty))
    ExpectedField.copy.$copy(ExpectedField.field)(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test"), "copy" -> JsString("test"))))
  }

  test("Compositor") {
    ExpectedField.field.$set("test") ~
    ExpectedField.copy.$set("test2") |>
      JsObject(Map.empty) should be (JsObject(Map("field" -> JsString("test"), "copy" -> JsString("test2"))))
  }

  object NestedField extends Contract {
    val nested = new \\{
      val field = \[String]
      val nested = new \\ {
        val copy = \[String]
      }
    }
  }

  test("nested lens, nested objects exist") {
    NestedField.nested.field.$set("test")(JsObject(Map("nested" -> JsObject(Map.empty)))) should
      be (JsObject(Map("nested" -> JsObject(Map("field" -> JsString("test"))))))
    NestedField.nested.field.$modify(_ + "2")(JsObject(Map("nested" -> JsObject(Map("field" -> JsString("test")))))) should
      be (JsObject(Map("nested" -> JsObject(Map("field" -> JsString("test2"))))))
    NestedField.nested.field.$copy(NestedField.nested.nested.copy)(JsObject(Map("nested" -> JsObject(Map("nested" -> JsObject(Map("copy" -> JsString("test")))))))) should
      be (JsObject(Map("nested" -> JsObject(Map("field" -> JsString("test"), "nested" -> JsObject(Map("copy" -> JsString("test"))))))))
  }


  object MaybeField extends Contract {
    val field = \?[String]
    val copy = \?[String]
  }

  test("maybe lens") {
    MaybeField.field.$set("test")(JsObject(Map.empty)) should be (JsObject(Map("field" -> JsString("test"))))
    MaybeField.field.$set("test2")(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    MaybeField.field.$get(JsObject(Map.empty)) should be (None)
    MaybeField.field.$get(JsObject(Map("field" -> JsString("test")))) should be (Some("test"))

    MaybeField.field.$maybeSet(None)(JsObject(Map.empty)) should be (JsObject(Map.empty))
    MaybeField.field.$maybeSet(Some("test"))(JsObject(Map.empty)) should be (JsObject(Map("field" -> JsString("test"))))
    MaybeField.field.$maybeSet(Some("test2"))(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    MaybeField.field.$modify(t => t.map(_ + "2")) (JsObject(Map.empty)) should be (JsObject(Map.empty))
    MaybeField.field.$modify(t => t.map(_ + "2"))(JsObject(Map("field" -> JsBool(false)))) should be (JsObject(Map()))
    MaybeField.field.$modify(t => t.map(_ + "2"))(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    MaybeField.copy.$copy(MaybeField.field)(JsObject(Map.empty)) should be (JsObject(Map.empty))
    MaybeField.copy.$copy(MaybeField.field)(JsObject(Map("copy" -> JsString("remove")))) should be (JsObject(Map.empty))
    MaybeField.copy.$copy(MaybeField.field)(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test"), "copy" -> JsString("test"))))

    MaybeField.field.$drop(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map.empty))

    MaybeField.field.$deltaDelete(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsNull)))
  }

  object DefaultField extends Contract {
    val field = \![String]("default1")
    val copy = \![String]("default2")
  }

  test("default lens") {
    DefaultField.field.$set("test")(JsObject(Map.empty)) should be (JsObject(Map("field" -> JsString("test"))))
    DefaultField.field.$set("test2")(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    DefaultField.field.$get(JsObject(Map.empty)) should be (Some("default1"))
    DefaultField.field.$get(JsObject(Map("field" -> JsString("test")))) should be (Some("test"))

    DefaultField.field.$maybeSet(None)(JsObject(Map.empty)) should be (JsObject(Map.empty))
    DefaultField.field.$maybeSet(Some("test"))(JsObject(Map.empty)) should be (JsObject(Map("field" -> JsString("test"))))
    DefaultField.field.$maybeSet(Some("test2"))(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    DefaultField.field.$modify(t => t + "2")(JsObject(Map.empty)) should be (JsObject(Map("field" -> JsString("default12"))))
    DefaultField.field.$modify(t => t + "2")(JsObject(Map("field" -> JsBool(false)))) should be (JsObject(Map("field" -> JsString("default12"))))
    DefaultField.field.$modify(t => t + "2")(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test2"))))

    DefaultField.copy.$copy(DefaultField.field)(JsObject(Map.empty)) should be (JsObject(Map("copy" -> JsString("default1"))))
    DefaultField.copy.$copy(DefaultField.field)(JsObject(Map("copy" -> JsString("value")))) should be (JsObject(Map("copy" -> JsString("default1"))))
    DefaultField.copy.$copy(DefaultField.field)(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsString("test"), "copy" -> JsString("test"))))

    DefaultField.field.$restore(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map.empty))

    DefaultField.field.$deltaDelete(JsObject(Map("field" -> JsString("test")))) should be (JsObject(Map("field" -> JsNull)))
  }
}
