package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class LensTests extends FunSuite with Matchers with FailureMatchers {

  import PessimisticCodecs._
  import Dsentric._

  object ExpectedField extends Contract {
    val field = \[String]
    val copy = \[String]
  }

  test("expected lens") {
    ExpectedField.field.$set("test")(JObject.empty) should be (JObject("field" := "test"))
    ExpectedField.field.$set("test2")(JObject("field" := "test")) should be (JObject("field" := "test2"))

    ExpectedField.field.$get(JObject.empty) should be (None)
    ExpectedField.field.$get(JObject("field" := "test")) should be (Some("test"))

    ExpectedField.field.$maybeSet(None)(JObject.empty) should be (JObject.empty)
    ExpectedField.field.$maybeSet(Some("test"))(JObject.empty) should be (JObject("field" := "test"))
    ExpectedField.field.$maybeSet(Some("test2"))(JObject("field" := "test")) should be (JObject("field" := "test2"))

    ExpectedField.field.$modify(_ + "2")(JObject.empty) should be (JObject.empty)
    ExpectedField.field.$modify(_ + "2")(JObject("field" := false)) should be (JObject("field" := false))
    ExpectedField.field.$modify(_ + "2")(JObject("field" := "test")) should be (JObject("field" := "test2"))

    ExpectedField.copy.$copy(ExpectedField.field)(JObject.empty) should be (JObject.empty)
    ExpectedField.copy.$copy(ExpectedField.field)(JObject("copy" := "leave")) should be (JObject("copy" := "leave"))
    ExpectedField.copy.$copy(ExpectedField.field)(JObject("field" := "test")) should be (JObject("field" := "test", "copy" := "test"))
  }

  test("Compositor") {
    ExpectedField.field.$set("test") ~
    ExpectedField.copy.$set("test2") |>
      JObject.empty should be (JObject("field" := "test", "copy" := "test2"))
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
    NestedField.nested.field.$set("test")(JObject("nested" := JObject.empty)) should
      be (JObject("nested" := JObject("field" := "test")))
    NestedField.nested.field.$modify(_ + "2")(JObject("nested" := JObject("field" := "test"))) should
      be (JObject("nested" := JObject("field" := "test2")))
    NestedField.nested.field.$copy(NestedField.nested.nested.copy)(JObject("nested" := JObject("nested" := JObject("copy" := "test")))) should
      be (JObject("nested" := JObject("field" := "test", "nested" := JObject("copy" := "test"))))
  }

  object MaybeField extends Contract {
    val field = \?[String]
    val copy = \?[String]
  }

  test("maybe lens") {
    MaybeField.field.$set("test")(JObject.empty) should be (JObject("field" := "test"))
    MaybeField.field.$set("test2")(JObject("field" := "test")) should be (JObject("field" := "test2"))

    MaybeField.field.$get(JObject.empty) should be (None)
    MaybeField.field.$get(JObject("field" := "test")) should be (Some("test"))

    MaybeField.field.$maybeSet(None)(JObject.empty) should be (JObject.empty)
    MaybeField.field.$maybeSet(Some("test"))(JObject.empty) should be (JObject("field" := "test"))
    MaybeField.field.$maybeSet(Some("test2"))(JObject("field" := "test")) should be (JObject("field" := "test2"))

    MaybeField.field.$modifyOrDrop(t => t.map(_ + "2"))(JObject.empty) should be (JObject.empty)
    //strict
    MaybeField.field.$modifyOrDrop(t => t.map(_ + "2"))(JObject("field" := false)) should be (JObject("field" := false))
    MaybeField.field.$modifyOrDrop(t => t.map(_ + "2"))(JObject("field" := "test")) should be (JObject("field" := "test2"))

    MaybeField.copy.$copy(MaybeField.field)(JObject.empty) should be (JObject.empty)
    //strict
    MaybeField.copy.$copy(MaybeField.field)(JObject("copy" := "remove")) should be (JObject.empty)
    MaybeField.copy.$copy(MaybeField.field)(JObject("field" := "test")) should be (JObject("field" := "test", "copy" := "test"))

    MaybeField.field.$drop(JObject("field" := "test")) should be (JObject.empty)

  }

  object DefaultField extends Contract {
    val field = \![String]("default1")
    val copy = \![String]("default2")
  }

  test("default lens") {
    DefaultField.field.$set("test")(JObject.empty) should be (JObject("field" := "test"))
    DefaultField.field.$set("test2")(JObject("field" := "test")) should be (JObject("field" := "test2"))

    DefaultField.field.$get(JObject.empty) should be ("default1")
    DefaultField.field.$get(JObject("field" := "test")) should be ("test")

    DefaultField.field.$maybeSet(None)(JObject.empty) should be (JObject.empty)
    DefaultField.field.$maybeSet(Some("test"))(JObject.empty) should be (JObject("field" := "test"))
    DefaultField.field.$maybeSet(Some("test2"))(JObject("field" := "test")) should be (JObject("field" := "test2"))

    DefaultField.field.$modify(t => t + "2")(JObject.empty) should be (JObject("field" := "default12"))
    //Strict
    DefaultField.field.$modify(t => t + "2")(JObject("field" := false)) should be (JObject("field" := false))
    DefaultField.field.$modify(t => t + "2")(JObject("field" := "test")) should be (JObject("field" := "test2"))

    DefaultField.copy.$copy(DefaultField.field)(JObject.empty) should be (JObject("copy" := "default1"))
    DefaultField.copy.$copy(DefaultField.field)(JObject("copy" := "value")) should be (JObject("copy" := "default1"))
    DefaultField.copy.$copy(DefaultField.field)(JObject("field" := "test")) should be (JObject("field" := "test", "copy" := "test"))

    DefaultField.field.$restore(JObject("field" := "test")) should be (JObject.empty)

    //DefaultField.field.$deltaDelete(JObject("field" := "test")))) should be (JObject("field" := JsNull)))
  }
}
