package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class LensTests extends FunSuite with Matchers with FailureMatchers {

  import PessimisticCodecs._
  implicit def strictness = MaybeOptimistic

  object ExpectedField extends Contract {
    val field = \[String]
    val copy = \[String]
  }

  test("expected lens") {
    ExpectedField.field.$set("test")(JObject(Map.empty)) should be (JObject(Map("field" -> "test")))
    ExpectedField.field.$set("test2")(JObject(Map("field" -> "test"))) should be (JObject(Map("field" -> "test2")))

    ExpectedField.field.$get(JObject(Map.empty)) should be (None)
    ExpectedField.field.$get(JObject(Map("field" -> "test"))) should be (Some("test"))

    ExpectedField.field.$maybeSet(None)(JObject(Map.empty)) should be (JObject(Map.empty))
    ExpectedField.field.$maybeSet(Some("test"))(JObject(Map.empty)) should be (JObject(Map("field" -> "test")))
    ExpectedField.field.$maybeSet(Some("test2"))(JObject(Map("field" -> "test"))) should be (JObject(Map("field" -> "test2")))

    ExpectedField.field.$modify(_ + "2")(JObject(Map.empty)) should be (JObject(Map.empty))
    ExpectedField.field.$modify(_ + "2")(JObject(Map("field" -> false))) should be (JObject(Map("field" -> false)))
    ExpectedField.field.$modify(_ + "2")(JObject(Map("field" -> "test"))) should be (JObject(Map("field" -> "test2")))

    ExpectedField.copy.$copy(ExpectedField.field)(JObject(Map.empty)) should be (JObject(Map.empty))
    ExpectedField.copy.$copy(ExpectedField.field)(JObject(Map("field" -> "test"))) should be (JObject(Map("field" -> "test", "copy" -> "test")))
  }

//  test("Compositor") {
//    ExpectedField.field.$set("test") ~
//    ExpectedField.copy.$set("test2") |>
//      JObject(Map.empty) should be (JObject(Map("field" -> "test"), "copy" -> "test2"))))
//  }

  object NestedField extends Contract {
    val nested = new \\{
      val field = \[String]
      val nested = new \\ {
        val copy = \[String]
      }
    }
  }

  test("nested lens, nested objects exist") {
    NestedField.nested.field.$set("test")(JObject(Map("nested" -> Map.empty))) should
      be (JObject(Map("nested" -> Map("field" -> "test"))))
    NestedField.nested.field.$modify(_ + "2")(JObject(Map("nested" -> Map("field" -> "test")))) should
      be (JObject(Map("nested" -> Map("field" -> "test2"))))
    NestedField.nested.field.$copy(NestedField.nested.nested.copy)(JObject(Map("nested" -> Map("nested" -> Map("copy" -> "test"))))) should
      be (JObject(Map("nested" -> Map("field" -> "test"), "nested" -> Map("copy" -> "test"))))
  }


  object MaybeField extends Contract {
    val field = \?[String]
    val copy = \?[String]
  }
//
//  test("maybe lens") {
//    MaybeField.field.$set("test")(JObject(Map.empty)) should be (JObject(Map("field" -> "test"))))
//    MaybeField.field.$set("test2")(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> "test2"))))
//
//    MaybeField.field.$get(JObject(Map.empty)) should be (None)
//    MaybeField.field.$get(JObject(Map("field" -> "test")))) should be (Some("test"))
//
//    MaybeField.field.$maybeSet(None)(JObject(Map.empty)) should be (JObject(Map.empty))
//    MaybeField.field.$maybeSet(Some("test"))(JObject(Map.empty)) should be (JObject(Map("field" -> "test"))))
//    MaybeField.field.$maybeSet(Some("test2"))(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> "test2"))))
//
//    MaybeField.field.$modify(t => t.map(_ + "2")) (JObject(Map.empty)) should be (JObject(Map.empty))
//    MaybeField.field.$modify(t => t.map(_ + "2"))(JObject(Map("field" -> false)))) should be (JObject(Map()))
//    MaybeField.field.$modify(t => t.map(_ + "2"))(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> "test2"))))
//
//    MaybeField.copy.$copy(MaybeField.field)(JObject(Map.empty)) should be (JObject(Map.empty))
//    MaybeField.copy.$copy(MaybeField.field)(JObject(Map("copy" -> "remove")))) should be (JObject(Map.empty))
//    MaybeField.copy.$copy(MaybeField.field)(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> "test"), "copy" -> "test"))))
//
//    MaybeField.field.$drop(JObject(Map("field" -> "test")))) should be (JObject(Map.empty))
//
//    MaybeField.field.$deltaDelete(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> JsNull)))
//  }
//
//  object DefaultField extends Contract {
//    val field = \![String]("default1")
//    val copy = \![String]("default2")
//  }
//
//  test("default lens") {
//    DefaultField.field.$set("test")(JObject(Map.empty)) should be (JObject(Map("field" -> "test"))))
//    DefaultField.field.$set("test2")(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> "test2"))))
//
//    DefaultField.field.$get(JObject(Map.empty)) should be (Some("default1"))
//    DefaultField.field.$get(JObject(Map("field" -> "test")))) should be (Some("test"))
//
//    DefaultField.field.$maybeSet(None)(JObject(Map.empty)) should be (JObject(Map.empty))
//    DefaultField.field.$maybeSet(Some("test"))(JObject(Map.empty)) should be (JObject(Map("field" -> "test"))))
//    DefaultField.field.$maybeSet(Some("test2"))(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> "test2"))))
//
//    DefaultField.field.$modify(t => t + "2")(JObject(Map.empty)) should be (JObject(Map("field" -> "default12"))))
//    DefaultField.field.$modify(t => t + "2")(JObject(Map("field" -> false)))) should be (JObject(Map("field" -> "default12"))))
//    DefaultField.field.$modify(t => t + "2")(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> "test2"))))
//
//    DefaultField.copy.$copy(DefaultField.field)(JObject(Map.empty)) should be (JObject(Map("copy" -> "default1"))))
//    DefaultField.copy.$copy(DefaultField.field)(JObject(Map("copy" -> "value")))) should be (JObject(Map("copy" -> "default1"))))
//    DefaultField.copy.$copy(DefaultField.field)(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> "test"), "copy" -> "test"))))
//
//    DefaultField.field.$restore(JObject(Map("field" -> "test")))) should be (JObject(Map.empty))
//
//    DefaultField.field.$deltaDelete(JObject(Map("field" -> "test")))) should be (JObject(Map("field" -> JsNull)))
//  }
}
