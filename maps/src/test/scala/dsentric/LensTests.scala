package dsentric

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class LensTests extends FunSuite with Matchers with FailureMatchers {

  import PessimisticCodecs._
  import Dsentric._

  object ExpectedField extends Contract {
    val field = \[String]
    val copy = \[String]
    val maybeCopied = \?[String]
  }

  test("expected lens") {
    ExpectedField.field.$set("test")(DObject.empty) should be (DObject("field" := "test"))
    ExpectedField.field.$set("test2")(DObject("field" := "test")) should be (DObject("field" := "test2"))

    ExpectedField.field.$get(DObject.empty) should be (None)
    ExpectedField.field.$get(DObject("field" := "test")) should be (Some("test"))

    ExpectedField.field.$maybeSet(None)(DObject.empty) should be (DObject.empty)
    ExpectedField.field.$maybeSet(Some("test"))(DObject.empty) should be (DObject("field" := "test"))
    ExpectedField.field.$maybeSet(Some("test2"))(DObject("field" := "test")) should be (DObject("field" := "test2"))

    ExpectedField.field.$modify(_ + "2")(DObject.empty) should be (DObject.empty)
    ExpectedField.field.$modify(_ + "2")(DObject("field" := false)) should be (DObject("field" := false))
    ExpectedField.field.$modify(_ + "2")(DObject("field" := "test")) should be (DObject("field" := "test2"))

    ExpectedField.copy.$copy(ExpectedField.field)(DObject.empty) should be (DObject.empty)
    ExpectedField.copy.$copy(ExpectedField.field)(DObject("copy" := "leave")) should be (DObject("copy" := "leave"))
    ExpectedField.copy.$copy(ExpectedField.field)(DObject("field" := "test")) should be (DObject("field" := "test", "copy" := "test"))
    ExpectedField.copy.$maybeCopy(ExpectedField.maybeCopied)(DObject("copy" := "test")) should be (DObject("copy" := "test"))
    ExpectedField.copy.$maybeCopy(ExpectedField.maybeCopied)(DObject("copy" := "test", "maybeCopied" := "test2")) should be (DObject("copy" := "test2", "maybeCopied" := "test2"))
  }

  test("Compositor") {
    ExpectedField.field.$set("test") ~
    ExpectedField.copy.$set("test2") |>
      DObject.empty should be (DObject("field" := "test", "copy" := "test2"))
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
    NestedField.nested.field.$set("test")(DObject("nested" := DObject.empty)) should
      be (DObject("nested" := DObject("field" := "test")))
    NestedField.nested.field.$modify(_ + "2")(DObject("nested" := DObject("field" := "test"))) should
      be (DObject("nested" := DObject("field" := "test2")))
    NestedField.nested.field.$copy(NestedField.nested.nested.copy)(DObject("nested" := DObject("nested" := DObject("copy" := "test")))) should
      be (DObject("nested" := DObject("field" := "test", "nested" := DObject("copy" := "test"))))
  }

  object MaybeField extends Contract {
    val field = \?[String]
    val copy = \?[String]
  }

  test("maybe lens") {
    MaybeField.field.$set("test")(DObject.empty) should be (DObject("field" := "test"))
    MaybeField.field.$set("test2")(DObject("field" := "test")) should be (DObject("field" := "test2"))

    MaybeField.field.$get(DObject.empty) should be (None)
    MaybeField.field.$get(DObject("field" := "test")) should be (Some("test"))

    MaybeField.field.$maybeSet(None)(DObject.empty) should be (DObject.empty)
    MaybeField.field.$maybeSet(Some("test"))(DObject.empty) should be (DObject("field" := "test"))
    MaybeField.field.$maybeSet(Some("test2"))(DObject("field" := "test")) should be (DObject("field" := "test2"))

    MaybeField.field.$modifyOrDrop(t => t.map(_ + "2"))(DObject.empty) should be (DObject.empty)
    //strict
    MaybeField.field.$modifyOrDrop(t => t.map(_ + "2"))(DObject("field" := false)) should be (DObject("field" := false))
    MaybeField.field.$modifyOrDrop(t => t.map(_ + "2"))(DObject("field" := "test")) should be (DObject("field" := "test2"))

    MaybeField.copy.$copy(MaybeField.field)(DObject.empty) should be (DObject.empty)
    //strict
    MaybeField.copy.$copy(MaybeField.field)(DObject("copy" := "remove")) should be (DObject.empty)
    MaybeField.copy.$copy(MaybeField.field)(DObject("field" := "test")) should be (DObject("field" := "test", "copy" := "test"))

    MaybeField.field.$drop(DObject("field" := "test")) should be (DObject.empty)

  }

  object DefaultField extends Contract {
    val field = \![String]("default1")
    val copy = \![String]("default2")
    val maybeCopied = \?[String]
  }

  test("default lens") {
    DefaultField.field.$set("test")(DObject.empty) should be (DObject("field" := "test"))
    DefaultField.field.$set("test2")(DObject("field" := "test")) should be (DObject("field" := "test2"))

    DefaultField.field.$get(DObject.empty) should be ("default1")
    DefaultField.field.$get(DObject("field" := "test")) should be ("test")

    DefaultField.field.$maybeSet(None)(DObject.empty) should be (DObject.empty)
    DefaultField.field.$maybeSet(Some("test"))(DObject.empty) should be (DObject("field" := "test"))
    DefaultField.field.$maybeSet(Some("test2"))(DObject("field" := "test")) should be (DObject("field" := "test2"))

    DefaultField.field.$modify(t => t + "2")(DObject.empty) should be (DObject("field" := "default12"))
    //Strict
    DefaultField.field.$modify(t => t + "2")(DObject("field" := false)) should be (DObject("field" := false))
    DefaultField.field.$modify(t => t + "2")(DObject("field" := "test")) should be (DObject("field" := "test2"))

    DefaultField.copy.$copy(DefaultField.field)(DObject.empty) should be (DObject("copy" := "default1"))
    DefaultField.copy.$copy(DefaultField.field)(DObject("copy" := "value")) should be (DObject("copy" := "default1"))
    DefaultField.copy.$copy(DefaultField.field)(DObject("field" := "test")) should be (DObject("field" := "test", "copy" := "test"))

    DefaultField.field.$restore(DObject("field" := "test")) should be (DObject.empty)
    DefaultField.copy.$maybeCopy(DefaultField.maybeCopied)(DObject("copy" := "test")) should be (DObject("copy" := "test"))
    DefaultField.copy.$maybeCopy(DefaultField.maybeCopied)(DObject("copy" := "test", "maybeCopied" := "test2")) should be (DObject("copy" := "test2", "maybeCopied" := "test2"))

    //DefaultField.field.$deltaDelete(JObject("field" := "test")))) should be (JObject("field" := JsNull)))
  }
}
