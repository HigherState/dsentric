package dsentricTests

import org.scalatest.{Matchers, FunSuite}

/**
 * Created by jamie on 19/11/15.
 */
class LensTests extends FunSuite with Matchers with FailureMatchers {

  import J._

  object ExpectedField extends Contract {
    val field = \[String]
  }

  test("expected set/modify") {
    ExpectedField.field.$set("test")(JsObject(Map.empty)) should be (JsObject(Map("field" -> JsString("test"))))
  }
}
