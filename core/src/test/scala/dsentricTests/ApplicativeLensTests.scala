package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}
import shapeless.ops.hlist.Tupler

class ApplicativeLensTests extends FunSuite with Matchers {
  import J._
  import ApplicativeLens._
  import Tupler._

  object TestObj extends Contract {
    val int = \[Int]
    val bool = \[Boolean]
    val string = \[String]("string")

    lazy val triple = TestObj.string @: TestObj.int @: TestObj.bool
  }

  test("Messing about") {
    val json = JsObject(Map("int" -> JsNumber(1), "bool" -> JsBool(false), "string" -> JsString("Test")))

    json match {
      case TestObj.triple(s, i, b) =>
        s should equal ("Test")
        i should equal (1)
        b should equal (false)
    }

    TestObj.triple.$set(("string", 3, true))(json) should be (JsObject(Map("int" -> JsNumber(3), "bool" -> JsBool(true), "string" -> JsString("string"))))
  }
}
