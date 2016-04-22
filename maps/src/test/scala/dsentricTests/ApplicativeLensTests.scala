package dsentricTests

import dsentric._
import Dsentric._
import org.scalatest.{FunSuite, Matchers}

class ApplicativeLensTests extends FunSuite with Matchers {

  import dsentric.ApplicativeLens._
  import PessimisticCodecs._
  implicit def strictness = MaybePessimistic

  object TestObj extends Contract {
    val int = \[Int]
    val bool = \[Boolean]
    val string = \[String]("string")

    lazy val triple = TestObj.string @: TestObj.int @: TestObj.bool
  }

  test("Messing about") {
    val json = JObject("int" := 1, "bool" := false, "string" := "Test")

    json match {
      case TestObj.triple(s, i, b) =>
        s should equal ("Test")
        i should equal (1)
        b should equal (false)
    }

    TestObj.triple.$set(("string", 3, true))(json) should be (JObject("int" := 3, "bool" := true, "string" := "string"))
  }
}
