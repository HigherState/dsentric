package dsentricTests

import dsentric._
import Dsentric._
import org.scalatest.{FunSuite, Matchers}

class ApplicativeLensTests extends FunSuite with Matchers {

  import dsentric.ApplicativeLens._
  import PessimisticCodecs._

  object TestObj extends Contract {
    val int = \[Int]
    val bool = \[Boolean]
    val string = \[String]("string")

    lazy val triple = TestObj.string @: TestObj.int @: TestObj.bool
  }

  trait TestSubObject extends SubContract {
    val int = \[Int]
    val bool = \[Boolean]
    val string = \[String]("string")

    lazy val triple = TestObj.string @: TestObj.int @: TestObj.bool
  }

  object WithTest extends Contract with TestSubObject

  test("Messing about") {
    val json = DObject("int" := 1, "bool" := false, "string" := "Test")

    json match {
      case WithTest.triple(s, i, b) =>
        s should equal ("Test")
        i should equal (1)
        b should equal (false)
    }

    WithTest.triple.$set(("string", 3, true))(json) should be (DObject("int" := 3, "bool" := true, "string" := "string"))
  }
}
