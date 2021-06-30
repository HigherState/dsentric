package dsentric

import dsentric._
import Dsentric._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ApplicativeLensTests extends AnyFunSuite with Matchers {

  import dsentric.ApplicativeLens._
  import dsentric.codecs.std.DCodecs._

  object TestObj extends Contract {
    val int = \[Int]
    val bool = \[Boolean]
    val string = \[String]("string")

    lazy val triple = TestObj.string @: TestObj.int @: TestObj.bool
  }

  test("Contract applicative") {
    val json = DObject("int" := 1, "bool" := false, "string" := "Test")

    json match {
      case WithTest.triple(s, i, b) =>
        s should equal ("Test")
        i should equal (1)
        b should equal (false)
    }

   // WithTest.triple.$set(("string", 3, true))(json) should be (DObject("int" := 3, "bool" := true, "string" := "string"))
  }

  trait TestSubObject extends SubContract {
    val int = \[Int]
    val bool = \[Boolean]
    val string = \[String]("string")

    lazy val triple = TestObj.string @: TestObj.int @: TestObj.bool
  }

  object WithTest extends Contract with TestSubObject

  test("Subcontract applicative about") {
    val json = DObject("int" := 1, "bool" := false, "string" := "Test")

    json match {
      case WithTest.triple(s, i, b) =>
        s should equal ("Test")
        i should equal (1)
        b should equal (false)
    }
  }

  object MatcherMaybe extends Contract {
    val maybeInt = \?[Int]
    val string = \[String]
    val maybeBoolean = \?[Boolean]

    lazy val compo = maybeBoolean @: string
    lazy val comp1 = maybeInt @: string @: maybeBoolean
    lazy val comp2 = maybeInt @: maybeBoolean @: string
  }

  test("Maybe Applicative") {
    val empty = DObject("string" := "s")
    val full = DObject("string" := "s", "maybeInt" := 4, "maybeBoolean" := false)

    empty match {
      case MatcherMaybe.comp1((None, s, None)) =>
        s should equal ("s")
    }
  }
}
