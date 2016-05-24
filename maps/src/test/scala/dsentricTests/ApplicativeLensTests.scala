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

    //WithTest.triple.$set(("string", 3, true))(json) should be (DObject("int" := 3, "bool" := true, "string" := "string"))
  }

  object MatcherContract extends ContractType("req", "check") {
    val req = \[String]
    val int = \[Int]
    val string = \[String]

    lazy val one = this.isType @: string
    lazy val two = this.isType @: int @: string
  }

  test("Matcher one applicative") {
    val isMatch = DObject("req" := "check", "int" := 3, "string" := "s")
    val notMatch = DObject("req" := "cross", "int" := 3, "string" := "s")

    isMatch match {
      case MatcherContract.one(s) =>
        s should equal ("s")
    }

    notMatch match {
      case MatcherContract.one(s) =>
        assert(false)
      case _ =>

    }

   // MatcherContract.one.$set("ss")(isMatch) should be (DObject("req" := "check", "int" := 3, "string" := "ss"))
  }

  test("Matcher two applicative") {
    val isMatch = DObject("req" := "check", "int" := 3, "string" := "s")
    val notMatch = DObject("req" := "cross", "int" := 3, "string" := "s")

    isMatch match {
      case MatcherContract.two((i, s)) =>
        i should equal (3)
        s should equal ("s")
    }

    notMatch match {
      case MatcherContract.two((i, s)) =>
        assert(false)
      case _ =>

    }

    //MatcherContract.two.$set((5, "ss"))(isMatch) should be (DObject("req" := "check", "int" := 5, "string" := "ss"))
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
