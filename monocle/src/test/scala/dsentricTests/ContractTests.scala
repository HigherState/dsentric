package dsentric

import dsentric.AndMatcher._
import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractTests extends FunSuite with Matchers {

  import J._

  implicit val strictness = MaybePessimistic

  object Test extends J.Contract {
    val one = \[String]
    val two = \?[Boolean]
    val three = \![Int](3)
  }

  test("Contract pattern matching") {

    (JsObject(Map("one" -> JsString("string"), "two" -> JsBool(false))) match {
      case Test.one(s) && Test.two(Some(v)) =>
        s -> v
    }) should equal("string" -> false)

    (JsObject(Map("one" -> JsString("string"))) match {
      case Test.one(s) && Test.two(None) && Test.three(int) =>
        true
    }) should equal(true)

    (JsObject(Map("one" -> JsNumber(123))) match {
      case Test.one(s) && Test.two(None) => s
      case _ => "wrong type"
    }) should equal("wrong type")

    (JsObject(Map("two" -> JsBool(false))) match {
      case Test.one(s) && Test.two(None) => s
      case Test.two(Some(false)) => "two match"
    }) should equal("two match")

    (JsObject(Map("three" -> JsNumber(4))) match {
      case Test.three(i) => i
    }) should equal(4)

    (JsObject(Map("three" -> JsString("4"))) match {
      case Test.three(i) => true
      case _ => false
    }) should equal(false)

    (JsObject(Map("two" -> JsString("String"))) match {
      case Test.two(None) => true
      case _ => false
    }) should equal(false)

//    (JsObject(Map("two" -> JsNull)) match {
//      case Test.two(None) => true
//      case _ => false
//    }) should be(true)

    (JsObject(Map("two" -> JsString("false"))) match {
      case Test.two(Some(i)) => true
      case _ => false
    }) should equal(false)

    (JsObject(Map("three" -> JsString("not a number"))) match {
      case Test.three(i) => i
      case _ => "wrong type"
    }) should equal("wrong type")

    (JsObject(Map.empty) match {
      case Test.two(None) => true
      case _ => false
    }) should be (true)
  }

  object Nested extends J.Contract {
    val parent = new \\ {
      val string = \[String]
    }
  }

  test("nested properties") {
    (JsObject(Map("parent" -> JsObject(Map("string" -> JsString("string"))))) match {
      case Nested.parent.string(s) =>
        s
    }) should equal("string")
  }

  trait RecursiveSub extends Recursive[Json, JsObject] {
    val name = \[String]
    lazy val child = rec(new \\?("child") with RecursiveSub)
  }

  object RecursiveContract extends Contract with RecursiveSub

  test("recursive properties") {
    val hierarchy = JsObject(Map("name" -> JsString("parent"), "child" -> JsObject(Map("name" -> JsString("child"), "child" -> JsObject(Map("name" -> JsString("grandchild")))))))
    (hierarchy match {
      case RecursiveContract.name(v) =>
        v
    }) should equal ("parent")

    (hierarchy match {
      case RecursiveContract.child.name(v) =>
        v
    }) should equal ("child")

    (hierarchy match {
      case RecursiveContract.child.child.name(v) =>
        v
    }) should equal ("grandChild")
  }
}
