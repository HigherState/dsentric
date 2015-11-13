package dsentricTests

import dsentric._
import org.scalatest.{Matchers, FunSuite}
import AndMatcher._

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

    (JsObject(Map("two" -> JsNull)) match {
      case Test.two(None) => true
      case _ => false
    }) should be(true)

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
    }) should be(true)
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
}
