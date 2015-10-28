package dsentric

import monocle.{POptional, Optional, function, Prism}
import org.scalatest.{Matchers, FunSuite}
import AndMatcher._

import scalaz.{Applicative, \/}

class ContractTests extends FunSuite with Matchers {

  import J._

  implicit val strictness = MaybePessimistic

  object Test extends Contract[Json, JsObject] {
    val one = new Expected[Json, JsObject, String](Validator.empty, None)
    val two = new Maybe[Json, JsObject, Boolean](Validator.empty, None)
    val three = new Default[Json, JsObject, Int](3, Validator.empty, None)
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
}
