package dsentricTests

import dsentric.AndMatcher._
import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractTests extends FunSuite with Matchers {

  import PessimisticCodecs._
  implicit def strictness = MaybePessimistic
  import Dsentric._

  object Flat extends Contract {
    val one = \[String]
    val two = \?[Boolean]
    val three = \![Int](3)
  }

  object Nested extends Contract {
    val child = new \\ {
      val value = \[Int]
    }
  }

  trait Recursive extends SubContract {
    val name = \[String]
    lazy val child = new \\? with Recursive
  }

  object Recursive extends Contract with Recursive

  trait SubRecursive extends SubContract {
    val name = \[String]
    val first = new \\ {
      val name2 = \[String]
      lazy val second = new \\? with SubRecursive
    }
  }

  object SubRecursive extends Contract with SubRecursive

  test("Level Contract pattern matching") {

    (JObject("one" := "string", "two" := false) match {
      case Flat.one(s) =>
        s
    }) should equal("string")

    (JObject("one" := "string", "two" := false) match {
      case Flat.one(s) && Flat.two(Some(false)) =>
        s
    }) should equal("string")

    (JObject("one" := "string", "two" := false) match {
      case Flat.two(None) =>
        assert(false)
      case _ =>
        "no match"
    }) should equal("no match")

    (JObject("one" := "string") match {
      case Flat.one(s) && Flat.two(None) && Flat.three(int) =>
        true
    }) should equal(true)

    (JObject("one" := 123) match {
      case Flat.one(s) && Flat.two(None) => s
      case _ => "wrong type"
    }) should equal("wrong type")

    (JObject("two" := false) match {
      case Flat.one(s) && Flat.two(None) => s
      case Flat.two(Some(false)) => "two match"
    }) should equal("two match")

    (JObject("three" := 4) match {
      case Flat.three(i) => i
    }) should equal(4)

    (JObject("three" := "4") match {
      case Flat.three(i) => true
      case _ => false
    }) should equal(false)

    (JObject("two" := "String") match {
      case Flat.two(None) => true
      case _ => false
    }) should equal(false)

    //    (JsObject("two" := JsNull)) match {
    //      case Test.two(None) => true
    //      case _ => false
    //    }) should be(true)

    (JObject("two" := "false") match {
      case Flat.two(Some(i)) => true
      case _ => false
    }) should equal(false)

    (JObject("three" := "not a number") match {
      case Flat.three(i) => i
      case _ => "wrong type"
    }) should equal("wrong type")

    (JObject.empty match {
      case Flat.two(None) => true
      case _ => false
    }) should be (true)
  }

  test("nested Contract pattern matching") {
    (JObject("child" := JObject("value" := 2)) match {
      case Nested.child.value(s) =>
        s
    }) should equal(2)

    (JObject("child" := JObject("value" := 2)) match {
      case Nested.child(j) =>
        j
    }) should equal(JObject("value" := 2))
  }

  test("recursive Contract pattern matching") {
    val obj = JObject("name" := "one", "child" := JObject("name" := "two", "child" := JObject("name" := "three")))
    (obj match {
      case Recursive.name(name) =>
        name
    }) should equal ("one")
    (obj match {
      case Recursive.child.name(name) =>
        name
    }) should equal ("two")
    (obj match {
      case Recursive.child.child.name(name) =>
        name
    }) should equal ("three")

    val subObj = JObject("name" := "one", "first" := JObject("name2" := "two", "second" := JObject("name" := "three", "first" := JObject("name2" := "four"))))

    (subObj match {
      case SubRecursive.name(name) =>
        name
    }) should equal ("one")
    (subObj match {
      case SubRecursive.first.name2(name) =>
        name
    }) should equal ("two")
    (subObj match {
      case SubRecursive.first.second.name(name) =>
        name
    }) should equal ("three")
    (subObj match {
      case SubRecursive.first.second.first.name2(name) =>
        name
    }) should equal ("four")
  }

}
