package dsentricTests

import dsentric.AndMatcher._
import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractTests extends FunSuite with Matchers {

  import PessimisticCodecs._
  implicit def strictness = MaybePessimistic

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

    (JObject(Map("one" -> "string", "two" -> false)) match {
      case Flat.one(s) =>
        s
    }) should equal("string")

    (JObject(Map("one" -> "string", "two" -> false)) match {
      case Flat.one(s) && Flat.two(Some(false)) =>
        s
    }) should equal("string")

    (JObject(Map("one" -> "string", "two" -> false)) match {
      case Flat.two(None) =>
        assert(false)
      case _ =>
        "no match"
    }) should equal("no match")

    (JObject(Map("one" -> "string")) match {
      case Flat.one(s) && Flat.two(None) && Flat.three(int) =>
        true
    }) should equal(true)

    (JObject(Map("one" -> 123)) match {
      case Flat.one(s) && Flat.two(None) => s
      case _ => "wrong type"
    }) should equal("wrong type")

    (JObject(Map("two" -> false)) match {
      case Flat.one(s) && Flat.two(None) => s
      case Flat.two(Some(false)) => "two match"
    }) should equal("two match")

    (JObject(Map("three" -> 4)) match {
      case Flat.three(i) => i
    }) should equal(4)

    (JObject(Map("three" -> "4")) match {
      case Flat.three(i) => true
      case _ => false
    }) should equal(false)

    (JObject(Map("two" -> "String")) match {
      case Flat.two(None) => true
      case _ => false
    }) should equal(false)

    //    (JsObject(Map("two" -> JsNull)) match {
    //      case Test.two(None) => true
    //      case _ => false
    //    }) should be(true)

    (JObject(Map("two" -> "false")) match {
      case Flat.two(Some(i)) => true
      case _ => false
    }) should equal(false)

    (JObject(Map("three" -> "not a number")) match {
      case Flat.three(i) => i
      case _ => "wrong type"
    }) should equal("wrong type")

    (JObject(Map.empty) match {
      case Flat.two(None) => true
      case _ => false
    }) should be (true)
  }

  test("nested Contract pattern matching") {
    (JObject(Map("child" -> Map("value" -> 2))) match {
      case Nested.child.value(s) =>
        s
    }) should equal(2)
  }

  test("recursive Contract pattern matching") {
    val obj = JObject(Map("name" -> "one", "child" -> Map("name" -> "two", "child" -> Map("name" -> "three"))))
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

    val subObj = JObject(Map("name" -> "one", "first" -> Map("name2" -> "two", "second" -> Map("name" -> "three", "first" -> Map("name2" -> "four")))))

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
