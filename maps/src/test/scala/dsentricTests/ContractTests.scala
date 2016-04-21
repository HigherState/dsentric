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
    //val three = \![Int](3)
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
//    (JsObject(Map("one" -> JsString("string"))) match {
//      case Test.one(s) && Test.two(None) && Test.three(int) =>
//        true
//    }) should equal(true)
//
//    (JsObject(Map("one" -> JsNumber(123))) match {
//      case Test.one(s) && Test.two(None) => s
//      case _ => "wrong type"
//    }) should equal("wrong type")
//
//    (JsObject(Map("two" -> JsBool(false))) match {
//      case Test.one(s) && Test.two(None) => s
//      case Test.two(Some(false)) => "two match"
//    }) should equal("two match")
//
//    (JsObject(Map("three" -> JsNumber(4))) match {
//      case Test.three(i) => i
//    }) should equal(4)
//
//    (JsObject(Map("three" -> JsString("4"))) match {
//      case Test.three(i) => true
//      case _ => false
//    }) should equal(false)
//
//    (JsObject(Map("two" -> JsString("String"))) match {
//      case Test.two(None) => true
//      case _ => false
//    }) should equal(false)
//
////    (JsObject(Map("two" -> JsNull)) match {
////      case Test.two(None) => true
////      case _ => false
////    }) should be(true)
//
//    (JsObject(Map("two" -> JsString("false"))) match {
//      case Test.two(Some(i)) => true
//      case _ => false
//    }) should equal(false)
//
//    (JsObject(Map("three" -> JsString("not a number"))) match {
//      case Test.three(i) => i
//      case _ => "wrong type"
//    }) should equal("wrong type")
//
//    (JsObject(Map.empty) match {
//      case Test.two(None) => true
//      case _ => false
//    }) should be (true)
//  }

//  object Nested extends J.Contract {
//    val parent = new \\ {
//      val string = \[String]
//    }
//  }
//
//  test("nested properties") {
//    (JsObject(Map("parent" -> JsObject(Map("string" -> JsString("string"))))) match {
//      case Nested.parent.string(s) =>
//        s
//    }) should equal("string")
//  }
//
//  trait RecursiveSub extends Recursive[Json, JsObject] {
//    val name = \[String]
//    lazy val child = rec(new \\?("child") with RecursiveSub)
//  }
//
//  object RecursiveContract extends Contract with RecursiveSub
//
//  test("recursive properties") {
//    val hierarchy = JsObject(Map("name" -> JsString("parent"), "child" -> JsObject(Map("name" -> JsString("child"), "child" -> JsObject(Map("name" -> JsString("grandchild")))))))
//    (hierarchy match {
//      case RecursiveContract.name(v) =>
//        v
//    }) should equal ("parent")
//
//    (hierarchy match {
//      case RecursiveContract.child.name(v) =>
//        v
//    }) should equal ("child")
//
//    (hierarchy match {
//      case RecursiveContract.child.child.name(v) =>
//        v
//    }) should equal ("grandChild")
//  }
}
