package dsentricTests

import dsentric.AndMatcher._
import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractTests extends FunSuite with Matchers {

  import PessimisticCodecs._
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

  object Tuple extends Contract {
    val value = \[(String, Boolean)]
    val optionValue = \[(String, Option[Boolean])]
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

    (DObject("one" := "string", "two" := false) match {
      case Flat.one(s) =>
        s
    }) should equal("string")

    (DObject("one" := "string", "two" := false) match {
      case Flat.one(s) && Flat.two(Some(false)) =>
        s
    }) should equal("string")

    (DObject("one" := "string", "two" := false) match {
      case Flat.two(None) =>
        assert(false)
      case _ =>
        "no match"
    }) should equal("no match")

    (DObject("one" := "string") match {
      case Flat.one(s) && Flat.two(None) && Flat.three(int) =>
        true
    }) should equal(true)

    (DObject("one" := 123) match {
      case Flat.one(s) && Flat.two(None) => s
      case _ => "wrong type"
    }) should equal("wrong type")

    (DObject("two" := false) match {
      case Flat.one(s) && Flat.two(None) => s
      case Flat.two(Some(false)) => "two match"
    }) should equal("two match")

    (DObject("three" := 4) match {
      case Flat.three(i) => i
    }) should equal(4)

    (DObject("three" := "4") match {
      case Flat.three(i) => true
      case _ => false
    }) should equal(false)

    (DObject("two" := "String") match {
      case Flat.two(None) => true
      case _ => false
    }) should equal(false)

    //    (JsObject("two" := JsNull)) match {
    //      case Test.two(None) => true
    //      case _ => false
    //    }) should be(true)

    (DObject("two" := "false") match {
      case Flat.two(Some(i)) => true
      case _ => false
    }) should equal(false)

    (DObject("three" := "not a number") match {
      case Flat.three(i) => i
      case _ => "wrong type"
    }) should equal("wrong type")

    (DObject.empty match {
      case Flat.two(None) => true
      case _ => false
    }) should be (true)
  }

  test("nested Contract pattern matching") {
    (DObject("child" := DObject("value" := 2)) match {
      case Nested.child.value(s) =>
        s
    }) should equal(2)

    (DObject("child" := DObject("value" := 2)) match {
      case Nested.child(j) =>
        j
    }) should equal(DObject("value" := 2))
  }

  test("recursive Contract pattern matching") {
    val obj = DObject("name" := "one", "child" := DObject("name" := "two", "child" := DObject("name" := "three")))
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

    val subObj = DObject("name" := "one", "first" := DObject("name2" := "two", "second" := DObject("name" := "three", "first" := DObject("name2" := "four"))))

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

  test("Type contracts required field") {
    object Existence extends ContractType("req") {
      val req = \[String]
      val value = \[Boolean]
    }
    (DObject("req" := "test") match {
      case Existence.isType() => true
    }) should be (true)

    (DObject("value" := "test") match {
      case Existence.isType() => true
      case _ => false
    }) should be (false)
  }

  test("Type contracts field value match") {
    object Existence extends ContractType("req", "test") {
      val req = \[String]
      val value = \[Boolean]
    }
    (DObject("req" := "test") match {
      case Existence.isType() => true
      case _ => false
    }) should be (true)

    (DObject("req" := "test2") match {
      case Existence.isType() => true
      case _ => false
    }) should be (false)

    (DObject("value" := "test") match {
      case Existence.isType() => true
      case _ => false
    }) should be (false)
  }

  test("name override") {
    object Override extends Contract {
      val _type = \[String]("type")
    }

    (DObject("type" := "value") match {
      case Override._type(t) => t == "value"
    }) should be (true)

    Override.$create(_._type.$set("temp")) should be (DObject("type" := "temp"))
  }

  test("Expected Property") {
    val property = \[Boolean]("property")

    (DObject("property" := true) match {
      case property(t) => t
    }) should be (true)
  }
  test("Expected Property path") {
    val property = \[String]("first" \ "second" \ "third")

    (DObject("first" := DObject("second" := DObject("third" := "value"))) match {
      case property(t) => t == "value"
    }) should be (true)
  }

  test("Default contract array") {
    object Element extends Contract {
      val value = \[Int]
    }
    object ContractArray extends Contract {
      val elements = \:!(Element, Vector.empty)
    }

    (DObject.empty match {
      case ContractArray.elements(elements) =>
        elements.size
    }) should be (0)
  }

  test("tuple codec") {
    val d = DObject("value" := ("one" -> false))

    (d match {
      case Tuple.value(("one", false)) => true
    }) should be (true)

    (DObject("optionValue" := ("one" -> false)) match {
      case Tuple.optionValue(("one", Some(false))) => true
    }) should be (true)

    (DObject("optionValue" := ("one" -> dNull)) match {
      case Tuple.optionValue(("one", None)) => true
    }) should be (true)
  }


  test("Delta maybe matching") {
    object Element extends Contract {
      val value = \?[Int]
    }
    DObject.empty match {
      case Element.value.$delta(d) =>
        d shouldBe None
      case _ =>
        assert(true)
    }

    DObject("value" -> Dsentric.dNull) match {
      case Element.value.$delta(d) =>
        d shouldBe Some(None)
      case _ =>
        assert(false)
    }

    DObject("value" := 4) match {
      case Element.value.$delta(d) =>
        d shouldBe Some(Some(4))
      case _ =>
        assert(false)
    }
  }

  test("Delta default matching") {
    object Element extends Contract {
      val value = \![Int](34)
    }
    DObject.empty match {
      case Element.value.$delta(d) =>
        d shouldBe None
      case _ =>
        assert(true)
    }

    DObject("value" -> Dsentric.dNull) match {
      case Element.value.$delta(d) =>
        d shouldBe Some(None)
      case _ =>
        assert(false)
    }

    DObject("value" := 4) match {
      case Element.value.$delta(d) =>
        d shouldBe Some(Some(4))
      case _ =>
        assert(false)
    }

    DObject.empty match {
      case Element.value.$deltaDefault(d) =>
        d shouldBe None
      case _ =>
        assert(true)
    }

    DObject("value" -> Dsentric.dNull) match {
      case Element.value.$deltaDefault(d) =>
        d shouldBe Some(34)
      case _ =>
        assert(false)
    }

    DObject("value" := 4) match {
      case Element.value.$deltaDefault(d) =>
        d shouldBe Some(4)
      case _ =>
        assert(false)
    }
  }

  test("Delta expected matching") {
    object Element extends Contract {
      val value = \[Int]
    }
    DObject.empty match {
      case Element.value.$delta(d) =>
        d shouldBe None
      case _ =>
        assert(true)
    }

    DObject("value" -> Dsentric.dNull) match {
      case Element.value.$delta(d) =>
        assert(false)
      case _ =>
        assert(true)
    }

    DObject("value" := 4) match {
      case Element.value.$delta(d) =>
        d shouldBe Some(4)
      case _ =>
        assert(false)
    }
  }


}
