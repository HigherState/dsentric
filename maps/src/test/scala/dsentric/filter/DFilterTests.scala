package dsentric.filter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import dsentric.{DArray, DObject, Dsentric, Path}

import scala.annotation.nowarn

class DFilterTests extends AnyFunSuite with Matchers {

  import Dsentric._
  import dsentric.filter.DFilterSyntax._
  import dsentric.codecs.std.DCodecs._

  test("Existance/nonexistance of field") {
    object Query1 extends Contract {
      val field  = \?[String]
      val nested = new \\ {
        val field2 = \?[String]
      }
    }
    val query = Query1.field.$exists(true)

    query.isMatch(DObject("field" := "value")) should be(true)
    query.isMatch(DObject("field2" := "value")) should be(false)
    val query2 = Query1.field.$exists(false) && Query1.nested.field2.$exists(true)

    query2.isMatch(DObject("nested" -> DObject("field2" := "value"))) should be(true)
    query2.isMatch(DObject("field" := "value", "nested" ::= ("field2" := "value"))) should be(false)

  }

  test("Equality") {
    object Query2 extends Contract {
      val field  = \?[String]
      val nested = new \\ {
        val field2 = \[Int]
      }
    }
    val query1 = Query2.field.$eq("TEST") || Query2.nested.field2.$eq(45)
    query1.isMatch(DObject("field" := "TEST")) should be(true)
    query1.isMatch(DObject.empty) should be(false)
    query1.isMatch(DObject("field" := "TEST2")) should be(false)
    query1.isMatch(DObject("nested" -> DObject("field2" := 45))) should be(true)
    query1.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 45))) should be(true)

    val query3 = Query2(q => q.field.$in("TEST", "TEST2") && q.nested.field2.$nin(4, 5, 6))
    query3.isMatch(DObject("field" := "TEST")) should be(true)
    query3.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 3))) should be(true)
    query3.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 4))) should be(false)
    query3.isMatch(DObject("field" := "TEST3")) should be(false)
    query3.isMatch(DObject("field" := "TEST3", "nested" -> DObject("field2" := 3))) should be(false)
    query3.isMatch(DObject("nested" -> DObject("field2" := 3))) should be(false)

    //TODO not a generalised solution
    val query4 = Query2.field.$like("value")
    query4.isMatch(DObject("field" := "Value")) should be(true)
    query4.isMatch(DObject.empty) should be(false)
    query4.isMatch(DObject("field" := "Values")) should be(false)

    val query5 = Query2.field.$like("%lue")
    query5.isMatch(DObject("field" := "ValuE")) should be(true)
    query5.isMatch(DObject.empty) should be(false)
    query5.isMatch(DObject("field" := "Values")) should be(false)

    val query6 = Query2.field.$regex("vaLUe", "i")
    query6.isMatch(DObject("field" := "Value")) should be(true)
    query6.isMatch(DObject.empty) should be(false)
    query6.isMatch(DObject("field" := "Values")) should be(false)
  }

  test("Long double equality") {
    DFilter("field" := 1L).isMatch(DObject("field" := 1.00d)) shouldBe true
  }

  test("element value match") {
    object Query3 extends Contract {
      val doubles = \[Vector[Long]]
      val nested  = new \\ {
        val strings = \?[Vector[String]]
      }
    }

    val query1 = Query3.doubles.$elemMatch(_.$gt(4))

    query1.isMatch(DObject("doubles" := Vector(3, 5))) should be(true)
    query1.isMatch(DObject("doubles" := Vector(2, 4))) should be(false)
    query1.isMatch(DObject("doubles" -> DArray.empty)) should be(false)

    val query2 = Query3.nested.strings.$elemMatch(_.$eq("value"))

    query2.isMatch(DObject("nested" -> DObject("strings" := Vector("value", "test")))) should be(true)
    query2.isMatch(DObject("nested" -> DObject("strings" := Vector("test", "test2")))) should be(false)
  }

  test("element object match") {
    object Query3  extends Contract {
      val objs = \[DArray]
    }
    object Element extends Contract {
      val value = \[String]
    }

    val query1 = Query3.objs.$elemMatch(_ => Element.value.$eq("Test"))

    query1.isMatch(DObject("objs" := DArray(DObject("value" := "Test2"), DObject("value" := "Test")))) should be(true)
    query1.isMatch(DObject("objs" := DArray(DObject("value" := "Test2"), DObject("value" := "Test3")))) should be(false)
    query1.isMatch(DObject("objs" -> DArray.empty)) should be(false)
  }

  test("boolean operators") {
    object Query4 extends Contract {
      val value = \[Double]("value")
    }

    val query1 = Query4.value.$gt(0) || Query4.value.$lt(-10)
    query1.isMatch(DObject("value" := 2)) should be(true)
    query1.isMatch(DObject("value" := -3)) should be(false)
    query1.isMatch(DObject("value" := -15)) should be(true)

    val query2 = query1 !

    query2.isMatch(DObject("value" := 2)) should be(false)
    query2.isMatch(DObject("value" := -3)) should be(true)
    query2.isMatch(DObject("value" := -15)) should be(false)

    val query3 = Query4.value.$gte(0) && Query4.value.$lt(50)
    query3.isMatch(DObject("value" := 12)) should be(true)
    query3.isMatch(DObject("value" := -3)) should be(false)
    query3.isMatch(DObject("value" := 50)) should be(false)
  }

  test("contract element match") {
    object Element extends Contract {
      val value = \[Int]
    }
    object Query5  extends Contract {
      val elements = \[Vector[DObject]](Element)
    }

    val query = Query5.elements.$elemMatch(_ => Element.value.$gt(5))
    query.isMatch(DObject("elements" -> DArray.empty)) should be(false)
    query.isMatch(DObject("elements" -> DArray(DObject("value" := 6)))) should be(true)
    query.isMatch(DObject("elements" -> DArray(DObject("value" := 4)))) should be(false)
    query.isMatch(
      DObject(
        "elements" -> DArray(DObject("value" := 4), DObject("value" := 3), DObject("value" := 7), DObject("value" := 4))
      )
    ) should be(true)
  }
  test("Map property match") {

    object Query5 extends Contract {
      val elements = \[Map[String, String]]
    }

    val query = (Query5.elements \ "child").$regex(".*bob.*")
    query.isMatch(DObject("elements" -> DObject.empty)) should be(false)
    query.isMatch(DObject("elements" ::= ("child" := "what bob value"))) should be(true)
    query.isMatch(DObject("elements" ::= ("child" := "fail"))) should be(false)
  }
  test("Map property object match") {
    object Element extends Contract {
      val value = \[Double]
    }
    object Query5  extends Contract {
      val elements = \[Map[String, DObject]](Element)
    }

    val query = (Query5.elements \ "child" \\ Element.value).$gt(45)
    query.isMatch(DObject("elements" -> DObject.empty)) should be(false)
    query.isMatch(DObject("elements" ::= ("child" ::= ("value" := 56.78)))) should be(true)
    query.isMatch(DObject("elements" ::= ("child" ::= ("value" := 33)))) should be(false)

  }

  test("toPaths") {
    @nowarn
    val query = DFilter(
      "first" := 1,
      "$and" := Vector(DObject("second" := 1), DObject("third" ::= ("fourth" := 4))),
      "fifth" ::= ("$not" ::= ("sixth" := false))
    )
    query.toPaths shouldBe Set(Path("first"), Path("second"), Path("third", "fourth"), Path("fifth", "sixth"))
  }
}
