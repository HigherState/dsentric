package dsentric

import dsentric.queryTree.QueryTree
import org.scalatest.{FunSuite, Matchers}

class QueryTests extends FunSuite with Matchers {

  import Dsentric._
  import Query._
  import PessimisticCodecs._

  test("Existance/nonexistance of field") {
    object Query1 extends Contract {
      val field = \?[String]
      val nested = new \\ {
        val field2 = \?[String]
      }
    }
    val query = Query1.field.$exists(true)
    val tree = QueryTree(query)

    query.isMatch(DObject("field" := "value")) should be (true)
    query.isMatch(DObject("field2" := "value")) should be (false)
    tree.isMatch(DObject("field" := "value")) should be (true)
    tree.isMatch(DObject("field2" := "value")) should be (false)

    val query2 = Query1.field.$exists(false) && Query1.nested.field2.$exists(true)
    val tree2 = QueryTree(query2)

    query2.isMatch(DObject("nested" -> DObject("field2" := "value"))) should be (true)
    query2.isMatch(DObject("field" := "value", "nested" -> DObject("field2" := "value"))) should be (false)
    tree2.isMatch(DObject("nested" -> DObject("field2" := "value"))) should be (true)
    tree2.isMatch(DObject("field" := "value", "nested" -> DObject("field2" := "value"))) should be (false)

  }

  test("Equality")   {
    object Query2 extends Contract {
      val field = \?[String]
      val nested = new \\ {
        val field2 = \[Int]
      }
    }
    val query1 = Query2.field.$eq("TEST") || Query2.nested.field2.$eq(45)
    val tree1 = QueryTree(query1)
    query1.isMatch(DObject("field" := "TEST")) should be (true)
    query1.isMatch(DObject.empty) should be (false)
    query1.isMatch(DObject("field" := "TEST2")) should be (false)
    query1.isMatch(DObject("nested" -> DObject("field2" := 45))) should be (true)
    query1.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 45))) should be (true)
    tree1.isMatch(DObject("field" := "TEST")) should be (true)
    tree1.isMatch(DObject.empty) should be (false)
    tree1.isMatch(DObject("field" := "TEST2")) should be (false)
    tree1.isMatch(DObject("nested" -> DObject("field2" := 45))) should be (true)
    tree1.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 45))) should be (true)


    val query3 = Query2(q => q.field.$in("TEST", "TEST2") && q.nested.field2.$nin(4,5,6))
    val tree3 = QueryTree(query3)
    query3.isMatch(DObject("field" := "TEST")) should be (true)
    query3.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 3))) should be (true)
    query3.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 4))) should be (false)
    query3.isMatch(DObject("field" := "TEST3")) should be (false)
    query3.isMatch(DObject("field" := "TEST3", "nested" -> DObject("field2" := 3))) should be (false)
    query3.isMatch(DObject("nested" -> DObject("field2" := 3))) should be (false)
    tree3.isMatch(DObject("field" := "TEST")) should be (true)
    tree3.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 3))) should be (true)
    tree3.isMatch(DObject("field" := "TEST", "nested" -> DObject("field2" := 4))) should be (false)
    tree3.isMatch(DObject("field" := "TEST3")) should be (false)
    tree3.isMatch(DObject("field" := "TEST3", "nested" -> DObject("field2" := 3))) should be (false)
    tree3.isMatch(DObject("nested" -> DObject("field2" := 3))) should be (false)

    //TODO not a generalised solution
    val query4 = Query2.field.$like("value")
    val tree4 = QueryTree(query4)
    query4.isMatch(DObject("field" := "Value")) should be (true)
    query4.isMatch(DObject.empty) should be (false)
    query4.isMatch(DObject("field" := "Values")) should be (false)
    tree4.isMatch(DObject("field" := "Value")) should be (true)
    tree4.isMatch(DObject.empty) should be (false)
    tree4.isMatch(DObject("field" := "Values")) should be (false)

    val query5 = Query2.field.$like("%lue")
    val tree5 = QueryTree(query5)
    query5.isMatch(DObject("field" := "ValuE")) should be (true)
    query5.isMatch(DObject.empty) should be (false)
    query5.isMatch(DObject("field" := "Values")) should be (false)
    tree5.isMatch(DObject("field" := "ValuE")) should be (true)
    tree5.isMatch(DObject.empty) should be (false)
    tree5.isMatch(DObject("field" := "Values")) should be (false)

    val query6 = Query2.field.$regex("vaLUe", "i")
    val tree6 = QueryTree(query6)
    query6.isMatch(DObject("field" := "Value")) should be (true)
    query6.isMatch(DObject.empty) should be (false)
    query6.isMatch(DObject("field" := "Values")) should be (false)
    tree6.isMatch(DObject("field" := "Value")) should be (true)
    tree6.isMatch(DObject.empty) should be (false)
    tree6.isMatch(DObject("field" := "Values")) should be (false)
  }

  test("Long double equality") {
    DQuery("field" := 1L).map(_.isMatch(DObject("field" := 1.00D))) should be (Right(true))
    DQuery("field" := 1L).map(QueryTree(_).isMatch(DObject("field" := 1.00D))) should be (Right(true))
  }

  test("element value match") {
    object Query3 extends Contract {
      val doubles = \[Vector[Long]]
      val nested = new \\ {
        val strings = \?[Vector[String]]
      }
    }

    val query1 = Query3.doubles.$elemMatch(_.$gt(4))

    query1.isMatch(DObject("doubles" := Vector(3, 5))) should be (true)
    query1.isMatch(DObject("doubles" := Vector(2, 4))) should be (false)
    query1.isMatch(DObject("doubles" -> DArray.empty)) should be (false)

    val query2 = Query3.nested.strings.$elemMatch(_.$eq("value"))

    query2.isMatch(DObject("nested" -> DObject("strings" := Vector("value", "test")))) should be (true)
    query2.isMatch(DObject("nested" -> DObject("strings" := Vector("test", "test2")))) should be (false)
  }

  test("element object match") {
    object Query3 extends Contract {
      val objs = \[DArray]
    }
    object Element extends Contract {
      val value = \[String]
    }

    val query1 = Query3.objs.$elemMatch(_ => Element.value.$eq("Test"))

    query1.isMatch(DObject("objs" := DArray(DObject("value" := "Test2"), DObject("value" := "Test")))) should be (true)
    query1.isMatch(DObject("objs" := DArray(DObject("value" := "Test2"), DObject("value" := "Test3")))) should be (false)
    query1.isMatch(DObject("objs" -> DArray.empty)) should be (false)
  }

  test("boolean operators") {
    object Query4 extends Contract {
      val value = \[Double]("value")
    }

    val query1 = Query4.value.$gt(0) || Query4.value.$lt(-10)
    val tree1 = QueryTree(query1)
    query1.isMatch(DObject("value" := 2)) should be (true)
    query1.isMatch(DObject("value" := -3)) should be (false)
    query1.isMatch(DObject("value" := -15)) should be (true)
    tree1.isMatch(DObject("value" := 2)) should be (true)
    tree1.isMatch(DObject("value" := -3)) should be (false)
    tree1.isMatch(DObject("value" := -15)) should be (true)

    val query2 = query1!
    val tree2 = QueryTree(query2)
    query2.isMatch(DObject("value" := 2)) should be (false)
    query2.isMatch(DObject("value" := -3)) should be (true)
    query2.isMatch(DObject("value" := -15)) should be (false)
    tree2.isMatch(DObject("value" := 2)) should be (false)
    tree2.isMatch(DObject("value" := -3)) should be (true)
    tree2.isMatch(DObject("value" := -15)) should be (false)

    val query3 = Query4.value.$gte(0) && Query4.value.$lt(50)
    val tree3 = QueryTree(query3)
    query3.isMatch(DObject("value" := 12)) should be (true)
    query3.isMatch(DObject("value" := -3)) should be (false)
    query3.isMatch(DObject("value" := 50)) should be (false)
    tree3.isMatch(DObject("value" := 12)) should be (true)
    tree3.isMatch(DObject("value" := -3)) should be (false)
    tree3.isMatch(DObject("value" := 50)) should be (false)
  }

  test("contract element match") {
    object Element extends Contract {
      val value = \[Int]
    }
    object Query5 extends Contract {
      val elements = \:(Element)
    }

    val query = Query5.elements.$elemMatch(_.value.$gt(5))
    query.isMatch(DObject("elements" -> DArray.empty)) should be (false)
    query.isMatch(DObject("elements" -> DArray(DObject("value" := 6)))) should be (true)
    query.isMatch(DObject("elements" -> DArray(DObject("value" := 4)))) should be (false)
    query.isMatch(DObject("elements" -> DArray(DObject("value" := 4), DObject("value" := 3), DObject("value" := 7), DObject("value" := 4)))) should be (true)
  }
}
