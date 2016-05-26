package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ProjectionTests extends FunSuite with Matchers {
  import Dsentric._
  import PessimisticCodecs._

  test("Projection selection") {
    object Query1 extends Contract {
      val field = \?[String]
      val nested = new \\ {
        val field2 = \[String]
        val field3 = \?[Int]
      }
    }
    val projection = Query1.field.$ & Query1.nested.field2.$
    val result = Query1.$create(c => c.field.$set("one") ~ c.nested.field2.$set("two"))
    val p = projection.select(result)
    p should equal (DObject("field" := "one", "nested" -> DObject("field2" := "two")))

    val noMatch = DObject("value" := 1, "nested" -> DObject("value2" := "test"))
    projection.select(noMatch) should equal (DObject.empty)
  }

  test("Projection to paths") {
    object Query1 extends Contract {
      val field = \?[String]
      val nested = new \\ {
        val field2 = \[String]
        val field3 = \?[Int]
      }
    }

    val projection = Query1.field.$ & Query1.nested.field3.$

    projection.toPaths should be (Some(Set(List(Right("field")), List(Right("nested"), Right("field3")))) )

  }
}
