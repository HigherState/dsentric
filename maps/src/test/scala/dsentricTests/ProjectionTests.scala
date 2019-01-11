package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ProjectionTests extends FunSuite with Matchers {
  import Dsentric._
  import PessimisticCodecs._

  object Query1 extends Contract {
    val field = \?[String]
    val nested = new \\ {
      val field2 = \[String]
      val field3 = \?[Int]
    }
  }

  test("Projection selection") {
    val projection = Query1.field.$ & Query1.nested.field2.$
    val result = Query1.$create(c => c.field.$set("one") ~ c.nested.field2.$set("two"))
    val p = projection.select(result)
    p should equal (DObject("field" := "one", "nested" -> DObject("field2" := "two")))

    val noMatch = DObject("value" := 1, "nested" -> DObject("value2" := "test"))
    projection.select(noMatch) should equal (DObject.empty)
  }

  test("Projection to paths") {

    val projection = Query1.field.$ & Query1.nested.field3.$

    projection.toPaths should be (Some(Set(List(Right("field")), List(Right("nested"), Right("field3")))) )

  }

  test("Nested dynamic projections") {

    val p = Query1.nested.$$("dynamic1", "dynamic2", Path("dynamic3", "dynamic4"))
    p.toDObject shouldBe DObject("nested" -> DObject("dynamic1" := 1, "dynamic2" := 1, "dynamic3" -> DObject("dynamic4" := 1)))
  }
}
