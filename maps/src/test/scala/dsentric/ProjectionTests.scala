package dsentric

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
    val nested2 = new \\ {
      val field4 = \[Int]
    }
    val field5 = \[String]
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

    projection.toPaths shouldBe Set(Path("field"), Path("nested","field3"))

  }

  test("Nested dynamic projections") {

    val p = Query1.nested.$$("dynamic1".p, "dynamic2".p, "dynamic3" \ "dynamic4")
    p.toDObject shouldBe DObject("nested" -> DObject("dynamic1" := 1, "dynamic2" := 1, "dynamic3" -> DObject("dynamic4" := 1)))
  }

  test("Omit") {
    val p = Query1.field5.$ & Query1.nested.field3.$
    val result = Query1.$create { c =>
      c.field.$set("one") ~
      c.nested.field2.$set("two") ~
      c.nested.field3.$set(4) ~
      c.nested2.field4.$set(5) ~
      c.field5.$set("three")
    }
    result.omit(p) shouldBe DObject("field" := "one", "nested" -> DObject("field2" :="two"), "nested2" -> DObject("field4" := 5))
    result.omit(p & Query1.nested2.field4.$) shouldBe DObject("field" := "one", "nested" -> DObject("field2" :="two"))
  }

  test("Contains") {
    val p = Query1.field5.$ & Query1.nested.field3.$
    val result = Query1.$create { c =>
      c.field.$set("one") ~
        c.nested.field2.$set("two") ~
        c.nested.field3.$set(4) ~
        c.nested2.field4.$set(5) ~
        c.field5.$set("three")
    }
    result.contains(p) shouldBe true

    val result2 = DObject.empty
    result2.contains(p) shouldBe false

    val result3 =
      Query1.$create { c =>
        c.field.$set("one") ~
          c.nested.field2.$set("two") ~
          c.nested.field3.$set(4) ~
          c.nested2.field4.$set(5)
      }

    result3.contains(p) shouldBe false

    val result4 =
      Query1.$create { c =>
        c.field.$set("one") ~
        c.nested.field2.$set("two") ~
        c.nested2.field4.$set(5) ~
        c.field5.$set("three")
      }

    result4.contains(p) shouldBe false
  }

  test("Intersects") {
    val p = Query1.field5.$ & Query1.nested.field3.$ & Query1.field5.$
    val result = Query1.$create { c =>
      c.field.$set("one") ~
        c.nested.field2.$set("two") ~
        c.nested.field3.$set(4) ~
        c.nested2.field4.$set(5) ~
        c.field5.$set("three")
    }
    result.contains(p) shouldBe true

    val result2 = DObject.empty
    result2.contains(p) shouldBe false

    val result3 =
      Query1.$create { c =>
        c.field.$set("one") ~
          c.nested.field3.$set(4) ~
          c.nested2.field4.$set(5)
      }
    val p2 = Query1.nested.field3.$
    result3.intersects(p2) shouldBe true
    val p3 = Query1.nested.field2.$

    result3.intersects(p3) shouldBe false

    val p4 = Query1.field5.$
    result3.intersects(p4) shouldBe false
  }
}
