package dsentric.queryTree

import dsentric._
import Dsentric._
import dsentric.PessimisticCodecs._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
  * Created by jamie.pullar on 05/07/2016.
  */
class QueryJsonbTests extends AnyFunSuite with Matchers {
  implicit def r:Renderer = SimpleRenderer

  val queryParser = QueryJsonb(_.replace("'","''"))
  //not an actual test as of yet
  test("Generate query") {
    val query = DQuery("Owner" -> DObject("$elemMatch" -> DObject("$regex" := "^jamie.*"))).toOption.get
    val psql = queryParser("Indexed", query)
    println(psql)
  }

  test("Generate query2") {
    val query = ForceWrapper.dQuery(Map("head" -> Map("string" -> "value1", "dobject" -> Map("value" -> "key", "myNull" -> DNull), "array" -> Vector("value3", "value4"), "boolean" -> true, "integer" -> 1)))
    val psql = queryParser("Indexed", query)
    println(psql)
  }

  test("Generate query3") {
    val query = ForceWrapper.dQuery(Map("key1" -> "value1", "key2" -> "value2"))
    val psql = queryParser("Indexed", query)
    println(psql)
  }

  test("Generate query4") {
    val query = ForceWrapper.dQuery(Map("$or" -> Vector(Map("a" -> 1, "b" -> false), Map("b" -> Map("$ne" -> false)))))
    val psql = queryParser("Indexed", query)
    println(psql)
  }

  test("Empty query") {
    val query = ForceWrapper.dQuery(Map("$and" -> Vector(Map("key1" -> "value1", "key2" -> "value2"), Map(), Map("b" -> Map("$ne" -> false)))))
    val psql = queryParser("Indexed", query)
    println(psql)
  }

  test("temp") {
    val query = ForceWrapper.dQuery(Map("Work Country" -> Map("$in" -> Vector("India"))))
    val psql = queryParser("Indexed", query)
    println(psql)
  }

  test("sequence of 'or' serializes correctly") {
    val query = ForceWrapper.dQuery(Map("$or" -> Vector(
      Map("a" -> 1),
      Map("c" -> 1),
      Map("e" -> 1),
      Map("g" -> 1)
    )))
    val psql = queryParser("Indexed", query)
    psql shouldBe Right("""(Indexed @> '{"a":1}'::jsonb OR Indexed @> '{"c":1}'::jsonb OR Indexed @> '{"e":1}'::jsonb OR Indexed @> '{"g":1}'::jsonb)""")
  }

}
