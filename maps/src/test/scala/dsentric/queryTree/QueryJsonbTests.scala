package dsentric.queryTree

import dsentric._
import Dsentric._
import org.scalatest.{FunSuite, Matchers}
import dsentric.PessimisticCodecs._

/**
  * Created by jamie.pullar on 05/07/2016.
  */
class QueryJsonbTests extends FunSuite with Matchers {
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

  test("regex field query") {
    val query = DQuery(
      "$/^test/" -> DObject("$/^level2/" := DObject("element" := DObject("$gte" := 4)))
    ).right.get
    val psql = queryParser("Indexed", query)
    psql shouldBe Right("""EXISTS ( SELECT key, value FROM jsonb_each(Indexed) WHERE key ~ '^test' AND ( EXISTS ( SELECT key, value FROM jsonb_each(value) WHERE key ~ '^level2' AND ( (jsonb_typeof(value #> '{element}') = 'number' AND (value #>> '{element}') :: NUMERIC >= 4) ) ) ) )""")
  }

  test("empty operators") {
    val orQuery = ForceWrapper.dQuery(Map("$or" -> Vector()))
    val orPsql = queryParser("Indexed", orQuery)
    orPsql shouldBe Right("true")

    val andQuery = ForceWrapper.dQuery(Map("$and" -> Vector()))
    val andPsql = queryParser("Indexed", andQuery)
    andPsql shouldBe Right("true")

    val orAndQuery = ForceWrapper.dQuery(Map("$or" -> Vector(Map("$and" -> Vector()), Map("$or" -> Vector()))))
    val orAndPsql = queryParser("Indexed", orAndQuery)
    orAndPsql shouldBe Right("true OR true")
  }



}
