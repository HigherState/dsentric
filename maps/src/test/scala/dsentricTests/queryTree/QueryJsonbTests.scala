package dsentricTests.queryTree

import dsentric._
import Dsentric._
import dsentric.queryTree.QueryJsonb
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
    val query = ForceWrapper.dQuery(Map("head" -> Map("string" -> "value1", "dobject" -> Map("value" -> "key", "myNull" -> dNull), "array" -> Vector("value3", "value4"), "boolean" -> true, "integer" -> 1)))
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

}
