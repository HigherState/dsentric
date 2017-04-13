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
    val query = DQuery("Owner" -> DObject("$elemMatch" := DObject("$regex" := "^jamie.*"))).toOption.get
    val psql = queryParser("Indexed", query)
    println(psql)
  }

  test("Generate query2") {
    val query = DQuery(Map("string" -> "value1", "dobject" -> DObject("value" -> Data("key"), "myNull" -> new DNull), "array" -> DArray("value3", "value4"), "boolean" -> true, "integer" -> 1)).toOption.get
    val psql = queryParser("Indexed", query)
    println(psql)
  }
}
