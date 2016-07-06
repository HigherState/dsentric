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

  //not an actual test as of yet
  test("Generate query") {
    val query = DQuery("Owner" -> DObject("$elemMatch" := DObject("$regex" := "^jamie.*"))).toOption.get
    val psql = QueryJsonb("Indexed", query)
    println(psql)
  }
}
