package dsentricTests.queryTree

import dsentric.Dsentric._
import dsentric.PessimisticCodecs._
import dsentric._
import dsentric.queryTree._
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by jamie.pullar on 05/07/2016.
  */
class QueryTreeTests extends FunSuite with Matchers {
  implicit def r:Renderer = SimpleRenderer

  val queryParser = QueryJsonb(_.replace("'","''"))
  //not an actual test as of yet
  test("Generate element match query tree") {
    val query = DQuery("Owner" -> DObject("$elemMatch" -> DObject("$regex" := "^jamie.*"))).toOption.get
    QueryTree(query).toString should be (∃(Path("Owner"),/(Path.empty,"^jamie.*".r)).toString)
  }

  test("Generate nested query tree") {
    val query = ForceWrapper.dQuery(Map("head" -> Map("string" -> "value1", "dobject" -> Map("value" -> "key", "myNull" -> dNull), "array" -> Vector("value3", "value4"), "boolean" -> true, "integer" -> 1)))
    QueryTree(query) should be (&(List(ϵ(Path("head"),Map("string" -> "value1", "array" -> Vector("value3", "value4"), "boolean" -> true, "integer" -> 1)), ϵ(Path("head", "dobject"),Map("value" -> "key", "myNull" -> dNull)))))
  }

  test("Generate or query tree") {
    val query = ForceWrapper.dQuery(Map("$or" -> Vector(Map("a" -> 1, "b" -> false), Map("b" -> Map("$ne" -> false)))))
    QueryTree(query) should be (|(List(ϵ(Path.empty,Map("a" -> 1, "b" -> false)), ?(Path("b"),"$ne",false))))
  }

  test("simple partition") {
    val query = QueryTree(ForceWrapper.dQuery(Map("value" -> true)))
    query.partition(Set(Path("value"))) should be (Some(?(Path("value"),"$eq",true)) -> None)
    query.partition(Set(Path("value2"))) should be (None -> Some(?(Path("value"),"$eq",true)))

    val query2 = QueryTree(ForceWrapper.dQuery(Map("value" -> true, "value2" -> 123, "value3" -> "text", "value4" -> Map("value5" -> Vector(1,2,3)))))
    query2.partition(Set(Path("value"))) should be (Some(ϵ(Path.empty,Map("value" -> true))) -> Some(&(List(ϵ(Path.empty,Map("value2" -> 123, "value3" -> "text")), ?(Path("value4","value5"),"$eq",Vector(1, 2, 3))))))
    query2.partition(Set(Path("value2"), Path("value4"))) should be (Some(&(List(ϵ(Path.empty, Map("value2" -> 123)), ?(Path("value4", "value5"),"$eq",Vector(1, 2, 3))))) -> Some(ϵ(Path.empty,Map("value" -> true, "value3" -> "text"))))
  }

  test("or partition") {
    val query = QueryTree(ForceWrapper.dQuery(Map("$or" -> Vector(Map("a" -> 1, "b" -> 4), Map("b" -> 2, "c" -> 3), Map("b" -> 4), Map("b" -> 0, "d" -> 5)))))
    query.partition(Set(Path("a"))) should be (None -> Some(|(List(ϵ(Path.empty, Map("a" -> 1, "b" -> 4)), ϵ(Path.empty,Map("b" -> 2, "c" -> 3)), ?(Path("b"),"$eq",4), ϵ(Path.empty,Map("b" -> 0, "d" -> 5))))))
    query.partition(Set(Path("b"))) should be (Some(|(List(ϵ(Path.empty,Map("b" -> 4)), ϵ(Path.empty,Map("b" -> 2)), ?(Path("b"),"$eq",4), ϵ(Path.empty,Map("b" -> 0))))) -> Some(|(List(ϵ(Path.empty, Map("a" -> 1, "b" -> 4)), ϵ(Path.empty,Map("b" -> 2, "c" -> 3)), ?(Path("b"),"$eq",4), ϵ(Path.empty,Map("b" -> 0, "d" -> 5))))))
    query.partition(Set(Path("c"))) should be (None -> Some(|(List(ϵ(Path.empty, Map("a" -> 1, "b" -> 4)), ϵ(Path.empty,Map("b" -> 2, "c" -> 3)), ?(Path("b"),"$eq",4), ϵ(Path.empty,Map("b" -> 0, "d" -> 5))))))
    query.partition(Set(Path("a"), Path("b"), Path("c"), Path("d"))) should be (Some(|(List(ϵ(Path.empty, Map("a" -> 1, "b" -> 4)), ϵ(Path.empty,Map("b" -> 2, "c" -> 3)), ?(Path("b"),"$eq",4), ϵ(Path.empty,Map("b" -> 0, "d" -> 5))))) -> None)
  }

  test("neq partition") {
    val query = QueryTree(ForceWrapper.dQuery(Map("$not" -> Map("a" -> true, "b" -> 3))))
    query.partition(Set(Path("a"))) should be (None -> Some(!!(ϵ(Path.empty,Map("a" -> true, "b" -> 3)))))
    query.partition(Set(Path("b"))) should be (None -> Some(!!(ϵ(Path.empty,Map("a" -> true, "b" -> 3)))))
    query.partition(Set(Path("a"), Path("b"))) should be (Some(!!(ϵ(Path.empty,Map("a" -> true, "b" -> 3)))) -> None)
  }

}
