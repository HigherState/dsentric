package dsentric

import org.scalatest.{FunSuite, Matchers}

import Dsentric._
import PessimisticCodecs._
import Query._
import dsentric.graphQl._

//@GraphQl(typeName = "MyNested")
trait AnotherNested extends SubContract {
  val prop = \[Boolean]
}

//@GraphQl(typeName = "MyQuery")
object Query1 extends Contract {
  val field = \?[String]
  val field2 = \[Long]
  val fieldX = \[Float]("actualName")

  @GraphQl(typeName = "Anonymous")
  val nested = new \\? {
    val field2 = \?[String]
  }
  val nested2 = new \\ with AnotherNested

  val nested3 = new \\? with AnotherNested
}

class GraphQLTests extends FunSuite with Matchers {

  test("Existance/nonexistance of field") {

    //println(Query1.$schema("\n", "  "))

  }
}
