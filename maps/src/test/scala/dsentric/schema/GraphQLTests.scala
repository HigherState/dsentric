package dsentric.schema

import dsentric.Dsentric._
import dsentric.{Contract, SubContract}
import dsentric.PessimisticCodecs._
import org.scalatest.{FunSuite, Matchers}

trait GQLAnotherNested extends SubContract {
  val prop = \[Boolean]
}

object GQLQuery1 extends Contract {
  val field = \?[String]
  val field2 = \[Long]
  val fieldX = \[Float]("actualName")

  @Schema(typeName = "Anonymous")
  val nested = new \\? {
    val field2 = \?[String]
  }
  val nested2 = new \\ with GQLAnotherNested

  val nested3 = new \\? with GQLAnotherNested
}

class GraphQLTests extends FunSuite with Matchers {

  test("Existance/nonexistance of field") {

    //println(Query1.$schema("\n", "  "))

  }
}
