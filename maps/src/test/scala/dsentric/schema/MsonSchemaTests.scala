package dsentric.schema

import dsentric.Dsentric._
import dsentric.PessimisticCodecs._
import dsentric._
import org.scalatest.{FunSuite, Matchers}


trait MsonAnotherNested extends SubContract {
  val prop = \[Boolean]
}

object MsonQuery1 extends Contract {
  val field = \?[String]
  val field2 = \[Long]
  val fieldX = \[Float]("actualName")


  val fieldDef = \![Vector[Int]](Vector(1,2,3))

  @Schema(typeName = "Anonymous")
  val nested = new \\? {
    val field2 = \?[String]
  }
  val nested2 = new \\ with MsonAnotherNested

  val nested3 = new \\? with MsonAnotherNested
}

class MsonSchemaTests extends FunSuite with Matchers {
  test("Existance/nonexistance of field") {

    println(MsonSchema.processContract(Query1, "\n", "\t"))

  }
}
