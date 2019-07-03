package dsentric.schema

import dsentric.Dsentric._
import dsentric.PessimisticCodecs._
import dsentric._
import org.scalatest.{FunSuite, Matchers}


trait AvroAnotherNested extends SubContract {
  val prop = \[Boolean]
}

object AvroQuery1 extends Contract {
  val field = \?[String]
  val field2 = \[Long]
  val fieldX = \[Float]("actualName")


  val fieldDef = \![Vector[Int]](Vector(1,2,3))

  @Schema(typeName = "Anonymous")
  val nested = new \\? {
    val field2 = \?[String]
  }
  val nested2 = new \\ with AvroAnotherNested

  val nested3 = new \\? with AvroAnotherNested
}

class AvroSchemaTests extends FunSuite with Matchers {
  implicit def renderer =  SimpleRenderer

  test("Existance/nonexistance of field") {

    println(AvroSchema.processContract(Query1))

  }
}
