package dsentric.avro

import java.util.UUID

import dsentric.{DObject, _}
import org.scalatest.{FunSuite, Matchers}
import Dsentric._
import PessimisticCodecs._


trait AnotherNested extends SubContract {
  val prop = \[Boolean]
}

object Query1 extends Contract {
  val field = \?[String]
  val field2 = \[Long]
  val fieldX = \[Float]("actualName")


  val fieldDef = \![Vector[Int]](Vector(1,2,3))

  @Avro(typeName = "Anonymous")
  val nested = new \\? {
    val field2 = \?[String]
  }
  val nested2 = new \\ with AnotherNested

  val nested3 = new \\? with AnotherNested
}

class AvroSchemaTests extends FunSuite with Matchers {
  implicit def renderer =  SimpleRenderer

  test("Existance/nonexistance of field") {

    println(Query1.$schema.render)

  }
}
