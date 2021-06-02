package dsentric.schema

import dsentric.Dsentric._
import dsentric._
import namespaced.{AnotherNested, AnotherNested2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


object MsonQuery1 extends Contract {
  import dsentric.codecs.std.DCodecs._
  val field = \?[String]
  val field2 = \[Long]
  val fieldX = \[Float]("actualName")

  @Description("a field")
  val fieldDef = \![Vector[Int]](Vector(1,2,3))

  val nested = new \\? {
    val field2 = \?[String]
  }
  val nested2 = new \\ with AnotherNested

  //val nested3 = new \\? with AnotherNested2
}

class MsonSchemaAnnotationsTests extends AnyFunSuite with Matchers {
  test("Existance/nonexistance of field") {

    //println(MsonSchema.contractRecords(MsonQuery1, Vector.empty).mkString("\n"))

  }
}
