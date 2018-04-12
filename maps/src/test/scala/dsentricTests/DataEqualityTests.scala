package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}
import PessimisticCodecs.booleanCodec

class DataEqualityTests extends FunSuite with Matchers {

  test("Compare2Equality should compare boolean Data") {
    val equality = Compare2Equality[Boolean](_ == 0)
    val data1: Data = new DTrue
    val data2: Data = new DTrue

    equality(data1, data2) shouldBe Some(true)
  }
}
