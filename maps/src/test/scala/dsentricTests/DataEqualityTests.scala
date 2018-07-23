package dsentricTests

import dsentric._
import org.scalatest.{FunSuite, Matchers}
import PessimisticCodecs.booleanCodec

class DataEqualityTests extends FunSuite with Matchers {


  test("Data equals") {
    Data(true) shouldBe Data(true)
  }

  test("Compare2Equality should compare boolean Data") {
    val equality = Compare2Equality[Boolean](_ == 0)
    val data1: Data = Data(true)
    val data2: Data = Data(true)

    equality(data1, data2) shouldBe Some(true)
  }
}
