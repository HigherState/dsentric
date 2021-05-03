package dsentric

import dsentric._
import dsentric.codecs.PessimisticCodecs.booleanCodec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DataEqualityTests extends AnyFunSuite with Matchers {


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
