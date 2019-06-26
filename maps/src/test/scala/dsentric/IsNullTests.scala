package dsentric

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class IsNullTests extends FunSuite with Matchers {
  import Dsentric._
  import PessimisticCodecs._

  test("Data.isNull on non-null data") {
    Data(5).isNull should be (false)
    DObject.empty.isNull should be (false)
    DObject(Map("data" -> Data(5))).isNull should be (false)
    DArray.empty.isNull should be (false)
    new DArray(Vector(null)).isNull should be (false)
  }

  test("Data.isNull on null data") {
    dNull.isNull should be (true)
    Data(dNull).isNull should be (true)
    Data(Data(dNull)).isNull should be (true)
    new Data { val value: Any = null }.isNull should be (true)
    new DArray(null).isNull should be (true)
  }

}
