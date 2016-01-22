package dsentric.performance


import argonaut.Json
import dsentric.argonaut.Dsentric._
import dsentric.ApplicativeLens._
import org.scalatest.{FunSuite, Matchers}


class ArgonautTests extends FunSuite with Matchers {

  test("Messing about") {
    val numberList:List[Int] = (1 to 100).toList
    val numberMap = (numberList.map(_.toString) zip numberList).toMap
    val json = ArgonautTestObj.triple.$set(("Test", 1, false))(Json.jEmptyObject)

    /*val result = Timer.print("match") {
      json match {
        case ArgonautTestObj.triple(s, i, b) =>
          (s, i, b)
      }
    }*/

    //TestObj.triple.$set(("string", 3, true))(json) should be (JsObject(Map("int" -> JsNumber(3), "bool" -> JsBool(true), "string" -> JsString("string"))))
  }

}