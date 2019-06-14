package dsentric

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by stefano.paluello on 03/10/2016.
  */
class TypedPathTest  extends FunSuite with Matchers {
  import Dsentric._
  import PessimisticCodecs._

  private val testDObject = DObject(
    "firstString" := "there we are",
    "vector1" := DArray("one", "two", "three"),
    "level1" -> DObject(
      "vector123" := DArray(1),
      "level2" -> DObject(
        "string1" := "stringValue",
        "level3" -> DObject(
          "valInt" := 123
        )
      )
    )
  )

  test("Read a string") {
    val testRes = for {
      res <- testDObject.\[String](Path("firstString"))
    } yield res
    testRes.get should be ("there we are")
  }

  test("read a nested Int value") {
    val path = "level1" \ "level2" \ "level3" \ "valInt"
    val testRes = for {
      res <- testDObject.\[Int](path)
    } yield res
    testRes.get should be (123)
  }

  test("read a DArray as a Vector") {
    val testRes = for {
      res <- testDObject.\[DArray](Path("vector1"))
    } yield res
    val resArray = testRes.get.value
    resArray should be (Vector("one", "two", "three"))
    resArray.length should be (3)
  }

  test("read a DArray as a Vector should return the right data") {
    val testRes = for {
      res <- testDObject.\[DArray](Path("vector1"))
    } yield res
    val resArray = testRes.get.value
    resArray should not be Vector("one", "two")
    resArray.length should not be 2
  }

  test("read a nested DArray as a Vector") {
    val path = "level1" \ "vector123"
    val testRes = for {
      res <- testDObject.\[DArray](path)
    } yield res
    val resArray = testRes.get.value
    resArray should be (Vector(1))
    resArray.length should be (1)
  }

  test("Read a nested DObject") {
    val testRes = for {
      res <- testDObject.\[DObject](Path("level1"))
    } yield res
    testRes.get.isInstanceOf[DObject] should be (true)
  }

  test("Read a more nested DObject") {
    val path = "level1" \ "level2" \ "level3"
    val testRes = for {
      res <- testDObject.\[DObject](path)
    } yield res
    testRes.get.isInstanceOf[DObject] should be (true)
  }



  test("Read a string, wrong Path") {
    val path = "level1" \ "level2" \ "string123"
    val testRes = for {
      res <- testDObject.\[String](path)
    } yield res
    testRes should be (None)
  }

  test("Read an inner DObject, wrong Path") {
    val path = "level1" \ "level2b"
    val testRes = for {
      res <- testDObject.\[DObject](path)
    } yield res
    testRes should be (None)
  }

}
