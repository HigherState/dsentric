package dsentric.contracts

import dsentric.{DObject, Delta}
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PathSetterTests extends AnyFunSuite with Matchers with EitherValues {

  import dsentric.Dsentric._
  import dsentric.codecs.std.DCodecs._
  object DeltaCheck extends Contract {
    val one = \[String]
    val nested = new \\ {
      val value = \[Int]
    }
  }

  test("Delta $set generation") {
    val testData = DObject("one" := "test", "nested" ::= ("value" := 123))
    DeltaCheck.one.$set("test2").asDelta(testData) shouldBe Delta("one" := "test2")
    DeltaCheck.nested.value.$set(456).asDelta(testData) shouldBe Delta("nested" ::= ("value" := 456))
    (DeltaCheck.one.$set("test3") ~ DeltaCheck.nested.value.$set(456)).asDelta(testData) shouldBe Delta("one" := "test3", "nested" ::= ("value" := 456))
  }

  test("Delta $modify generation") {
    val testData = DObject("one" := "test", "nested" ::= ("value" := 123))
    DeltaCheck.one.$modify(_ + "2").asDelta(testData).value shouldBe Delta("one" := "test2")
    DeltaCheck.nested.value.$modify(_ + 333).asDelta(testData).value shouldBe Delta("nested" ::= ("value" := 456))
    (DeltaCheck.one.$modify(_ + "3") ~ DeltaCheck.nested.value.$modify(_ + 333)).asDelta(testData).value shouldBe Delta("one" := "test3", "nested" ::= ("value" := 456))
  }

}
