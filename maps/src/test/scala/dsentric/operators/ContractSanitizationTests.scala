package dsentric.operators

import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractSanitizationTests extends FunSuite with Matchers with FailureMatchers {

  implicit def strictness = MaybePessimistic
  import Dsentric._
  import PessimisticCodecs._

  object Empty extends Contract

  def f(elems:(Path, String)*) =
    elems.toVector


//  object ToSanitize extends Contract with Validation {
//    val sanitize = \?[String](Validators.internal)
//    val value = \?[Boolean]
//    val nested = new \\ {
//      val sanitize = \?[String](Validators.internal)
//      val value = \?[Int]
//    }
//  }
//
//
//  test("Sanitize data") {
//    val j = DObject("sanitize" := "value", "value" := true, "nested" := DObject("sanitize" := "value", "value" := 123))
//
//    ToSanitize.$sanitize(j) should
//      be (DObject("value" := true, "nested" := DObject("value" := 123)))
//  }

//  object Masking extends Contract with Validation {
//
//    val value = \?[Int](Validators.mask("******"))
//  }
//
//  test("Application of mask") {
//    val obj = DObject("value" := 3)
//    Masking.$sanitize(obj) shouldBe DObject("value" := "******")
//    val obj2 = DObject("value2" := 3)
//    Masking.$sanitize(obj2) shouldBe DObject("value2" := 3)
//  }

}
