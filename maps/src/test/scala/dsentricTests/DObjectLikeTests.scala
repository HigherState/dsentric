package dsentricTests

import cats.data.NonEmptyList
import dsentric._
import org.scalatest.{FunSuite, Matchers}

case class Custom(value:Map[String, Any]) extends DObject with DObjectLike[Custom] {
  protected def wrap(value: Map[String, Any]): Custom =
    Custom(value)
}

class DObjectLikeTests extends FunSuite with Matchers {

  import Dsentric._
  import PessimisticCodecs._
  object ContractValidatorsImpl extends ContractValidators
  import ContractValidatorsImpl._

  object CustomContract extends ContractFor[Custom] {
    val string = \[String]
    val nested = new \\ {
      val value = \[Int]
    }
  }

  test("Custom crud return custom") {
    val custom = Custom(Map("nested" -> Map("value" -> 4)))
    custom match {
      case CustomContract.nested.value(4) =>
        assert(true)
      case _ =>
        assert(false)
    }
    val newCustom:Custom =
      CustomContract.nested.value.$set(4)(Custom(Map.empty))

    newCustom shouldBe custom

    val dropCustom =
      CustomContract.string.$set("Value") ~
      CustomContract.nested.$forceDrop
  }
  implicit val customCodec =
    DefaultCodecs.dObjectLikeCodec[Custom](Custom)

  object WithCustomContract extends Contract {
    val custom = \[Custom]
    val mapOfCustom = \[Map[String, Custom]](mapContract(CustomContract))
  }

  test("Nested custom object") {
    val custom = Custom(Map("string" -> "STRING"))
    val dObject = WithCustomContract.custom.$set(custom)(DObject.empty)

    dObject match {
      case WithCustomContract.custom(c) =>
        c shouldBe custom
    }
  }

  test("Validation of custom contract") {
    val custom = Custom(Map("string" -> "STRING"))
    val dObject = (WithCustomContract.custom.$set(custom) ~ WithCustomContract.mapOfCustom.$set(Map("first" -> custom)))(DObject.empty)

    WithCustomContract.$validate(dObject) shouldBe Left(NonEmptyList((List(Right("mapOfCustom"), Right("first"), Right("nested")),"Value was expected."), Nil))
  }

}
