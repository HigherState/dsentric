package dsentric

import java.util.UUID

import cats.data.NonEmptyList
import dsentric._
import org.scalatest.{FunSuite, Matchers}

case class Custom(value:Map[String, Any]) extends DObject with DObjectLike[Custom] {
  protected def wrap(value: Map[String, Any]): Custom =
    Custom(value)
}

case class CustomParams(id:UUID)(val value:Map[String, Any]) extends DObject with DObjectLike[CustomParams] {
  protected def wrap(value: Map[String, Any]): CustomParams =
    CustomParams(id)(value)
}

class DObjectLikeTests extends FunSuite with Matchers {

  import Dsentric._
  import PessimisticCodecs._

  import dsentric.operators.Validators._

  object CustomContract extends ContractFor[Custom] {
    val string = \[String]
    val nested = new \\ {
      val value = \[Int]
    }
  }

  object CustomParamsContract extends ContractFor[CustomParams] {
    val string = \[String]
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
    val mapOfCustom = \->[String, Custom](CustomContract)
  }

  test("Nested custom object") {
    val custom = Custom(Map("string" -> "STRING"))
    val dObject = WithCustomContract.custom.$set(custom)(DObject.empty)

    dObject match {
      case WithCustomContract.custom(c) =>
        c shouldBe custom
    }
  }

  test("Extracting with Custom Params") {
    val customParams = CustomParams(new UUID(123,456))(Map("string" -> "String"))

    customParams match {
      case CustomParams(id) && CustomParamsContract.string(s) =>
        id shouldBe new UUID(123,456)
        s shouldBe "String"
      case _ =>
        assert(false)
    }
  }

  test("Iterator functionality") {
    val data = DObject("one" := 1, "two" := "string")
    val v = data.toVector
    v shouldBe Vector("one" := 1, "two" := "string")
  }

}
