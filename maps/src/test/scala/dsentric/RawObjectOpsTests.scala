package dsentric

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RawObjectOpsTests extends AnyFunSuite with Matchers {

  import dsentric.codecs.std.DCodecs._
  import Dsentric._

  test("Type structure in collection") {
    val obj         = Delta("one" := 1, "two" := "two")
    val collection  = List(1 -> obj, 2 -> obj)
    val collection2 = collection.map(p => p._1 -> p._2.applyDelta(obj))
    collection.flatMap { _ =>
      collection2
    }
  }

  test("nested value map") {
    val obj = DObject(
      "one" := 1,
      "obj" := DObject("two" := "string", "three" := List(1, 2, 3, 4), "four" := DObject("five" := "string2")),
      "six" := "string3"
    )

    val obj2 = obj.nestedValueMap[String, String] { case value =>
      value + " concat"
    }

    obj2 shouldBe DObject(
      "one" := 1,
      "obj" := DObject(
        "two" := "string concat",
        "three" := List(1, 2, 3, 4),
        "four" := DObject("five" := "string2 concat")
      ),
      "six" := "string3 concat"
    )
  }

  test("nested key value map") {
    val obj = DObject(
      "change1" := 1,
      "array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3, "change3" := "string"))
    )
    val c1  = obj.nestedKeyValueMap[Data, Data] { case ("change1", a) =>
      Some("changed" -> a)
    }
    c1 shouldBe DObject(
      "changed" := 1,
      "array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3, "change3" := "string"))
    )
    obj.nestedKeyValueMap[Boolean, Boolean] { case ("change2", a) =>
      Some("changed" -> !a)
    } shouldBe DObject(
      "change1" := 1,
      "array" := Vector(DObject("changed" := false, "two" := "test"), DObject("three" := 3, "change3" := "string"))
    )

    obj.nestedKeyValueMap[Long, Long] { case ("change3", a) =>
      Some("changed" -> (a + 4))
    } shouldBe obj

    obj.nestedKeyValueMap[Data, Data] {
      case ("change3", _) => None
      case ("change1", _) => None
    } shouldBe DObject("array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3)))
  }

  test("nested key map") {
    val obj = DObject(
      "change1" := 1,
      "array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3, "change3" := "string"))
    )
    val c1  = obj.nestedKeyMap { case "change1" =>
      Some("changed")
    }
    c1 shouldBe DObject(
      "changed" := 1,
      "array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3, "change3" := "string"))
    )
    obj.nestedKeyMap { case "change2" =>
      Some("changed")
    } shouldBe DObject(
      "change1" := 1,
      "array" := Vector(DObject("changed" := true, "two" := "test"), DObject("three" := 3, "change3" := "string"))
    )

    obj.nestedKeyMap {
      case "change3" => None
      case "change1" => None
    } shouldBe DObject("array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3)))
  }
}
