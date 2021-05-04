package dsentric

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RawObjectOpsTests extends AnyFunSuite with Matchers {

  import dsentric.codecs.std.DCodecs._
  import Dsentric._


  test("Applying single key delta object value on obj") {
    val obj = DObject("one" := 1, "two" := "two")
    RawObjectOps.rightReduceConcat(obj, DObject.empty) should equal (obj)
    RawObjectOps.rightReduceConcat(obj, DObject("three" := 3)) should equal (obj + ("three" := 3))
    RawObjectOps.rightReduceConcat(obj, DObject("two" := "three")) should equal (DObject("one" := 1, "two" := "three"))
    RawObjectOps.rightReduceConcat(obj, DObject("one" := DNull)) should equal (DObject("two" := "two"))
    RawObjectOps.rightReduceConcat(obj, DObject("two" := DObject.empty)) should equal (DObject("one" := 1))
  }

  test("Type structure in collection") {
    val obj = DObject("one" := 1, "two" := "two")
    val collection = List(1 -> obj, 2 -> obj)
    val collection2 = collection.map(p => p._1 -> p._2.applyDelta(obj))
    collection.flatMap{p =>
      collection2
    }
  }

  test("Applying nested delta object") {
    val obj = DObject("one" := 1, "obj" := DObject("two" := false, "three" := List(1,2,3,4), "four" := DObject("five" := 5)))
    RawObjectOps.rightReduceConcat(obj, DObject("obj" := DObject.empty)) should equal (DObject("one" := 1))
    RawObjectOps.rightReduceConcat(obj, DObject("obj" := DNull)) should equal (DObject("one" := 1))
    RawObjectOps.rightReduceConcat(obj, DObject("obj" := DObject("two" := true))) should equal (DObject("one" := 1, "obj" := DObject("two" := true, "three" := List(1,2,3,4), "four" := DObject("five" := 5))))
    RawObjectOps.rightReduceConcat(obj, DObject("obj" := DObject("three" := DNull))) should equal (DObject("one" := 1, "obj" := DObject("two" := false, "four" := DObject("five" := 5))))
    RawObjectOps.rightReduceConcat(obj, DObject("obj" := DObject("two" := DNull, "three" := DNull, "four" := DNull))) should equal (DObject("one" := 1))
    RawObjectOps.rightReduceConcat(obj, DObject("obj" := DObject("six" := "vi"))) should equal (DObject("one" := 1, "obj" := DObject("two" := false, "three" := List(1,2,3,4), "four" := DObject("five" := 5), "six" := "vi")))
  }

  test("Get difference") {
    val obj = DObject("one" := 1, "obj" := DObject("two" := false, "three" := List(1,2,3,4), "four" := DObject("five" := 5)))
    RawObjectOps.rightDifference(obj, obj) should be (DObject.empty)
    RawObjectOps.rightDifference(obj, DObject.empty) should be (DObject.empty)
    RawObjectOps.rightDifference(obj, DObject("one" := 1)) should be (DObject.empty)
    RawObjectOps.rightDifference(obj, DObject("one" := 1, "obj" := DObject("two" := false))) should be (DObject.empty)
    RawObjectOps.rightDifference(obj, DObject("obj" := DObject("four" := DObject.empty))) should be (DObject.empty)
    RawObjectOps.rightDifference(obj, DObject("obj" := DObject("four" := DObject("five" := 5)))) should be (DObject.empty)

    RawObjectOps.rightDifference(obj, DObject("one" := 2)) should be (DObject("one" := 2))
    RawObjectOps.rightDifference(obj, DObject("six" := 6)) should be (DObject("six" := 6))
    RawObjectOps.rightDifference(obj, DObject("obj" := DObject("four" := DObject("six" := 34.56)))) should be (DObject("obj" := DObject("four" := DObject("six" := 34.56))))
    RawObjectOps.rightDifference(obj, DObject("obj" := DObject("two" := true, "three" := List(1,2,3,4)))) should be (DObject("obj" := DObject("two" := true)))
    RawObjectOps.rightDifference(obj, DObject("obj" := DObject("three" := List(1,2,3,4,5,6)))) should be (DObject("obj" := DObject("three" := List(1,2,3,4,5,6))))
  }

  test("nested value map") {
    val obj = DObject("one" := 1, "obj" := DObject("two" := "string", "three" := List(1,2,3,4), "four" := DObject("five" := "string2")), "six" := "string3")

    val obj2 = obj.nestedValueMap[String,String]{
      case value => value + " concat"
    }

    obj2 shouldBe DObject("one" := 1, "obj" := DObject("two" := "string concat", "three" := List(1,2,3,4), "four" := DObject("five" := "string2 concat")), "six" := "string3 concat")
  }

  test("nested key value map") {
    val obj = DObject("change1" := 1, "array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3, "change3" := "string")))
    val c1 = obj.nestedKeyValueMap[Data, Data]{
      case ("change1", a) => Some("changed" -> a)
    }
    c1 shouldBe DObject("changed" := 1, "array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3, "change3" := "string")))
    obj.nestedKeyValueMap[Boolean, Boolean]{
      case ("change2", a) => Some("changed" -> !a)
    } shouldBe DObject("change1" := 1, "array" := Vector(DObject("changed" := false, "two" := "test"), DObject("three" := 3, "change3" := "string")))

    obj.nestedKeyValueMap[Long, Long]{
      case ("change3", a) => Some("changed" -> (a + 4))
    } shouldBe obj

    obj.nestedKeyValueMap[Data, Data]{
      case ("change3", _) => None
      case ("change1", _) => None
    } shouldBe DObject("array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3)))
  }

  test("nested key map") {
    val obj = DObject("change1" := 1, "array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3, "change3" := "string")))
    val c1 = obj.nestedKeyMap{
      case "change1" => Some("changed")
    }
    c1 shouldBe DObject("changed" := 1, "array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3, "change3" := "string")))
    obj.nestedKeyMap{
      case "change2" => Some("changed")
    } shouldBe DObject("change1" := 1, "array" := Vector(DObject("changed" := true, "two" := "test"), DObject("three" := 3, "change3" := "string")))

    obj.nestedKeyMap{
      case "change3" => None
      case "change1" => None
    } shouldBe DObject("array" := Vector(DObject("change2" := true, "two" := "test"), DObject("three" := 3)))
  }
}
