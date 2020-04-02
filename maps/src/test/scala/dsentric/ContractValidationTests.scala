package dsentric

import cats.data.NonEmptyList
import dsentric._
import org.scalatest.{FunSuite, Matchers}

class ContractValidationTests extends FunSuite with Matchers with FailureMatchers {

  implicit def strictness = MaybePessimistic
  import Dsentric._
  import PessimisticCodecs._

  object Empty extends Contract

  def s(d:DObject) = Right(d)
  def f(head:(Path, String), tail:(Path, String)*) =
    Left(NonEmptyList[(Path, String)](head, tail.toList))

  def toSet(n:NonEmptyList[(Path, String)]) =
    (n.head :: n.tail).toSet

  test("validation of contract type")  {

    Empty.$validate(DObject.empty) should be (s(DObject.empty))
    Empty.$validate(DObject("key" := "value")) should be (s(DObject("key" := "value")))
  }


  object ExpectedField extends Contract {

    val expGT = \[Int](Validators.>(5))
  }

  test("validation of expected field") {

    ExpectedField.$validate(DObject.empty) should be (f(Path("expGT") -> ValidationText.EXPECTED_VALUE))
    ExpectedField.$validate(DObject("expGT" := false)) should be (f(Path("expGT") -> ValidationText.UNEXPECTED_TYPE))
    ExpectedField.$validate(DObject("expGT" := 7)) should be (s(DObject("expGT" := 7)))
    ExpectedField.$validate(DObject("expGT" := 3)) should be (f(Path("expGT") -> "Value 3 is not greater than 5."))
  }

  object MaybeField extends Contract {
    implicit def strictness = MaybePessimistic
    val mayNonEmpty = \?[String](Validators.nonEmptyOrWhiteSpace)

  }

  test("validation of optional field") {
    MaybeField.$validate(DObject.empty) should be (s(DObject.empty))
    MaybeField.$validate(DObject("mayNonEmpty" := false)) should be (f(Path("mayNonEmpty") -> ValidationText.UNEXPECTED_TYPE))
    MaybeField.$validate(DObject("mayNonEmpty" := "TEST")) should be (s(DObject("mayNonEmpty" := "TEST")))
    MaybeField.$validate(DObject("mayNonEmpty" := "")) should be (f(Path("mayNonEmpty") -> "String must not be empty or whitespace."))
  }

  object DefaultField extends Contract {
    implicit def strictness = MaybePessimistic
    val inDefault = \![String]("default", Validators.in("default", "one", "two"))
  }

  test("validation of default field") {
    DefaultField.$validate(DObject.empty) should be (s(DObject.empty))
    DefaultField.$validate(DObject("inDefault" := false)) should be (f(Path("inDefault") -> ValidationText.UNEXPECTED_TYPE))
    DefaultField.$validate(DObject("inDeafult" := "two")) should be (s(DObject("inDeafult" := "two")))
    DefaultField.$validate(DObject("inDefault" := "three")) should be (f(Path("inDefault") -> "'three' is not an allowed value."))
  }

  object NestedExpectedField extends Contract {

    val nested = new \\{
      val expected = \[String]
    }
  }

  test("validation of expected nested contract") {
    NestedExpectedField.$validate(DObject.empty) should be (f(Path("nested") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(DObject("nested" := DObject.empty)) should be (f(Path("nested", "expected") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(DObject("nested" := DObject("expected" := "value"))) should be (s(DObject("nested" := DObject("expected" := "value"))))
  }

  object NestedMaybeField extends Contract {
    val nested = new \\?{
      val expected = \[String]
    }
  }

  test("validation of maybe nested contract") {
    NestedMaybeField.$validate(DObject.empty) should be (s(DObject.empty))
    NestedMaybeField.$validate(DObject("nested" := DObject.empty)) should be (f(Path("nested", "expected") -> ValidationText.EXPECTED_VALUE))
    NestedExpectedField.$validate(DObject("nested" := DObject("expected" := "value"))) should be (s(DObject("nested" := DObject("expected" := "value"))))
  }

  test("Nested validation") {
    implicit def strictness = MaybePessimistic
    object NestValid extends Contract {
      val value1 = \[String]
      val nest1 = new \\ {
        val value2 = \[String]
        val value3 = \[String]
      }
      val nest2 = new \\? {
        val nest3 = new \\ {
          val value4 = \[String]
        }
        val value5 = \[String]
      }
    }

    val json1 = DObject("value1" := "V", "nest1" := DObject("value2" := "V", "value3" := "V"))
    NestValid.$validate(json1) should be (s(json1))
    val json2 = DObject("value1" := "V", "nest1" := DObject("value2" := "V", "value3" := "V"), "nest2" := DObject("nest3" := DObject("value4" := "V"), "value5" := "V"))
    NestValid.$validate(json2) should be (s(json2))

    NestValid.$validate(DObject("value1" := "V", "nest1" := DObject("value3" := 3))).left.map(toSet) should
      be (Left(Set("nest1"\"value2" -> "Value was expected.", "nest1"\"value3" -> "Value is not of the expected type.")))

    NestValid.$validate(DObject("value1" := "V", "nest2" := DObject.empty)).left.map(toSet) should
      be (Left(Set(Path("nest1") -> "Value was expected." , "nest2"\"nest3" -> "Value was expected.", "nest2"\"value5" -> "Value was expected.")))
  }

  object ToSanitize extends Contract {
    val sanitize = \?[String](Validators.internal)
    val value = \?[Boolean]
    val nested = new \\ {
      val sanitize = \?[String](Validators.internal)
      val value = \?[Int]
    }
  }


  test("Sanitize data") {
    val j = DObject("sanitize" := "value", "value" := true, "nested" := DObject("sanitize" := "value", "value" := 123))

    ToSanitize.$sanitize(j) should
      be (DObject("value" := true, "nested" := DObject("value" := 123)))
  }

  object ContractArray extends Contract {
    val array = \:(ExpectedField)
  }

  test("Contract array validation") {
    ContractArray.$validate(DObject.empty) should be (f(Path("array") -> ValidationText.EXPECTED_VALUE))
    ContractArray.$validate(DObject("array" -> DArray.empty)) should be (s(DObject("array" -> DArray.empty)))

    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) should be (s(DObject("array" -> DArray(DObject("expGT" := 6)))))
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))) should be (s(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))))
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 4)))) should be (f("array" \ 0 \ "expGT" -> "Value 4 is not greater than 5."))
    ContractArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 4)))) should be (f("array" \ 1 \ "expGT" -> "Value 4 is not greater than 5."))
  }

  object ContractArrayNonEmpty extends Contract {
    val array  = \:(ExpectedField, Validators.nonEmpty)
  }

  test("Contract array nonEmpty validation") {
    ContractArrayNonEmpty.$validate(DObject.empty) should be (f(Path("array") -> ValidationText.EXPECTED_VALUE))
    ContractArrayNonEmpty.$validate(DObject("array" -> DArray.empty)) should be (f(Path("array") -> "Value must not be empty."))
    ContractArrayNonEmpty.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) should be (s(DObject("array" -> DArray(DObject("expGT" := 6)))))
  }

  object ContractMaybeArray extends Contract {
    val array = \:?(ExpectedField)
  }

  test("Contract maybe array validation") {
    ContractMaybeArray.$validate(DObject.empty) should be (s(DObject.empty))
    ContractMaybeArray.$validate(DObject("array" -> DArray.empty)) should be (s(DObject("array" -> DArray.empty)))

    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6)))) should be (s(DObject("array" -> DArray(DObject("expGT" := 6)))))
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))) should be (s(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 8)))))
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 4)))) should be (f("array" \ 0 \ "expGT" -> "Value 4 is not greater than 5."))
    ContractMaybeArray.$validate(DObject("array" -> DArray(DObject("expGT" := 6), DObject("expGT" := 4)))) should be (f("array" \ 1 \ "expGT" -> "Value 4 is not greater than 5."))
  }

  object Multi extends Contract {
    val id = \[String]("id" \ "path")
    val reserved = \?[String](Validators.reserved)
    val default = \![Boolean](false, Validators.reserved)
  }

  test("multi validators") {
    Multi.$validate(DObject("id" -> DObject("path" := "testId"))) should be (s(DObject("id" -> DObject("path" := "testId"))))

  }


  object Element extends Contract {
    val id = \[Int]
    val name = \?[String]
  }

  object Parent extends Contract {
    val elements = \[Map[String, DObject]](Validators.mapContract(Element))
  }

  test("map contract validator") {

    val succ = DObject("elements" := Map("first" -> DObject("id" := 4), "second" -> DObject("id" := 2, "name" := "bob")))
    Parent.$validate(succ) should be (s(succ))

    val failfirst = DObject("elements" := Map("first" -> DObject(), "second" -> DObject("id" := 2, "name" := "bob")))
    Parent.$validate(failfirst) should be (f(Path("elements", "first", "id") -> "Value was expected."))

    val failSecond = DObject("elements" := Map("first" -> DObject("id" := 4), "second" -> DObject("id" := 2, "name" := false)))
    Parent.$validate(failSecond) should be (f(Path("elements", "second", "name") -> "Value is not of the expected type."))

    val failBoth = DObject("elements" := Map("first" -> DObject(), "second" -> DObject("id" := 2, "name" := false)))
    Parent.$validate(failBoth) should be (f(Path("elements", "first", "id") -> "Value was expected.",Path("elements", "second", "name") -> "Value is not of the expected type."))
  }

  test("delta validation") {
    val current = Element.$create(e => e.id.$set(1) ~ e.name.$set("Value"))
    val delta = Element.$create(_.name.$setNull)
    Element.$validate(delta, current) should be (s(delta))
  }

  object Keys extends Contract {

    val int = \?[Int](Validators.>(5))
    val nested = new \\?(Validators.keyValidator("[a-z]*".r, "Invalid key"))
    val map = \[Map[String, String]]
  }

  test("Null validation") {
    val all = DObject("int" := 10, "nested" -> DObject("values" := 1), "map" := Map("key" := "value") )
    Keys.$validate(DObject("int" := DNull), all) shouldBe Right(DObject("int" := DNull))
    Keys.$validate(DObject("nested" := DNull), all) shouldBe Right(DObject("nested" := DNull))
    Keys.$validate(DObject("nested" -> DObject("values" := DNull)), Some(all)) shouldBe Right(DObject("nested" -> DObject("values" := DNull)))
    Keys.$validate(DObject("map" -> DObject("key" := DNull)), Some(all)) shouldBe Right(DObject("map" -> DObject("key" := DNull)))
  }

  object Appending extends Contract {

    val map = \?[Map[String, String]](Validators.noKeyRemoval)
  }

  test("No key removal only") {
    val obj = DObject("map" := Map("key" := "value") )
    Appending.$validate(DObject("map" := Map("Key" := "Value2")), Some(obj)) shouldBe Right(DObject("map" := Map("Key" := "Value2")))
    Appending.$validate(DObject("map" := Map("Key2" := "Value2")), Some(obj)) shouldBe Right(DObject("map" := Map("Key2" := "Value2")))
    Appending.$validate(DObject("map" := Map("Key2" := DNull)), Some(obj)) should not be Right(DObject("map" := Map("Key2" := DNull)))
  }

  object Masking extends Contract {

    val value = \?[Int](Validators.mask("******"))
  }

  test("Application of mask") {
    val obj = DObject("value" := 3)
    Masking.$sanitize(obj) shouldBe DObject("value" := "******")
    val obj2 = DObject("value2" := 3)
    Masking.$sanitize(obj2) shouldBe DObject("value2" := 3)
  }

  object WithSet extends Contract {
    val set = \?[Set[String]](Validators.minLength[String](2))
    val list = \?[List[String]](Validators.nonEmpty[String])
    val map = \?[Map[String, Int]](Validators.maxLength[String](5))
    val string = \?[String](Validators.nonEmpty)
  }

  test("validate a set") {
    val withSet = WithSet.$create(_.set.$set(Set("a", "b", "c")))
    val delta1 = DObject("set" := Map("b" := DNull, "d" := 1))
    val delta2 = DObject("set" := Map("b" := DNull, "c" := DNull))
    WithSet.$validate(withSet) shouldBe Right(DObject("set" := Map("a" := 1, "b" := 1, "c" := 1)))
    WithSet.$validate(delta1, withSet) shouldBe Right(delta1)
    withSet.applyDelta(delta1) shouldBe DObject("set" := Map("a" := 1, "c" := 1, "d" := 1))
    WithSet.$validate(delta2, withSet) shouldBe Right(delta2)
  }





  object DMap extends Contract {
    val map = \?[Map[String, DObject]]
  }


  test("Validate a map of DObjects") {
    val map = Map("one" -> DObject("one" := 1))
    val obj = DMap.$create(_.map.$set(map))
    DMap.$validate(obj).isRight shouldBe true

    DMap.$validate(DObject("map" -> DObject("two" -> DObject("one" := 1))), obj).isRight shouldBe true

    DMap.$validate(DObject("map" -> DObject("one" := DNull)), obj).isRight shouldBe true

    DMap.$validate(DObject("map" -> DNull), obj).isRight shouldBe true

    DMap.$validate(DObject("map" -> DObject("one" := 123)), obj).isRight shouldBe false
  }

}
