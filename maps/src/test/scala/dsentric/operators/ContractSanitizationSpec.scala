package dsentric.operators

import dsentric._
import dsentric.codecs.DKeyContractCollectionCodec
import dsentric.contracts.Open
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ContractSanitizationSpec extends AnyFunSpec with Matchers with EitherValues {

  import Dsentric._
  import StandardOperators._
  import dsentric.codecs.std.DCodecs._


  describe("Contract sanitization") {
    describe("When empty") {
      object Empty extends Contract

      it("Should leave empty empty") {
        Empty.$sanitize(DObject.empty) shouldBe DObject.empty
      }
      it("Should leave unsanitized property values alone") {
        Empty.$sanitize(DObject("unsanitized" := false)) shouldBe DObject("unsanitized" := false)
      }
    }

    describe("Maybe Properties") {
      object MaybeSanitize extends Contract {
        val internalProp = \?[String](internal)
        val maskMaybe = \?[DObject](mask("()"))
        val maskEmptyMaybe = \?[String](mask("Hidden", true))
        val maskToMaybe = \?[String](maskTo((s: String) => Some(s.length), Some(0)))
      }

      it("Should set sanitize empty results if empty") {
        MaybeSanitize.$sanitize(DObject.empty) shouldBe DObject("maskEmptyMaybe" := "Hidden", "maskToMaybe" := 0)
      }
      it("Should remove internal property") {
        MaybeSanitize.$sanitize(MaybeSanitize.$create(_.internalProp.$set("value"))) shouldBe DObject("maskEmptyMaybe" := "Hidden", "maskToMaybe" := 0)
      }
      it("Should replace set mask values") {
        MaybeSanitize.$sanitize(MaybeSanitize.$create(c => c.maskMaybe.$set(DObject("value" := 1)) ~ c.maskEmptyMaybe.$set("value"))) shouldBe
          DObject("maskMaybe" := "()", "maskEmptyMaybe" := "Hidden", "maskToMaybe" := 0)
      }
      it("Should apply function mask") {
        MaybeSanitize.$sanitize(MaybeSanitize.$create(c => c.maskToMaybe.$set("String value"))) shouldBe DObject("maskEmptyMaybe" := "Hidden", "maskToMaybe" := 12)
      }
    }

    describe("Default Properties") {
      object DefaultSanitize extends Contract {
        val maskDefault = \![Int](0, mask("***"))
        val maskToDefault = \![String]("value", maskTo((s: String) => Some(s.length), Some(0)))
      }

      it("Empty default values should set mask") {
        DefaultSanitize.$sanitize(DObject.empty) shouldBe DObject("maskDefault" := "***", "maskToDefault" := 5)
      }
      it("Default values should mask") {
        DefaultSanitize.$sanitize(DefaultSanitize.$create(_.maskDefault.$set(2))) shouldBe DObject("maskDefault" := "***", "maskToDefault" := 5)
      }
    }

    describe("Expected object validation") {
      object ExpectedObjectSanitize extends Contract {
        val nested = new \\ {
          val internalMaybe = \?[Boolean](internal)
          val maskExpected = \[String](mask("***"))
          val maskEmptyMaybe = \?[Boolean](mask("***", true))
        }
      }

      it("Should set empty values if object is empty") {
        ExpectedObjectSanitize.$sanitize(DObject.empty) shouldBe DObject("nested" ::= ("maskEmptyMaybe" := "***"))
      }
      it("Should remove internal and set when required") {
        val obj = ExpectedObjectSanitize.$create { c =>
          c.nested.internalMaybe.$set(false) ~
            c.nested.maskExpected.$set("value") ~
            c.nested.maskEmptyMaybe.$set(true)
        }
        ExpectedObjectSanitize.$sanitize(obj) shouldBe DObject("nested" ::= ("maskExpected" := "***", "maskEmptyMaybe" := "***"))
      }
    }

    describe("Maybe object validation") {
      object MaybeObjectSanitize extends Contract {
        val nested = new \\? {
          val internalMaybe = \?[Boolean](internal)
          val maskExpected = \[String](mask("***"))
          val maskEmptyMaybe = \?[Boolean](mask("***", true))
        }
      }

      it("Should not sanitize an object if not found") {
        MaybeObjectSanitize.$sanitize(DObject.empty) shouldBe DObject.empty
      }
      it("Should remove internal and set when required") {
        val obj = MaybeObjectSanitize.$create { c =>
          c.nested.internalMaybe.$set(false) ~
            c.nested.maskExpected.$set("value") ~
            c.nested.maskEmptyMaybe.$set(true)
        }
        MaybeObjectSanitize.$sanitize(obj) shouldBe DObject("nested" ::= ("maskExpected" := "***", "maskEmptyMaybe" := "***"))
      }
    }

    describe("Codec sanitization") {
      object MaybeMaskContract extends Contract {
        val maybeMask = \?[String](mask("***"))
        val maybeInternal = \?[Long](internal)
      }
      object WithContract extends Contract {
        val contractObject = \[DObject](MaybeMaskContract)
      }
      it("Should ignore if empty") {
        WithContract.$sanitize(DObject.empty) shouldBe DObject.empty
      }
      it("Should apply if object set") {
        val obj = DObject("contractObject" ::= ("maybeMask" := "value", "maybeInternal" := 1234))
        val result = DObject("contractObject" ::= ("maybeMask" := "***"))
        WithContract.$sanitize(obj) shouldBe result
      }
    }

    describe("Collection sanitization") {
      describe("Empty object") {
        object EmptyContract extends Contract

        object EmptyObjectsSanitize extends Contract {
          val objects = \[Vector[DObject]](EmptyContract)
          val objectsMask = \[Vector[DObject]](mask("***"))(EmptyContract)
        }

        it("Should be empty if empty") {
          EmptyObjectsSanitize.$sanitize(DObject.empty) shouldBe DObject.empty
        }
        it("Should return objects unchanged") {
          val ob1 = DObject("value" := 3)
          val ob2 = DObject("value" := 5)
          val ob3 = DObject("another" := false)
          val obj = EmptyObjectsSanitize.$create(_.objects.$set(Vector(ob1, ob2, ob3)))

          EmptyObjectsSanitize.$sanitize(obj) shouldBe obj
        }
        it("Should sanitize objects mask") {
          val ob1 = DObject("value" := 3)
          val ob2 = DObject("value" := 5)
          val ob3 = DObject("another" := false)
          val obj = EmptyObjectsSanitize.$create(_.objectsMask.$set(Vector(ob1, ob2, ob3)))
          EmptyObjectsSanitize.$sanitize(obj) shouldBe DObject("objectsMask" := "***")
        }
      }
      describe("With sanitizing properties") {
        object MaybeMaskContract extends Contract {
          val maybeMask = \?[String](mask("***"))
          val maybeInternal = \?[Long](internal)
        }

        object ObjectsSanitize extends Contract {
          val objects = \[Vector[DObject]](MaybeMaskContract)
        }

        it("Should sanitize objects that require sanitizing") {
          val ob1 = DObject("value" := 123)
          val ob2 = MaybeMaskContract.$create(_.maybeInternal.$set(1234L))
          val ob3 = MaybeMaskContract.$create(_.maybeMask.$set("value"))
          val ob4 = MaybeMaskContract.$create(c => c.maybeInternal.$set(1234L) ~ c.maybeMask.$set("value") ~+ ("value" := 123))
          val obj = ObjectsSanitize.$create(_.objects.$set(Vector(ob1, ob2, ob3, ob4)))
          val result = ObjectsSanitize.$create(_.objects.$set(Vector(ob1, DObject.empty, DObject("maybeMask" := "***"), DObject("maybeMask" := "***", "value" := 123))))
          ObjectsSanitize.$sanitize(obj) shouldBe result
        }
      }
    }

    describe("Map objects sanitization") {
      describe("Empty map") {
        object EmptyContract extends Contract

        object EmptyMapSanitize extends Contract {
          val objects = \[Map[String, DObject]](EmptyContract)
          val objectsMask = \[Map[String, DObject]](mask("***"))(EmptyContract)
        }

        it("Should be empty if empty") {
          EmptyMapSanitize.$sanitize(DObject.empty) shouldBe DObject.empty
        }
        it("Should return objects unchanged") {
          val ob1 = DObject("value" := 3)
          val ob2 = DObject("value" := 5)
          val ob3 = DObject("another" := false)
          val obj = EmptyMapSanitize.$create(_.objects.$set(Map("one" -> ob1, "two" -> ob2, "three" -> ob3)))

          EmptyMapSanitize.$sanitize(obj) shouldBe obj
        }
        it("Should sanitize objects mask") {
          val ob1 = DObject("value" := 3)
          val ob2 = DObject("value" := 5)
          val ob3 = DObject("another" := false)
          val obj = EmptyMapSanitize.$create(_.objectsMask.$set(Map("one" -> ob1, "two" -> ob2, "three" -> ob3)))
          EmptyMapSanitize.$sanitize(obj) shouldBe DObject("objectsMask" := "***")
        }
      }
      describe("With sanitizing properties") {
        object MaybeMaskContract extends Contract {
          val maybeMask = \?[String](mask("***"))
          val maybeInternal = \?[Long](internal)
        }

        object MapSanitize extends Contract {
          val objects = \[Map[String, DObject]](MaybeMaskContract)
          val objectObjects = \[Map[String, Map[String, DObject]]](keyValueMapCodec[String, Map[String, DObject]](stringCodec, MaybeMaskContract))
        }

        it("Should sanitize objects that require sanitizing") {
          val ob1 = DObject("value" := 123)
          val ob2 = MaybeMaskContract.$create(_.maybeInternal.$set(1234L))
          val ob3 = MaybeMaskContract.$create(_.maybeMask.$set("value"))
          val ob4 = MaybeMaskContract.$create(c => c.maybeInternal.$set(1234L) ~ c.maybeMask.$set("value") ~+ ("value" := 123))
          val obj = MapSanitize.$create(_.objects.$set(Map("one" -> ob1, "two" -> ob2, "three" -> ob3, "four" -> ob4)))
          val result = MapSanitize.$create(_.objects.$set(Map("one" -> ob1, "two" -> DObject.empty, "three" -> DObject("maybeMask" := "***"), "four" -> DObject("maybeMask" := "***", "value" := 123))))
          MapSanitize.$sanitize(obj) shouldBe result
        }
        it("Should sanitize objectObjects that require sanitizing") {
          val ob1 = DObject("value" := 123)
          val ob2 = MaybeMaskContract.$create(_.maybeInternal.$set(1234L))
          val ob3 = MaybeMaskContract.$create(_.maybeMask.$set("value"))
          val ob4 = MaybeMaskContract.$create(c => c.maybeInternal.$set(1234L) ~ c.maybeMask.$set("value") ~+ ("value" := 123))
          val obj = MapSanitize.$create(_.objectObjects.$set(Map("one" -> Map("one" -> ob1), "two" -> Map("two" -> ob2), "three" -> Map("three" -> ob3, "four" -> ob4))))
          val result = MapSanitize.$create(_.objectObjects.$set(
            Map("one" -> Map("one" -> ob1),
              "two" -> Map("two" -> DObject.empty),
              "three" -> Map(
              "three" -> DObject("maybeMask" := "***"),
              "four" -> DObject("maybeMask" := "***", "value" := 123))
            )))
          MapSanitize.$sanitize(obj) shouldBe result
        }
      }
    }

    describe("Coproduct sanitization") {
      describe("Empty object") {
        object EmptyContract extends Contract

        object EmptyProductSanitize extends Contract {
          val product = \[Either[String, DObject]](EmptyContract)
          val productMask = \[Either[DObject, Int]](mask("***"))(EmptyContract)
        }

        it("Should be empty if empty") {
          EmptyProductSanitize.$sanitize(DObject.empty) shouldBe DObject.empty
        }
        it("Should return product unchanged") {
          val objL = EmptyProductSanitize.$create(_.product.$set(Left("Left")))
          val objR = EmptyProductSanitize.$create(_.product.$set(Right(DObject("test" := 1))))

          EmptyProductSanitize.$sanitize(objL) shouldBe objL
          EmptyProductSanitize.$sanitize(objR) shouldBe objR
        }
        it("Should sanitize product mask for all types") {
          val objL = EmptyProductSanitize.$create(_.productMask.$set(Left(DObject("test" := 1))))
          val objR = EmptyProductSanitize.$create(_.productMask.$set(Right(123)))

          EmptyProductSanitize.$sanitize(objL) shouldBe DObject("productMask" := "***")
          EmptyProductSanitize.$sanitize(objR) shouldBe DObject("productMask" := "***")
        }
      }
      describe("With sanitizing properties") {
        object MaybeMaskContract extends Contract with Open{
          val maybeMask = \?[String](mask("***"))
          val maybeInternal = \?[Long](internal)
        }

        object ProductSanitize extends Contract {
          val product = \[Either[String, DObject]](MaybeMaskContract)
        }

        it("Should sanitize objects that require sanitizing") {
          val objR = ProductSanitize.$create(
            _.product.$set(Right(MaybeMaskContract.$create(c => c.maybeInternal.$set(1234L) ~ c.maybeMask.$set("value") ~+ ("value" := 123)))))
          val objL = ProductSanitize.$create(_.product.$set(Left("value")))

          val resultR = ProductSanitize.$create(_.product.$set(Right(DObject("maybeMask" := "***", "value" := 123))))
          ProductSanitize.$sanitize(objR) shouldBe resultR
          ProductSanitize.$sanitize(objL) shouldBe objL
        }
      }
    }

    describe("Key Contract sanitization") {
      case class KeyedObject(key: String, value: Map[String, Any]) extends DObjectOps[KeyedObject] with DObject {
        override protected def wrap(value: RawObject): KeyedObject = this.copy(value = value)
      }
      case class Store(v: Vector[KeyedObject])
      it("Should sanitize nested contract") {
        case class KeyedObject(key: String, value: Map[String, Any]) extends DObjectOps[KeyedObject] with DObject {
          override protected def wrap(value: RawObject): KeyedObject = this.copy(value = value)
        }
        case class Store(v: Vector[KeyedObject])
        object KeyContract extends ContractFor[KeyedObject] {
          val a = \[String]
          val b = \[String](mask("****"))
        }
        implicit val D = new DKeyContractCollectionCodec[Store, KeyedObject](
          KeyContract,
          (key, value) => Some(KeyedObject(key, value)),
          p => p.key -> p.value,
          v => Some(Store(v)),
          v => v.v
        )
        object KeyedSanitize extends Contract {
          val prop = \[Int]
          val keyedProp = \[Store]
        }
        val content = DObject("prop" := 1, "keyedProp" ::= ("key1" ::= ("a" := "v1", "b" := "v2"), "key2" ::= ("a" := "v3", "b" := "v4")))

        KeyedSanitize.$sanitize(content) shouldBe DObject(
          "prop" := 1, "keyedProp" ::= ("key1" ::= ("a" := "v1", "b" := "****"), "key2" ::= ("a" := "v3", "b" := "****"))
        )
      }
      it("Should ignore no sanitized nested contract") {

        object KeyContract extends ContractFor[KeyedObject] {
          val a = \[String]
          val b = \[String]
        }
        implicit val D = new DKeyContractCollectionCodec[Store, KeyedObject](
          KeyContract,
          (key, value) => Some(KeyedObject(key, value)),
          p => p.key -> p.value,
          v => Some(Store(v)),
          v => v.v
        )
        object KeyedSanitize extends Contract {
          val prop = \[Int]
          val keyedProp = \[Store]
        }
        val content = DObject("prop" := 1, "keyedProp" ::= ("key1" ::= ("a" := "v1", "b" := "v2"), "key2" ::= ("a" := "v3", "b" := "v4")))

        KeyedSanitize.$sanitize(content) shouldBe DObject(
          "prop" := 1, "keyedProp" ::= ("key1" ::= ("a" := "v1", "b" := "v2"), "key2" ::= ("a" := "v3", "b" := "v4"))
        )
      }
    }
  }

}
