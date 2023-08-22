package dsentric.contracts

import dsentric._
import dsentric.codecs.{
  DContractCodec,
  DCoproductCodec,
  DKeyContractCollectionCodec,
  DParameterisedContractCodec,
  DTypeContractCodec,
  DValueCodec
}
import dsentric.failure.{ClosedContractFailure, _}
import dsentric.schema.ObjectDefinition
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import shapeless.HList

class VerifySpec extends AnyFunSpec with Matchers with EitherValues {
  import dsentric.Dsentric._
  import dsentric.Implicits._

  describe("Value Property behaviour") {
    import dsentric.codecs.std.DCodecs._

    describe("Expected property") {
      object Expected extends Contract {
        val property = \[Int]
      }
      it("Should return failure if empty") {
        val base = DObject.empty
        Expected.$verify(base).left.value should contain(ExpectedFailure(Expected.property))
      }
      it("Should return object if expected set correctly") {
        val base = DObject("property" := 123)
        Expected.$verify(base).value.validObject shouldBe base
      }
      it("Should return incorrect type failure if incorrect type value") {
        val base = DObject("property" := "failed")
        Expected.$verify(base).left.value should contain(IncorrectTypeFailure(Expected.property, "failed"))
      }
      it("Should return incorrect type failure if value null") {
        val base = DObject("property" := DNull)
        Expected.$verify(base).left.value should contain(IncorrectTypeFailure(Expected.property, DNull))
      }
      it("Should return expected type failure if value empty object") {
        val base = DObject("property" := DObject.empty)
        Expected.$verify(base).left.value should contain(IncorrectTypeFailure(Expected.property, RawObject.empty))
      }
    }

    describe("Maybe property") {
      object Maybe extends Contract {
        val property = \?[Int]
      }
      it("Should return empty object if empty") {
        val base = DObject.empty
        Maybe.$verify(base).value.validObject shouldBe base
      }
      it("Should return object if Maybe set correctly") {
        val base = DObject("property" := 123)
        Maybe.$verify(base).value.validObject shouldBe base
      }
      it("Should return incorrect type failure if incorrect type value") {
        val base = DObject("property" := "failed")
        Maybe.$verify(base).left.value should contain(IncorrectTypeFailure(Maybe.property, "failed"))
      }
      it("Should return incorrect type failure if value null") {
        val base = DObject("property" := DNull)
        Maybe.$verify(base).left.value should contain(IncorrectTypeFailure(Maybe.property, DNull))
      }
      it("Should return incorrect type failure if value empty object") {
        val base = DObject("property" := DObject.empty)
        Maybe.$verify(base).left.value should contain(IncorrectTypeFailure(Maybe.property, RawObject.empty))
      }
    }

    describe("Default property") {
      object Default extends Contract {
        val property = \![Int](0)
      }
      it("Should return empty object if empty") {
        val base = DObject.empty
        Default.$verify(base).value.validObject shouldBe base
      }
      it("Should return object if Default set correctly") {
        val base = DObject("property" := 123)
        Default.$verify(base).value.validObject shouldBe base
      }
      it("Should return object if Default set to default value") {
        val base = DObject("property" := 0)
        Default.$verify(base).value.validObject shouldBe base
      }
      it("Should return incorrect type failure if incorrect type value") {
        val base = DObject("property" := "failed")
        Default.$verify(base).left.value should contain(IncorrectTypeFailure(Default.property, "failed"))
      }
      it("Should return incorrect type failure if value is null") {
        val base = DObject("property" := DNull)
        Default.$verify(base).left.value should contain(IncorrectTypeFailure(Default.property, DNull))
      }
      it("Should return incorrect type failure if value is empty object") {
        val base = DObject("property" := DObject.empty)
        Default.$verify(base).left.value should contain(IncorrectTypeFailure(Default.property, RawObject.empty))
      }
    }

    describe("Contract Codec Type") {

      object Nested               extends Contract with Open {
        val expected = \[String]
        val maybe    = \?[Boolean]
        val default  = \![Int](0)
      }
      object Blank                extends Contract with Open
      object ContractCodec        extends Contract           {
        val property = \[DObject](DContractCodec(Nested))
      }
      object MaybeContractCodec   extends Contract           {
        val property = \?[DObject](DContractCodec(Nested))
      }
      object UnusualContractCodec extends Contract           {
        val property = \[DObject](DContractCodec(Blank))
      }
      it("Should return object if contract values correct") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := false))
        ContractCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should return failure if expected object missing") {
        val base = DObject.empty
        ContractCodec.$verify(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return nested failure if expected object is empty") {
        val base = DObject("property" := DObject.empty)
        ContractCodec.$verify(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property", "expected")))

      }
      it("Should return failure if expected object missing is empty, even if empty satisfies contract") {
        val base = DObject.empty
        UnusualContractCodec.$verify(base).left.value should contain(
          ExpectedFailure(UnusualContractCodec, Path("property"))
        )

      }
      it("Should return failure if expected object does not satisfy contract conditions") {
        val base = DObject("property" ::= ("maybe" := false))
        ContractCodec.$verify(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property", "expected")))
      }
      it("Should return failure if maybe object does not satisfy contract conditions") {
        val base = DObject("property" ::= ("maybe" := false))
        MaybeContractCodec.$verify(base).left.value should contain(
          ExpectedFailure(MaybeContractCodec, Path("property", "expected"))
        )
      }

      it("Should return leave null and empty object values") {
        val base = DObject(
          "property" ::= ("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty)
        )
        ContractCodec.$verify(base).value.validObject shouldBe base
      }
    }
    describe("Parameterised Contract Codec Type") {

      case class Parametric(id: Long, _type: String, value: Map[String, Any])
          extends DObject
          with DObjectOps[Parametric] {
        protected def wrap(value: RawObject): Parametric = Parametric(id, _type, value)
      }
      object Parametric extends ContractFor[Parametric] with Closed {
        val expected = \[String]
        val maybe    = \?[Boolean]
        val default  = \![Int](0)
      }

      object ContractCodec extends Contract {
        val property = \[Parametric](DParameterisedContractCodec(Parametric))
      }
      it("Should return object if contract values correct") {
        val base = DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := false))
        ContractCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should return failure if expected object missing") {
        val base = DObject.empty
        ContractCodec.$verify(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return nested failure if expected property is empty") {
        val base = DObject("property" ::= ("id" := 482, "_type" := "test", "maybe" := false))
        ContractCodec.$verify(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property", "expected")))
      }
      it("Should return failure if param is empty") {
        val base = DObject("property" ::= ("id" := 482, "expected" := "value", "maybe" := false))
        ContractCodec.$verify(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property", "_type")))
      }
      it("Should return failure if param is wrong type") {
        val base =
          DObject("property" ::= ("id" := "blah", "expected" := "value", "expected" := "value", "maybe" := false))
        ContractCodec.$verify(base).left.value should contain(
          IncorrectTypeFailure(ContractCodec, Path("property", "id"), longCodec, "blah")
        )
      }
      it("Should fail on extending closed contract") {
        val base =
          DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := true, "add1" := 1234))
        ContractCodec.$verify(base).left.value should contain(
          ClosedContractFailure(ContractCodec, Path("property"), "add1")
        )
      }
    }
    describe("Contract Key Collection Codec Type") {

      case class WithKey(key: String, value: Map[String, Any]) extends DObject {
        protected def wrap(value: RawObject): DObject = this.copy(value = value)
      }
      case class WithKeys(values: Vector[WithKey])
      case class AtleastTwoWithKeys(values: Vector[WithKey])

      object WithKeyContract extends ContractFor[WithKey] with Open {
        val expected = \[String]
        val maybe    = \?[Boolean]
        val default  = \![Int](0)
      }
      object BlankContract   extends ContractFor[WithKey] with Open
      implicit val D: DKeyContractCollectionCodec[WithKeys, WithKey]            = DKeyContractCollectionCodec[WithKeys, WithKey](
        WithKeyContract,
        (k,d) => Some(WithKey(k,d)),
        w => w.key -> w.value,
        a => Some(WithKeys(a)),
        _.values
      )
      implicit val D2: DKeyContractCollectionCodec[AtleastTwoWithKeys, WithKey] =
        DKeyContractCollectionCodec[AtleastTwoWithKeys, WithKey](
          BlankContract,
          (k,d) => Some(WithKey(k,d)),
          w => w.key -> w.value,
          a => if (a.size <= 1) None else Some(AtleastTwoWithKeys(a)),
          _.values
        )

      object ContractCodec           extends Contract {
        val property = \[WithKeys]
      }
      object ContractAtLeastTwoCodec extends Contract {
        val property = \?[AtleastTwoWithKeys]
      }

      it("Should return object if contract values correct") {
        val base = DObject("property" ::= ("one" ::= ("expected" := "value", "maybe" := false)))
        ContractCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should return failure if expected object properties missing") {
        val base = DObject.empty
        ContractCodec.$verify(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return object if expected object is empty") {
        val base = DObject("property" := DObject.empty)
        ContractCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should not reduce key value if expected object is empty and contract allows empty") {
        val base = DObject(
          "property" ::= ("empty" := DObject.empty, "nonEmpty" := DObject("one" := 1), "nonEmpty2" := DObject("two" := 2))
        )
        ContractAtLeastTwoCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should fail if expected object is empty and contract doesnt allow empty") {
        val base = DObject("property" ::= ("empty" := DObject.empty, "nonEmpty" := DObject("one" := 1)))
        ContractCodec.$verify(base).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "empty", "expected"))
        )
      }
      it("Should return failure if expected object does not satisfy contract conditions") {
        val base = DObject("property" ::= ("failure" ::= ("maybe" := "one")))
        ContractCodec.$verify(base).left.value should contain allOf (
          ExpectedFailure(ContractCodec, Path("property", "failure", "expected")),
          IncorrectTypeFailure(ContractCodec, Path("property", "failure", "maybe"), booleanCodec, "one")
        )

      }
      it("Should return failure if collection entity fails") {
        val content = DObject("onlyOne" := DObject("one" := 1))
        val base    = DObject("property" := content)
        ContractAtLeastTwoCodec.$verify(base).left.value should contain(
          IncorrectTypeFailure(ContractAtLeastTwoCodec, Path("property"), D2, content.value)
        )
      }

      it("Should return null and empty object values in contract") {
        val base = DObject(
          "property" ::= ("reducing" ::= ("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty))
        )
        ContractCodec.$verify(base).value.validObject shouldBe base
      }
    }

    describe("Map Codec Type") {
      object Nested        extends Contract with Open {
        val expected = \[String]
        val maybe    = \?[Boolean]
        val default  = \![Int](0)
      }
      object MapCodec      extends Contract           {
        val property = \[Map[Length4String, Int]]

      }
      object MaybeMapCodec extends Contract           {
        val property = \?[Map[Length4String, DObject]](Nested)
      }
      it("Should return object if values are correct") {
        val base = DObject("property" ::= ("key1" := 1234, "key2" := 5678))
        MapCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should return failure if map key does not satisfy contract conditions") {
        val base = DObject("property" ::= ("key25" := 1234, "key2" := 131))
        MapCodec.$verify(base).left.value should contain(
          IncorrectKeyTypeFailure(MapCodec, Path("property"), Length4String.fixedLength4StringCodec, "key25")
        )
      }
      it("Should return failure if map value does not satisfy contract conditions") {
        val base = DObject("property" ::= ("key2" := 1234, "key2" := "bob"))
        MapCodec.$verify(base).left.value should contain(
          IncorrectTypeFailure(MapCodec, Path("property", "key2"), intCodec, "bob")
        )
      }
      it("Should return incorrect type failures for null and empty objects") {
        val base  = DObject("property" ::= ("key1" := DNull))
        MapCodec.$verify(base).left.value should contain(
          IncorrectTypeFailure(MapCodec, Path("property", "key1"), intCodec, DNull)
        )
        val base2 = DObject("property" ::= ("key1" := DObject.empty))
        MapCodec.$verify(base2).left.value should contain(
          IncorrectTypeFailure(MapCodec, Path("property", "key1"), intCodec, RawObject.empty)
        )
      }
      it("Should return failure with contract failures") {
        val base = DObject("property" ::= ("key3" := DObject("maybe" := false)))
        MaybeMapCodec.$verify(base).left.value should contain(
          ExpectedFailure(MaybeMapCodec, Path("property", "key3", "expected"))
        )
      }
      it("Should return full object even if contains null and empty object as long as they are not incorrect types") {
        val base = DObject(
          "property" ::= ("key1" ::= ("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty))
        )
        MaybeMapCodec.$verify(base).value.validObject shouldBe base
      }
    }
    describe("Collection Codec Type") {
      object Nested               extends Contract with Open {
        val expected = \[String]
        val maybe    = \?[Boolean]
        val default  = \![Int](0)
      }
      object CollectionCodec      extends Contract           {
        val property = \[List[Int]]
      }
      object MaybeCollectionCodec extends Contract           {
        val property = \?[List[DObject]](Nested)
      }
      it("Should return object if values are correct") {
        val base = DObject("property" := Vector(1234, 5678))
        CollectionCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should return failure if element does not satisfy contract conditions") {
        val base = DObject("property" := Vector(Data(1234), Data("bob")))
        CollectionCodec.$verify(base).left.value should contain(
          IncorrectTypeFailure(CollectionCodec, "property" :: 1 :: PathEnd, intCodec, "bob")
        )
      }
      it("Should return object if object collection is empty") {
        val base = DObject("property" := Vector[Int]())
        CollectionCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should return failure with contract failures") {
        val base = DObject("property" := Vector(DObject("maybe" := false)))
        MaybeCollectionCodec.$verify(base).left.value should contain(
          ExpectedFailure(MaybeCollectionCodec, "property" :: 0 :: "expected" :: PathEnd)
        )
      }

      it("Should return  null and empty object values in contract") {
        val base = DObject(
          "property" := Vector(
            DObject("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty)
          )
        )
        MaybeCollectionCodec.$verify(base).value.validObject shouldBe base
      }
    }
    describe("Coproduct Codec Type") {
      object Nested              extends Contract with Open {
        val expected = \[String]
        val maybe    = \?[Boolean]
        val default  = \![Int](0)
      }
      object CoproductCodec      extends Contract           {
        val property = \[Either[Int, String]]
      }
      object MaybeCoproductCodec extends Contract           {
        val property = \?[Either[Long, DObject]](Nested)
      }
      it("Should return object if values are correct") {
        val base  = DObject("property" := 123)
        CoproductCodec.$verify(base).value.validObject shouldBe base
        val base2 = DObject("property" := "value")
        CoproductCodec.$verify(base2).value.validObject shouldBe base2
      }
      it("Should return failure if incorrect type") {
        val base = DObject("property" := false)
        CoproductCodec.$verify(base).left.value should contain(
          CoproductTypeValueFailure(
            CoproductCodec,
            CoproductCodec.property._codec.asInstanceOf[DCoproductCodec[Either[Int, String], HList]],
            Path("property"),
            List(
              IncorrectTypeFailure(CoproductCodec, Path("property"), intCodec, false),
              IncorrectTypeFailure(CoproductCodec, Path("property"), stringCodec, false)
            ),
            false
          )
        )
      }

      it("Should null and empty object values in contract") {
        val base = DObject(
          "property" ::= ("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty)
        )
        MaybeCoproductCodec.$verify(base).value.validObject shouldBe base
      }
      it("Should return contract failures") {
        val base = DObject("property" ::= ("maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty))
        MaybeCoproductCodec.$verify(base).left.value should contain(
          CoproductTypeValueFailure(
            MaybeCoproductCodec,
            MaybeCoproductCodec.property._codec.asInstanceOf[DCoproductCodec[Either[Long, DObject], HList]],
            Path("property"),
            List(
              IncorrectTypeFailure(
                MaybeCoproductCodec,
                Path("property"),
                longCodec,
                DObject("maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty).value
              ),
              ExpectedFailure(MaybeCoproductCodec, Path("property", "expected"))
            ),
            DObject("maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty).value
          )
        )
      }
    }
    describe("Type Contract Codec Type") {

      object Type1             extends Contract with Open {
        val _type     = \[String]("type")(DValueCodec.literal("type1"))
        val property1 = \[String]
        val property2 = \?[Int]
      }
      object Type2             extends Contract with Open {
        val _type     = \[String]("type")(DValueCodec.literal("type2"))
        val property1 = \[Int]
        val property2 = \?[String]
      }
      object TypeContractCodec extends Contract           {
        val property = \[DObject](DTypeContractCodec(ObjectDefinition.empty) {
          case Type1._type(_) => Type1
          case Type2._type(_) => Type2
        })
      }
      it("Should return object if target contract satisfied") {
        val base  = DObject("property" := DObject("type" := "type1", "property1" := "value"))
        TypeContractCodec.$verify(base).value.validObject shouldBe base
        val base2 = DObject("property" := DObject("type" := "type2", "property1" := 34532, "property2" := "value"))
        TypeContractCodec.$verify(base2).value.validObject shouldBe base2
      }
      it("Should return failure if target contract not satisfied") {
        val base  = DObject("property" := DObject("type" := "type2", "property1" := "value"))
        TypeContractCodec.$verify(base).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property1"), intCodec, "value")
        )
        val base2 = DObject("property" := DObject("type" := "type1", "property2" := 1234))
        TypeContractCodec.$verify(base2).left.value should contain(
          ExpectedFailure(TypeContractCodec, Path("property", "property1"))
        )
      }
      it("Should return failure if expected property is empty") {
        val base = DObject.empty
        TypeContractCodec.$verify(base).left.value should contain(ExpectedFailure(TypeContractCodec, Path("property")))
      }
      it("Should return resolution failure if expected object is empty") {
        val base = DObject("property" := DObject.empty)
        TypeContractCodec.$verify(base).left.value should contain(
          ContractTypeResolutionFailure(TypeContractCodec, Path("property"), RawObject.empty)
        )
      }
      it("Should fail if target contract cannot be resolved") {
        val base = DObject("property" := DObject("property1" := "value"))
        TypeContractCodec.$verify(base).left.value should contain(
          ContractTypeResolutionFailure(TypeContractCodec, Path("property"), DObject("property1" := "value").value)
        )
      }
      it("Should rreturn null and empty object values in contract") {
        val base = DObject(
          "property" := DObject("type" := "type1", "property1" := "value", "add1" := DNull, "add2" := DObject.empty)
        )
        TypeContractCodec.$verify(base).value.validObject shouldBe base
      }
    }

    describe("Constraints") {
      import dsentric.operators.StandardOperators._

      object Constrained extends Contract {
        val expected = \[String](immutable)
        val maybe    = \?[Int](internal)
        val default  = \![String]("value", mask("******"))

        val expectedObject = new \\?(reserved) {
          val property1 = \[Int]
          val property2 = \?[String]
        }
      }

      it("Should return object if constraint not trigger") {
        val base = DObject("expected" := "value", "default" := "value")
        Constrained.$verify(base).value.validObject shouldBe base
      }
      it("Should ignore  constraint triggered") {
        val base = DObject("expected" := "value", "maybe" := 123, "default" := "******")
        Constrained.$verify(base).value.validObject shouldBe base
      }

      it("Should ignore  if Object constraint triggered") {
        val base = DObject("expected" := "value", "expectedObject" ::= ("property1" := 123, "property2" := "value"))
        Constrained.$verify(base).value.validObject shouldBe base
      }
    }
  }

  describe("Object Property behaviour") {
    import dsentric.codecs.std.DCodecs._

    describe("Expected property") {
      object ExpectedObject      extends Contract {
        val property = new \\ with Open {
          val expected = \[String]
          val maybe    = \?[Int]
        }
      }
      object ExpectedMaybeObject extends Contract {
        val property = new \\ with Open {
          val maybe  = \?[String]
          val maybe2 = \?[Int]
        }
      }
      it("Should return object if expected object satisfies contract") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := 123))
        ExpectedObject.$verify(base).value.validObject shouldBe base
      }
      it("Should fail if object is empty and requires expected properties") {
        val base = DObject.empty
        ExpectedObject.$verify(base).left.value should contain(ExpectedFailure(ExpectedObject.property.expected))
      }
      it("Should fail if expected object is empty and requires expected properties") {
        val base = DObject("property" := DObject.empty)
        ExpectedObject.$verify(base).left.value should contain(ExpectedFailure(ExpectedObject.property.expected))
      }
      it("Should fail on incorrect type if expected property is null") {
        val base = DObject("property" := DNull)
        ExpectedObject.$verify(base).left.value should contain(IncorrectTypeFailure(ExpectedObject.property, DNull))
      }
      it("Should fail if expected object expected properties fail to satisfy conditions") {
        val base = DObject("property" := DObject("expected" := 12341, "maybe" := "fail"))
        ExpectedObject.$verify(base).left.value should contain(
          IncorrectTypeFailure(ExpectedObject.property.expected, 12341)
        )
      }
      it("Should fail if expected object maybe properties fail to satisfy conditions") {
        val base = DObject("property" := DObject("expected" := "value", "maybe" := "fail"))
        ExpectedObject.$verify(base).left.value should contain(
          IncorrectTypeFailure(ExpectedObject.property.maybe, "fail")
        )
      }

      it("Should return object if contains null and empty object property values") {
        val base =
          DObject("property" := DObject("expected" := "value", "additional1" := DNull, "additional" := DObject.empty))
        ExpectedObject.$verify(base).value.validObject shouldBe base
      }
      it("Should return empty if object is empty and requires no expected properties") {
        val base = DObject.empty
        ExpectedMaybeObject.$verify(base).value.validObject shouldBe DObject.empty
      }
      it("Should return object if expected object is empty and requires no expected properties") {
        val base = DObject("property" := DObject.empty)
        ExpectedMaybeObject.$verify(base).value.validObject shouldBe base
      }
      it("Should fail with incorrect type failures if expected object is null") {
        val base = DObject("property" := DNull)
        ExpectedMaybeObject.$verify(base).left.value should contain(
          IncorrectTypeFailure(ExpectedMaybeObject.property, DNull)
        )
      }
    }
    describe("Maybe property") {
      object MaybeObject extends Contract {
        val property = new \\? with Open {
          val expected = \[String]
          val maybe    = \?[Int]
        }
      }
      it("Should return object if maybe object satisfies contract") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := 123))
        MaybeObject.$verify(base).value.validObject shouldBe base
      }
      it("Should return empty if object is empty") {
        val base = DObject.empty
        MaybeObject.$verify(base).value.validObject shouldBe DObject.empty
      }
      it("Should return failure if maybe object is empty and has expected property ") {
        val base = DObject("property" := DObject.empty)
        MaybeObject.$verify(base).left.value should contain(ExpectedFailure(MaybeObject.property.expected))
      }
      it("Should return incorrect type if maybe object is null ") {
        val base = DObject("property" := DNull)
        MaybeObject.$verify(base).left.value should contain(IncorrectTypeFailure(MaybeObject.property, DNull))
      }
      it("Should return failure if maybe object expected properties fail to satisfy conditions") {
        val base = DObject("property" := DObject("expected" := 12341, "maybe" := 134))
        MaybeObject.$verify(base).left.value should contain(IncorrectTypeFailure(MaybeObject.property.expected, 12341))
      }

      it("Should return object if contains null and empty object property values") {
        val base =
          DObject("property" := DObject("expected" := "value", "additional2" := DNull, "additional" := DObject.empty))
        MaybeObject.$verify(base).value.validObject shouldBe base
      }
    }
  }

  describe("Additional Property behaviour") {
    import dsentric.codecs.std.DCodecs._
    describe("Closed object") {
      object Closed extends Contract {
        val property = new \\? {
          val maybe = \?[Int]
        }
      }
      it("Should fail if object contains additional properties") {
        val base = DObject("property" ::= ("additional" := "value"))
        Closed.$verify(base).left.value should contain(ClosedContractFailure(Closed, Path("property"), "additional"))
      }
      it("Should return clsoed failure if object contains additional properties which are null or empty") {
        val base = DObject(
          "property" ::= ("additional" := DNull, "additional2" := DObject.empty, "additional3" := DObject(
            "nested" := DNull
          ))
        )
        Closed.$verify(base).left.value should contain allOf (
          ClosedContractFailure(Closed.property, "additional"),
          ClosedContractFailure(Closed.property, "additional2"),
          ClosedContractFailure(Closed.property, "additional3")
        )
      }
    }
    describe("Open object") {
      object Nested     extends Contract {
        val expected = \[String]
        val maybe    = \?[Boolean]
      }
      object Additional extends Contract {
        val property      = new \\\?[Length4String, Long]()() {
          val maybe = \?[String]
        }
        val codecProperty = new \\\?[String, DObject](DContractCodec(Nested))() {
          val maybe = \?[Int]
        }
      }
      it("Should return fail if additional properties have invalid key ") {
        val base = DObject("property" ::= ("failed" := 1234))
        Additional.$verify(base).left.value should contain(
          IncorrectKeyTypeFailure(Additional, Path("property"), Length4String.fixedLength4StringCodec, "failed")
        )
      }
      it("Should return fail if additional properties have invalid value") {
        val base = DObject("property" ::= ("add1" := "failed"))
        Additional.$verify(base).left.value should contain(
          IncorrectTypeFailure(Additional, Path("property", "add1"), longCodec, "failed")
        )
      }
      it("Should return failure if additional properties have null or reduced values even if key not satisfied") {
        val base = DObject(
          "property" ::= ("additional" := DNull, "additional2" := DObject.empty, "additional3" := DObject(
            "nested" := DNull
          ))
        )
        Additional.$verify(base).left.value should contain allOf (
          IncorrectKeyTypeFailure(Additional, Path("property"), Length4String.fixedLength4StringCodec, "additional"),
          IncorrectTypeFailure(Additional, Path("property", "additional"), longCodec, DNull),
          IncorrectKeyTypeFailure(Additional, Path("property"), Length4String.fixedLength4StringCodec, "additional2"),
          IncorrectTypeFailure(Additional, Path("property", "additional2"), longCodec, Map()),
          IncorrectKeyTypeFailure(Additional, Path("property"), Length4String.fixedLength4StringCodec, "additional3"),
          IncorrectTypeFailure(Additional, Path("property", "additional3"), longCodec, Map("nested" -> DNull))
        )
      }

      it("Should return valid object if additional properties are valid") {
        val base = DObject("property" ::= ("maybe" := "value", "add1" := 1412L))
        Additional.$verify(base).value.validObject shouldBe base
      }
      it("Should return failure if additional property value is satisfied but overlays defined field") {
        val base = DObject("property" ::= ("maybe" := 1412L))
        Additional.$verify(base).left.value should contain(IncorrectTypeFailure(Additional.property.maybe, 1412L))
      }

      it("Should fail if nested contract fails to validate") {
        val base = DObject("codecProperty" ::= ("obj1" ::= ("maybe" := true)))
        Additional.$verify(base).left.value should contain(
          ExpectedFailure(Additional, Path("codecProperty", "obj1", "expected"))
        )
      }
    }
  }
}
