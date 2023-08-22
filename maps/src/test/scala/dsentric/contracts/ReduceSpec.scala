package dsentric.contracts

import dsentric.codecs.{
  DContractCodec,
  DCoproductCodec,
  DKeyContractCollectionCodec,
  DParameterisedContractCodec,
  DTypeContractCodec,
  DValueCodec
}
import dsentric.failure._
import dsentric.schema.ObjectDefinition
import dsentric._
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import shapeless.HList

class ReduceSpec extends AnyFunSpec with Matchers with EitherValues {
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
        Expected.$reduce(base).left.value should contain(ExpectedFailure(Expected.property))
        Expected.$reduce(base, true).left.value should contain(ExpectedFailure(Expected.property))
      }
      it("Should return object if expected set correctly") {
        val base = DObject("property" := 123)
        Expected.$reduce(base).value shouldBe base
        Expected.$reduce(base, true).value shouldBe base
      }
      it("Should return incorrect type failure if incorrect type value") {
        val base = DObject("property" := "failed")
        Expected.$reduce(base).left.value should contain(IncorrectTypeFailure(Expected.property, "failed"))
        Expected.$reduce(base, true).left.value should contain(IncorrectTypeFailure(Expected.property, "failed"))
      }
      it("Should return expected type failure if value null") {
        val base = DObject("property" := DNull)
        Expected.$reduce(base).left.value should contain(ExpectedFailure(Expected.property))
        Expected.$reduce(base, true).left.value should contain(ExpectedFailure(Expected.property))
      }
      it("Should return expected type failure if value empty object") {
        val base = DObject("property" := DObject.empty)
        Expected.$reduce(base).left.value should contain(ExpectedFailure(Expected.property))
        Expected.$reduce(base, true).left.value should contain(ExpectedFailure(Expected.property))
      }
    }

    describe("Maybe property") {
      object Maybe extends Contract {
        val property = \?[Int]
      }
      it("Should return empty object if empty") {
        val base = DObject.empty
        Maybe.$reduce(base).value shouldBe base
        Maybe.$reduce(base, true).value shouldBe base
      }
      it("Should return object if Maybe set correctly") {
        val base = DObject("property" := 123)
        Maybe.$reduce(base).value shouldBe base
        Maybe.$reduce(base, true).value shouldBe base
      }
      it("Should return incorrect type failure if incorrect type value") {
        val base = DObject("property" := "failed")
        Maybe.$reduce(base).left.value should contain(IncorrectTypeFailure(Maybe.property, "failed"))
      }
      it("Should return empty object if incorrect type value with DropBadTypes is true") {
        val base = DObject("property" := "failed")
        Maybe.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty object  if value null") {
        val base = DObject("property" := DNull)
        Maybe.$reduce(base).value shouldBe DObject.empty
        Maybe.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty object if value empty object") {
        val base = DObject("property" := DObject.empty)
        Maybe.$reduce(base).value shouldBe DObject.empty
        Maybe.$reduce(base, true).value shouldBe DObject.empty
      }
    }

    describe("Default property") {
      object Default extends Contract {
        val property = \![Int](0)
      }
      it("Should return empty object if empty") {
        val base = DObject.empty
        Default.$reduce(base).value shouldBe base
        Default.$reduce(base, true).value shouldBe base
      }
      it("Should return object if Default set correctly") {
        val base = DObject("property" := 123)
        Default.$reduce(base).value shouldBe base
        Default.$reduce(base, true).value shouldBe base
      }
      it("Should return object if Default set to default value") {
        val base = DObject("property" := 0)
        Default.$reduce(base).value shouldBe base
        Default.$reduce(base, true).value shouldBe base
      }
      it("Should return incorrect type failure if incorrect type value") {
        val base = DObject("property" := "failed")
        Default.$reduce(base).left.value should contain(IncorrectTypeFailure(Default.property, "failed"))
      }
      it("Should return empty object if incorrect type value with DropBadTypes is true") {
        val base = DObject("property" := "failed")
        Default.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty object if value null") {
        val base = DObject("property" := DNull)
        Default.$reduce(base).value shouldBe DObject.empty
        Default.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty object if value empty object") {
        val base = DObject("property" := DObject.empty)
        Default.$reduce(base).value shouldBe DObject.empty
        Default.$reduce(base, true).value shouldBe DObject.empty
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
        ContractCodec.$reduce(base).value shouldBe base
        ContractCodec.$reduce(base, true).value shouldBe base
      }
      it("Should return failure if expected object missing") {
        val base = DObject.empty
        ContractCodec.$reduce(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
        ContractCodec.$reduce(base, true).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return failure if expected object is empty") {
        val base = DObject("property" := DObject.empty)
        ContractCodec.$reduce(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
        ContractCodec.$reduce(base, true).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return failure if expected object missing is empty, even if empty satisfies contract") {
        val base = DObject.empty
        UnusualContractCodec.$reduce(base).left.value should contain(
          ExpectedFailure(UnusualContractCodec, Path("property"))
        )
        UnusualContractCodec.$reduce(base, true).left.value should contain(
          ExpectedFailure(UnusualContractCodec, Path("property"))
        )
      }
      it("Should return failure if expected object does not satisfy contract conditions") {
        val base = DObject("property" ::= ("maybe" := false))
        ContractCodec.$reduce(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property", "expected")))
        ContractCodec.$reduce(base, true).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "expected"))
        )
      }
      it("Should return failure if maybe object does not satisfy contract conditions") {
        val base = DObject("property" ::= ("maybe" := false))
        MaybeContractCodec.$reduce(base).left.value should contain(
          ExpectedFailure(MaybeContractCodec, Path("property", "expected"))
        )
      }
      it("Should return empty if current empty if delta does not satisfy contract conditions and contract is maybe under dropBadTypes is true") {
        val base = DObject("property" ::= ("maybe" := false))
        MaybeContractCodec.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return reduce if maybe and default contract properties are wrong type under dropBadTypes is true") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := 123, "default" := false))
        ContractCodec.$reduce(base, true).value shouldBe DObject("property" ::= ("expected" := "value"))
      }
      it("Should return reduce null and empty object values in contract") {
        val base = DObject(
          "property" ::= ("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty)
        )
        ContractCodec.$reduce(base).value shouldBe DObject(
          "property" ::= ("expected" := "value", "maybe" := false, "default" := 0)
        )
        ContractCodec.$reduce(base, true).value shouldBe DObject(
          "property" ::= ("expected" := "value", "maybe" := false, "default" := 0)
        )
      }
    }
    describe("Contract Parameterised Codec Type") {

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
        ContractCodec.$reduce(base).value shouldBe base
        ContractCodec.$reduce(base, true).value shouldBe base
      }
      it("Should return failure if expected object missing") {
        val base = DObject.empty
        ContractCodec.$reduce(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
        ContractCodec.$reduce(base, true).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return failure if contract expected property is empty") {
        val base = DObject("property" ::= ("id" := 482, "_type" := "test", "maybe" := false))
        ContractCodec.$reduce(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property", "expected")))
        ContractCodec.$reduce(base, true).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return failure if param is empty") {
        val base = DObject("property" ::= ("id" := 482, "expected" := "value", "maybe" := false))
        ContractCodec.$reduce(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property", "_type")))
        ContractCodec.$reduce(base, true).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return failure if param is null") {
        val base = DObject("property" ::= ("id" := DNull, "_type" := "test", "expected" := "value", "maybe" := false))
        ContractCodec.$reduce(base).left.value should contain(
          IncorrectTypeFailure(ContractCodec, Path("property", "id"), longCodec, DNull)
        )
        ContractCodec.$reduce(base, true).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return reduce if maybe and default contract properties are wrong type under dropBadTypes is true") {
        val base = DObject(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := 123, "default" := false)
        )
        ContractCodec.$reduce(base, true).value shouldBe DObject(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value")
        )
      }
      it("Should return reduce null and empty object values in contract") {
        val base = DObject(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "maybe" := DNull, "default" := 0)
        )
        ContractCodec.$reduce(base).value shouldBe DObject(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "default" := 0)
        )
        ContractCodec.$reduce(base, true).value shouldBe DObject(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "default" := 0)
        )
      }
      it("Should return reduce additional null additional properties values in contract") {
        val base =
          DObject("property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "add1" := DNull, "default" := 0))
        ContractCodec.$reduce(base).value shouldBe DObject(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "default" := 0)
        )
        ContractCodec.$reduce(base, true).value shouldBe DObject(
          "property" ::= ("id" := 482, "_type" := "test", "expected" := "value", "default" := 0)
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
      object MaybeContractCodec      extends Contract {
        val property = \?[WithKeys]
      }
      object ContractAtLeastTwoCodec extends Contract {
        val property = \?[AtleastTwoWithKeys]
      }

      it("Should return object if contract values correct") {
        val base = DObject("property" ::= ("one" ::= ("expected" := "value", "maybe" := false)))
        ContractCodec.$reduce(base).value shouldBe base
        ContractCodec.$reduce(base, true).value shouldBe base
      }
      it("Should return failure if expected object properties missing") {
        val base = DObject.empty
        ContractCodec.$reduce(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
        ContractCodec.$reduce(base, true).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should return failure if expected object is empty") {
        val base = DObject("property" := DObject.empty)
        ContractCodec.$reduce(base).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
        ContractCodec.$reduce(base, true).left.value should contain(ExpectedFailure(ContractCodec, Path("property")))
      }
      it("Should reduce key value if expected object is empty and contract allows empty") {
        val base = DObject(
          "property" ::= ("empty" := DObject.empty, "nonEmpty" := DObject("one" := 1), "nonEmpty2" := DObject("two" := 2))
        )
        ContractAtLeastTwoCodec.$reduce(base).value shouldBe DObject(
          "property" ::= ("nonEmpty" := DObject("one" := 1), "nonEmpty2" := DObject("two" := 2))
        )
      }
      it("Should fail if expected object is empty and contract doesnt allow empty") {
        val base = DObject("property" ::= ("empty" := DObject.empty, "nonEmpty" := DObject("one" := 1)))
        ContractCodec.$reduce(base).left.value should contain(
          ExpectedFailure(ContractCodec, Path("property", "empty", "expected"))
        )
      }
      it("Should return failure if expected object does not satisfy contract conditions") {
        val base = DObject("property" ::= ("failure" ::= ("maybe" := "one")))
        ContractCodec.$reduce(base).left.value should contain allOf (
          ExpectedFailure(ContractCodec, Path("property", "failure", "expected")),
          IncorrectTypeFailure(ContractCodec, Path("property", "failure", "maybe"), booleanCodec, "one")
        )

      }
      it("Should return empty if current empty if value contract conditions under dropBadTypes is true") {
        val base = DObject("property" ::= ("failure" ::= ("maybe" := "one")))
        MaybeContractCodec.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should fail if collection entity fails") {
        val content = DObject("onlyOne" := DObject("one" := 1))
        val base    = DObject("property" := content)
        ContractAtLeastTwoCodec.$reduce(base).left.value should contain(
          IncorrectTypeFailure(ContractAtLeastTwoCodec, Path("property"), D2, content.value)
        )
      }

      it("Should return reduce null and empty object values in contract") {
        val base = DObject(
          "property" ::= ("reducing" ::= ("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty))
        )
        ContractCodec.$reduce(base).value shouldBe DObject(
          "property" ::= ("reducing" ::= ("expected" := "value", "maybe" := false, "default" := 0))
        )
        ContractCodec.$reduce(base, true).value shouldBe DObject(
          "property" ::= ("reducing" ::= ("expected" := "value", "maybe" := false, "default" := 0))
        )
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
        MapCodec.$reduce(base).value shouldBe base
        MapCodec.$reduce(base, true).value shouldBe base
      }
      it("Should return failure if map key does not satisfy contract conditions") {
        val base = DObject("property" ::= ("key25" := 1234, "key2" := 131))
        MapCodec.$reduce(base).left.value should contain(
          IncorrectKeyTypeFailure(MapCodec, Path("property"), Length4String.fixedLength4StringCodec, "key25")
        )
      }
      it("Should return failure if map value does not satisfy contract conditions") {
        val base = DObject("property" ::= ("key2" := 1234, "key2" := "bob"))
        MapCodec.$reduce(base).left.value should contain(
          IncorrectTypeFailure(MapCodec, Path("property", "key2"), intCodec, "bob")
        )
      }
      it("Should return reduced map if key does not satisfy contract conditions with DropBadType is true") {
        val base = DObject("property" ::= ("key25" := 1234, "key2" := 131))
        MapCodec.$reduce(base, true).value shouldBe DObject("property" ::= ("key2" := 131))
      }
      it("Should return reduced map if value does not satisfy contract conditions with DropBadType is true") {
        val base = DObject("property" ::= ("key1" := 1234, "key2" := "bob"))
        MapCodec.$reduce(base, true).value shouldBe DObject("property" ::= ("key1" := 1234))
      }
      it("Should return failure if expected map reduces to empty") {
        val base  = DObject("property" ::= ("key1" := DNull))
        MapCodec.$reduce(base).left.value should contain(ExpectedFailure(MapCodec.property))
        MapCodec.$reduce(base, true).left.value should contain(ExpectedFailure(MapCodec.property))
        val base2 = DObject("property" ::= ("key1" := DObject.empty))
        MapCodec.$reduce(base2).left.value should contain(ExpectedFailure(MapCodec.property))
        MapCodec.$reduce(base2, true).left.value should contain(ExpectedFailure(MapCodec.property))
      }
      it("Should return empty if maybe property reduces to empty") {
        val base = DObject("property" ::= ("key1" := DNull, "key2" := DObject.empty))
        MaybeMapCodec.$reduce(base).value shouldBe DObject.empty
        MaybeMapCodec.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return reduced object if maybe property reduces to empty even if keys are invalid") {
        val base = DObject(
          "property" ::= ("key1" := 3, "key11" := DNull, "key21" := DObject.empty, "key22" ::= ("empty" := DObject.empty))
        )
        MapCodec.$reduce(base).value shouldBe DObject("property" ::= ("key1" := 3))
        MapCodec.$reduce(base, true).value shouldBe DObject("property" ::= ("key1" := 3))
      }
      it("Should return failure with contract failures") {
        val base = DObject("property" ::= ("key3" := DObject("maybe" := false)))
        MaybeMapCodec.$reduce(base).left.value should contain(
          ExpectedFailure(MaybeMapCodec, Path("property", "key3", "expected"))
        )
      }
      it("Should return empty if maybe property contains only invalid key pairs and DropBadType is true") {
        val base = DObject(
          "property" ::= ("key1" := false, "key24" := DObject("expected" := "value"), "key3" := DObject("maybe" := false))
        )
        MaybeMapCodec.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return reduce null and empty object values in contract") {
        val base = DObject(
          "property" ::= ("key1" ::= ("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty))
        )
        MaybeMapCodec.$reduce(base).value shouldBe DObject(
          "property" ::= ("key1" ::= ("expected" := "value", "maybe" := false, "default" := 0))
        )
        MaybeMapCodec.$reduce(base, true).value shouldBe DObject(
          "property" ::= ("key1" ::= ("expected" := "value", "maybe" := false, "default" := 0))
        )
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
        CollectionCodec.$reduce(base).value shouldBe base
        CollectionCodec.$reduce(base, true).value shouldBe base
      }
      it("Should return failure if element does not satisfy contract conditions") {
        val base = DObject("property" := Vector(Data(1234), Data("bob")))
        CollectionCodec.$reduce(base).left.value should contain(
          IncorrectTypeFailure(CollectionCodec, "property" :: 1 :: PathEnd, intCodec, "bob")
        )
      }
      it("Should return element missing failure if expected value does not satisfy contract conditions with DropBadType is true") {
        val base = DObject("property" := Vector(Data(1234), Data("bob")))
        CollectionCodec.$reduce(base, true).left.value should contain(
          MissingElementFailure(CollectionCodec, CollectionCodec.property._codec, Path("property", 1))
        )
      }
      it("Should return object if object collection is empty") {
        val base = DObject("property" := Vector[Int]())
        CollectionCodec.$reduce(base).value shouldBe base
        CollectionCodec.$reduce(base, true).value shouldBe base
      }
      it("Should return failure with contract failures") {
        val base = DObject("property" := Vector(DObject("maybe" := false)))
        MaybeCollectionCodec.$reduce(base).left.value should contain(
          ExpectedFailure(MaybeCollectionCodec, Path("property", 0, "expected"))
        )
      }
      it("Should return empty if maybe property contains an invalid value with DropBadTypes is true") {
        val base = DObject("property" := Vector(DObject("expected" := "value"), Data("bob")))
        MaybeCollectionCodec.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return reduce null and empty object values in contract") {
        val base = DObject(
          "property" := Vector(
            DObject("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty)
          )
        )
        MaybeCollectionCodec.$reduce(base).value shouldBe DObject(
          "property" := Vector(DObject("expected" := "value", "maybe" := false, "default" := 0))
        )
        MaybeCollectionCodec.$reduce(base, true).value shouldBe DObject(
          "property" := Vector(DObject("expected" := "value", "maybe" := false, "default" := 0))
        )
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
        CoproductCodec.$reduce(base).value shouldBe base
        CoproductCodec.$reduce(base, true).value shouldBe base
        val base2 = DObject("property" := "value")
        CoproductCodec.$reduce(base2).value shouldBe base2
        CoproductCodec.$reduce(base2, true).value shouldBe base2
      }
      it("Should return failure if incorrect type") {
        val base = DObject("property" := false)
        CoproductCodec.$reduce(base).left.value should contain(
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
        CoproductCodec.$reduce(base, true).left.value should contain(
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

      it("Should reduce down contract values") {
        val base = DObject(
          "property" ::= ("expected" := "value", "maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty)
        )
        MaybeCoproductCodec.$reduce(base).value shouldBe DObject(
          "property" ::= ("expected" := "value", "maybe" := false, "default" := 0)
        )
        MaybeCoproductCodec.$reduce(base, true).value shouldBe DObject(
          "property" ::= ("expected" := "value", "maybe" := false, "default" := 0)
        )
      }
      it("Should return contract failures") {
        val base = DObject("property" ::= ("maybe" := false, "default" := 0, "add1" := DNull, "add2" := DObject.empty))
        MaybeCoproductCodec.$reduce(base).left.value should contain(
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
      it("Should return reduce contract failure if DropBadTypes is true") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := 1234, "default" := 0))
        MaybeCoproductCodec.$reduce(base, true).value shouldBe DObject(
          "property" ::= ("expected" := "value", "default" := 0)
        )
      }
    }
    describe("Type Contract Codec Type") {

      object Type1                  extends Contract with Open {
        val _type     = \[String]("type")(DValueCodec.literal("type1"))
        val property1 = \[String]
        val property2 = \?[Int]
      }
      object Type2                  extends Contract with Open {
        val _type     = \[String]("type")(DValueCodec.literal("type2"))
        val property1 = \[Int]
        val property2 = \?[String]
      }
      object TypeContractCodec      extends Contract           {
        val property = \[DObject](DTypeContractCodec(ObjectDefinition.empty) {
          case Type1._type(_) => Type1
          case Type2._type(_) => Type2
        })
      }
      object MaybeTypeContractCodec extends Contract           {
        val property = \?[DObject](DTypeContractCodec(ObjectDefinition.empty) {
          case Type1._type(_) => Type1
          case Type2._type(_) => Type2
        })
      }
      it("Should return object if target contract satisfied") {
        val base  = DObject("property" := DObject("type" := "type1", "property1" := "value"))
        TypeContractCodec.$reduce(base).value shouldBe base
        TypeContractCodec.$reduce(base, true).value shouldBe base
        val base2 = DObject("property" := DObject("type" := "type2", "property1" := 34532, "property2" := "value"))
        TypeContractCodec.$reduce(base2).value shouldBe base2
        TypeContractCodec.$reduce(base2, true).value shouldBe base2
      }
      it("Should return failure if target contract not satisfied") {
        val base  = DObject("property" := DObject("type" := "type2", "property1" := "value"))
        TypeContractCodec.$reduce(base).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property1"), intCodec, "value")
        )
        TypeContractCodec.$reduce(base, true).left.value should contain(
          IncorrectTypeFailure(TypeContractCodec, Path("property", "property1"), intCodec, "value")
        )
        val base2 = DObject("property" := DObject("type" := "type1", "property2" := 1234))
        TypeContractCodec.$reduce(base2).left.value should contain(
          ExpectedFailure(TypeContractCodec, Path("property", "property1"))
        )
        TypeContractCodec.$reduce(base2, true).left.value should contain(
          ExpectedFailure(TypeContractCodec, Path("property", "property1"))
        )
      }
      it("Should return failure if expected property is empty") {
        val base = DObject.empty
        TypeContractCodec.$reduce(base).left.value should contain(ExpectedFailure(TypeContractCodec, Path("property")))
        TypeContractCodec.$reduce(base, true).left.value should contain(
          ExpectedFailure(TypeContractCodec, Path("property"))
        )
      }
      it("Should return failure if expected object is empty") {
        val base = DObject("property" := DObject.empty)
        TypeContractCodec.$reduce(base).left.value should contain(ExpectedFailure(TypeContractCodec, Path("property")))
        TypeContractCodec.$reduce(base, true).left.value should contain(
          ExpectedFailure(TypeContractCodec, Path("property"))
        )
      }
      it("Should return empty if maybe target contract not expected satisfied and DropBadTypes is true") {
        val base  = DObject("property" := DObject("type" := "type2", "property1" := "value"))
        MaybeTypeContractCodec.$reduce(base, true).value shouldBe DObject.empty
        val base2 = DObject("property" := DObject("type" := "type1", "property2" := 1234))
        MaybeTypeContractCodec.$reduce(base2, true).value shouldBe DObject.empty
      }
      it("Should fail if target contract cannot be resolved") {
        val base = DObject("property" := DObject("property1" := "value"))
        TypeContractCodec.$reduce(base).left.value should contain(
          ContractTypeResolutionFailure(TypeContractCodec, Path("property"), DObject("property1" := "value").value)
        )
        TypeContractCodec.$reduce(base, true).left.value should contain(
          ContractTypeResolutionFailure(TypeContractCodec, Path("property"), DObject("property1" := "value").value)
        )
      }
      it("Should return empty if maybe target contract cannot be resolved and DropBadTypes is true") {
        val base = DObject("property" := DObject("property1" := "value"))
        MaybeTypeContractCodec.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should reduce elements in target contract") {
        val base = DObject(
          "property" := DObject("type" := "type1", "property1" := "value", "add1" := DNull, "add2" := DObject.empty)
        )
        TypeContractCodec.$reduce(base).value shouldBe DObject(
          "property" := DObject("type" := "type1", "property1" := "value")
        )
        TypeContractCodec.$reduce(base, true).value shouldBe DObject(
          "property" := DObject("type" := "type1", "property1" := "value")
        )
      }
      it("Should reduce failed types in target contract with DropBadTypes is true") {
        val base = DObject("property" := DObject("type" := "type2", "property1" := 1234, "property2" := false))
        MaybeTypeContractCodec.$reduce(base, true).value shouldBe DObject(
          "property" := DObject("type" := "type2", "property1" := 1234)
        )
      }
    }

    describe("Constraints") {
      import dsentric.operators.StandardOperators._

      object Constrained extends Contract {
        val expected       = \[String](immutable)
        val maybe          = \?[Int](internal)
        val default        = \![String]("value", mask("******"))
        val reserve        = \?[Boolean](reserved)
        val expectedObject = new \\?(reserved) {
          val property1 = \[Int]
          val property2 = \?[String]
        }
      }
      object Temp        extends Contract {
        val bob = new \\?(reserved) {
          val property1 = \[Int]
        }
      }
      it("Should fail reserved on nested object, even if wrong type") {
        val base = DObject("bob" := "next")
        val b    = Temp.$reduce(base).left.value
        b should contain only (ReservedFailure(Temp, Path("bob")))
      }
      it("Should return constraint failure even if type is wrong") {
        val base = DObject("expected" := "value", "default" := "value", "reserve" := "blah")
        val b    = Constrained.$reduce(base).left.value
        b should contain only (ReservedFailure(Constrained, Path("reserve")))
      }
      it("Should return constraint failure even if type is wrong and reserved is object") {
        val base = DObject("expected" := "value", "default" := "value", "expectedObject" := true)
        val b    = Constrained.$reduce(base).left.value
        b should contain only (ReservedFailure(Constrained, Path("expectedObject")))
      }

      it("Should return object if constraint not trigger") {
        val base = DObject("expected" := "value", "default" := "value")
        Constrained.$reduce(base).value shouldBe base
      }
      it("Should return failure if constraint triggered") {
        val base = DObject("expected" := "value", "maybe" := 123, "default" := "******")
        Constrained.$reduce(base).left.value should contain allOf (
          ReservedFailure(Constrained, Path("maybe")),
          MaskFailure(Constrained, Path("default"), "******")
        )
      }

      it("Should return failure if Object constraint triggered") {
        val base = DObject("expectedObject" ::= ("property1" := 123, "property2" := "value"))
        Constrained.$reduce(base).left.value should contain(ReservedFailure(Constrained, Path("expectedObject")))
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
        ExpectedObject.$reduce(base).value shouldBe base
        ExpectedObject.$reduce(base, true).value shouldBe base
      }
      it("Should fail if object is empty and requires expected properties") {
        val base = DObject.empty
        ExpectedObject.$reduce(base).left.value should contain(ExpectedFailure(ExpectedObject.property.expected))
        ExpectedObject.$reduce(base, true).left.value should contain(ExpectedFailure(ExpectedObject.property.expected))
      }
      it("Should fail if expected object is empty and requires expected properties") {
        val base = DObject("property" := DObject.empty)
        ExpectedObject.$reduce(base).left.value should contain(ExpectedFailure(ExpectedObject.property.expected))
        ExpectedObject.$reduce(base, true).left.value should contain(ExpectedFailure(ExpectedObject.property.expected))
      }
      it("Should fail if expected object is null and requires expected properties") {
        val base = DObject("property" := DNull)
        ExpectedObject.$reduce(base).left.value should contain(ExpectedFailure(ExpectedObject.property.expected))
        ExpectedObject.$reduce(base, true).left.value should contain(ExpectedFailure(ExpectedObject.property.expected))
      }
      it("Should fail if expected object expected properties fail to satisfy conditions") {
        val base = DObject("property" := DObject("expected" := 12341, "maybe" := "fail"))
        ExpectedObject.$reduce(base).left.value should contain(
          IncorrectTypeFailure(ExpectedObject.property.expected, 12341)
        )
        ExpectedObject.$reduce(base, true).left.value should contain(
          IncorrectTypeFailure(ExpectedObject.property.expected, 12341)
        )
      }
      it("Should fail if expected object maybe properties fail to satisfy conditions") {
        val base = DObject("property" := DObject("expected" := "value", "maybe" := "fail"))
        ExpectedObject.$reduce(base).left.value should contain(
          IncorrectTypeFailure(ExpectedObject.property.maybe, "fail")
        )
      }
      it("Should return reduced object if maybe properties fail to satisfy conditions and DropBadType is true") {
        val base = DObject("property" := DObject("expected" := "value", "maybe" := "fail"))
        ExpectedObject.$reduce(base, true).value shouldBe DObject("property" := DObject("expected" := "value"))
      }
      it("Should return reduce object if contains null and empty object property values") {
        val base = DObject("property" := DObject("expected" := "value", "maybe" := DNull, "additional" := DObject.empty))
        ExpectedObject.$reduce(base).value shouldBe DObject("property" := DObject("expected" := "value"))
        ExpectedObject.$reduce(base, true).value shouldBe DObject("property" := DObject("expected" := "value"))
      }
      it("Should return empty if object is empty and requires no expected properties") {
        val base = DObject.empty
        ExpectedMaybeObject.$reduce(base).value shouldBe DObject.empty
        ExpectedMaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty if expected object is empty and requires no expected properties") {
        val base = DObject("property" := DObject.empty)
        ExpectedMaybeObject.$reduce(base).value shouldBe DObject.empty
        ExpectedMaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty if expected object is null and requires no expected properties") {
        val base = DObject("property" := DNull)
        ExpectedMaybeObject.$reduce(base).value shouldBe DObject.empty
        ExpectedMaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty if expected object has no expected properties but others fail to satisfy conditions with DropBadTypes is true") {
        val base = DObject("property" ::= ("maybe" := 1234, "maybe2" := false))
        ExpectedMaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }
    }
    describe("Maybe property") {
      object MaybeObject      extends Contract {
        val property = new \\? with Open {
          val expected = \[String]
          val maybe    = \?[Int]
        }
      }
      object MaybeMaybeObject extends Contract {
        val notExpected = new \\? with Open {
          val maybe  = \?[String]
          val maybe2 = \?[Int]
        }
      }
      it("Should return object if maybe object satisfies contract") {
        val base = DObject("property" ::= ("expected" := "value", "maybe" := 123))
        MaybeObject.$reduce(base).value shouldBe base
        MaybeObject.$reduce(base, true).value shouldBe base
      }
      it("Should return empty if object is empty") {
        val base = DObject.empty
        MaybeObject.$reduce(base).value shouldBe DObject.empty
        MaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty if maybe object is empty ") {
        val base = DObject("property" := DObject.empty)
        MaybeObject.$reduce(base).value shouldBe DObject.empty
        MaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty if maybe object is null ") {
        val base = DObject("property" := DNull)
        MaybeObject.$reduce(base).value shouldBe DObject.empty
        MaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return failure if maybe object expected properties fail to satisfy conditions") {
        val base = DObject("property" := DObject("expected" := 12341, "maybe" := 134))
        MaybeObject.$reduce(base).left.value should contain(IncorrectTypeFailure(MaybeObject.property.expected, 12341))
      }
      it(
        "Should return empty if maybe object expected properties fail to satisfy conditions with DropBadTypes = true"
      ) {
        val base = DObject("property" := DObject("expected" := 12341, "maybe" := 134))
        MaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return reduced object if maybe properties fail to satisfy conditions and DropBadType is true") {
        val base = DObject("property" := DObject("expected" := "value", "maybe" := "fail"))
        MaybeObject.$reduce(base, true).value shouldBe DObject("property" := DObject("expected" := "value"))
      }
      it("Should return reduce object if contains null and empty object property values") {
        val base = DObject("property" := DObject("expected" := "value", "maybe" := DNull, "additional" := DObject.empty))
        MaybeObject.$reduce(base).value shouldBe DObject("property" := DObject("expected" := "value"))
        MaybeObject.$reduce(base, true).value shouldBe DObject("property" := DObject("expected" := "value"))
      }
      it("Should return failure if contains null and empty object property values only") {
        val base = DObject("property" := DObject("maybe" := DNull, "additional" := DObject.empty))
        MaybeObject.$reduce(base).left.value should contain(ExpectedFailure(MaybeObject.property.expected))
      }
      it("Should reduce to empty if contains null and empty object property values only with DropBadTypes is true") {
        val base = DObject("property" := DObject("maybe" := DNull, "additional" := DObject.empty))
        MaybeObject.$reduce(base, true).value shouldBe DObject.empty
      }

      it("Should return empty if maybe object has no expected properties but others fail to satisfy conditions with DropBadTypes is true") {
        val base = DObject("property" ::= ("maybe" := 1234, "maybe2" := false))
        MaybeMaybeObject.$reduce(base, true).value shouldBe DObject.empty
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
        Closed.$reduce(base).left.value should contain(ClosedContractFailure(Closed, Path("property"), "additional"))
      }
      it("Should return empty object if object contains additional properties which are null or empty") {
        val base = DObject(
          "property" ::= ("additional" := DNull, "additional2" := DObject.empty, "additional3" := DObject(
            "nested" := DNull
          ))
        )
        Closed.$reduce(base).value shouldBe DObject.empty
        Closed.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty if object contains additional properties and DropBadType is true") {
        val base = DObject("property" ::= ("additional" := "value"))
        Closed.$reduce(base, true).value shouldBe DObject.empty
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
        Additional.$reduce(base).left.value should contain(
          IncorrectKeyTypeFailure(Additional, Path("property"), Length4String.fixedLength4StringCodec, "failed")
        )
      }
      it("Should return empty if additional properties have invalid key with DropBadType is true") {
        val base = DObject("property" ::= ("failed" := 1234))
        Additional.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return fail if additional properties have invalid value") {
        val base = DObject("property" ::= ("add1" := "failed"))
        Additional.$reduce(base).left.value should contain(
          IncorrectTypeFailure(Additional, Path("property", "add1"), longCodec, "failed")
        )
      }
      it("Should return empty if additional properties have invalid value with DropBadType is true") {
        val base = DObject("property" ::= ("add1" := "failed"))
        Additional.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return empty if additional properties have null or reduced values even if key not satisfied") {
        val base = DObject(
          "property" ::= ("additional" := DNull, "additional2" := DObject.empty, "additional3" := DObject(
            "nested" := DNull
          ))
        )
        Additional.$reduce(base).value shouldBe DObject.empty
        Additional.$reduce(base, true).value shouldBe DObject.empty
      }

      it("Should return valid object if additional properties are valid") {
        val base = DObject("property" ::= ("maybe" := "value", "add1" := 1412L))
        Additional.$reduce(base).value shouldBe base
        Additional.$reduce(base, true).value shouldBe base
      }
      it("Should return failure if additional property value is satisfied but overlays defined field") {
        val base = DObject("property" ::= ("maybe" := 1412L))
        Additional.$reduce(base).left.value should contain(IncorrectTypeFailure(Additional.property.maybe, 1412L))
      }

      it("Should fail if nested contract fails to validate") {
        val base = DObject("codecProperty" ::= ("obj1" ::= ("maybe" := true)))
        Additional.$reduce(base).left.value should contain(
          ExpectedFailure(Additional, Path("codecProperty", "obj1", "expected"))
        )
      }
      it("Should return empty if nested contract expected fails under drop under bad type ") {
        val base = DObject("codecProperty" ::= ("obj1" ::= ("maybe" := true)))
        Additional.$reduce(base, true).value shouldBe DObject.empty
      }
      it("Should return reduce object if nested contract valid but contains nulls and empty objects") {
        val base = DObject(
          "codecProperty" ::= ("obj1" ::= ("expected" := "value", "maybe" := DNull, "additional2" := DObject.empty, "additional3" := DObject(
            "nested" := DNull
          )))
        )
        Additional.$reduce(base).value shouldBe DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value")))
        Additional.$reduce(base, true).value shouldBe DObject("codecProperty" ::= ("obj1" ::= ("expected" := "value")))
      }
    }
  }
}
