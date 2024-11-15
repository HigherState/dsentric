package dsentric.meta

import dsentric.{DObject, DObjectOps, RawObject}
import dsentric.Dsentric.{Contract, ContractFor, SubContractFor}
import dsentric.contracts.{AspectFor, ExpectedObjectProperty, Open, PathSetter}

import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.nowarn

class TypeTagSpec extends AnyFunSpec with Matchers with EitherValues {
  import dsentric.codecs.std.DCodecs.*

  sealed trait CustomContract extends DObject with DObjectOps[CustomContract]

  sealed trait SubTypeOfCustomContract extends CustomContract

  final case class GlobalClass(id: String, value: RawObject) extends CustomContract with DObjectOps[GlobalClass] {
    protected def wrap(value: RawObject): GlobalClass =
      GlobalClass(id, value)
  }

  final case class AnotherClass(value: RawObject) extends SubTypeOfCustomContract with DObjectOps[AnotherClass] {
    protected def wrap(value: RawObject): AnotherClass =
      AnotherClass(value)
  }

  sealed trait MoreAttributes[D <: CustomContract] extends SubContractFor[D] {
    def moreAttributes: SubContractFor[D] & ExpectedObjectProperty[D]
  }

  sealed trait MoreDynamicAttributes[D <: CustomContract] extends MoreAttributes[D] {
    val moreDynamicAttributes = \?[Map[String, DObject]]
  }

  trait AuditContract[D <: DObjectOps[D] & DObject] extends SubContractFor[D] {
    import dsentric.operators.StandardOperators.*

    val _createdAt = \?[Long](reserved)
    val _createdBy = \?[String](reserved)
  }

  trait HasMoreDynamicAttributes extends MoreDynamicAttributes[CustomContract] { this: SubTypeTraitContract => }

  sealed trait TraitContract extends ContractFor[CustomContract] with MoreAttributes[CustomContract] { _internal =>
    import dsentric.operators.StandardOperators.*

    private val _moreDynamicAttributesOperator =
      if (_internal.isInstanceOf[HasMoreDynamicAttributes]) Nil
      else List(internal)

    @nowarn("msg=never used") def $customGlobalFixtureVerification(globalFixture: GlobalClass): List[String] =
      Nil

    lazy val aspect =
      new AspectFor[CustomContract, GlobalClass](_internal)(PartialFunction.empty) with Open with AuditContract[GlobalClass] {
        val property = \?[Int]

        def create(globalFixture: GlobalClass, f: this.type => String): String = "Hello"
      }
  }

  sealed trait SubTypeTraitContract extends TraitContract { _internal =>
    import dsentric.operators.StandardOperators.*

    val subTypeProperty = \?[String]

    lazy val documentAspect =
      new AspectFor[CustomContract, AnotherClass](_internal)(PartialFunction.empty) with Open {
        val attributes0           = new \\?(internal) with Open {}
        val attributes1           = new \\?(internal) with Open {}
        val moreAttributes        = \\(_internal.moreAttributes)(PartialFunction.empty)
        val moreDynamicAttributes = \?[Map[String, String]](internal)
      }
  }

  object TestFixture extends SubTypeTraitContract with HasMoreDynamicAttributes {
    trait Attributes extends SubContractFor[CustomContract] {
      val bool = \[Boolean]
    }

    val moreAttributes: \\ & Attributes = new \\ with Attributes

    def withProperty(num: Int): PathSetter[GlobalClass] =
      this.aspect.property.$set(num)
  }

  object TestFixture2 extends SubTypeTraitContract with HasMoreDynamicAttributes {
    trait Attributes extends SubContractFor[CustomContract] {
      val bool = \[Boolean]
    }

    val moreAttributes: \\ & Attributes = new \\ with Attributes

    def withProperty(num: Int): PathSetter[GlobalClass] =
      this.aspect.property.$set(num)
  }

  object TestFixture3 extends SubTypeTraitContract with HasMoreDynamicAttributes {
    trait Attributes extends SubContractFor[CustomContract] {
      val bool = \[Boolean]
    }

    val moreAttributes: \\ & Attributes = new \\ with Attributes

    def withProperty(num: Int): PathSetter[GlobalClass] =
      this.aspect.property.$set(num)
  }

  describe("TypeTag") {
    val typeTag = TypeTag.of[TestFixture.type]

    it("should resolve for complex contracts") {
      typeTag.fields.find(_.name == "moreAttributes").nonEmpty shouldBe true
    }

    it("should resolve dynamically") {
      def contractList: List[TraitContract] = List(TestFixture)

      inline def typeTagOf[A <: TraitContract](contract: A): TypeTag[A] =
        contract match
          case _: TestFixture.type =>
            TypeTag.of[TestFixture.type].asInstanceOf[TypeTag[A]]
          case _: TestFixture2.type =>
            TypeTag.of[TestFixture2.type].asInstanceOf[TypeTag[A]]
          case _: TestFixture3.type =>
            TypeTag.of[TestFixture3.type].asInstanceOf[TypeTag[A]]

      contractList.map(typeTagOf).flatMap(_.fields).nonEmpty shouldBe true
    }
  }
}