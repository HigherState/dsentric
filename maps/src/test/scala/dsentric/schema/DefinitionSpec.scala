package dsentric.schema

import dsentric.{DObject, DObjectOps, RawObject}
import dsentric.Dsentric.{ContractFor, SubContractFor}
import dsentric.contracts.{AspectFor, ExpectedObjectProperty, Open, PathSetter}
import dsentric.codecs.std.DCodecs.*

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

sealed trait CustomContract extends DObject with DObjectOps[CustomContract]

sealed trait SubTypeOfCustomContract extends CustomContract

case class GlobalClass(id: String, value: RawObject) extends CustomContract with DObjectOps[GlobalClass] {
  protected def wrap(value: RawObject): GlobalClass =
    GlobalClass(id, value)
}

case class AnotherClass(value: RawObject) extends SubTypeOfCustomContract with DObjectOps[AnotherClass] {
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
  lazy val aspect =
    new AspectFor[CustomContract, GlobalClass](_internal)(PartialFunction.empty)
      with Open
      with AuditContract[GlobalClass] {
      val property = \?[Int]
    }
}

trait SubTypeTraitContract extends TraitContract { _internal =>
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
    val bool = \?[Boolean]
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

class DefinitionTests extends AnyFunSuite with Matchers {
  test("nestedContractObjectDefinition") {
    val definition = Definition.nestedContractObjectDefinition(TestFixture)
    definition.properties should not be empty
  }
}