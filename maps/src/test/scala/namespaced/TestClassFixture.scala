package namespaced

import dsentric.{DObject, DObjectOps, RawObject}
import dsentric.Dsentric.{ContractFor, SubContractFor}
import dsentric.codecs.{DCodec, DTypeContractCodec}
import dsentric.codecs.std.DCodecs.valueCodec2MapCodec
import dsentric.contracts.{AspectFor, Closed, Contract, DefaultProperty, ExpectedObjectProperty, MaybeProperty, Open, PathSetter}
import dsentric.codecs.std.DCodecs.*
import dsentric.schema.Description

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
  @Description("moreDynamicAttributes description")
  val moreDynamicAttributes = \?[Map[String, DObject]]
}

trait AuditContract[D <: DObjectOps[D] & DObject] extends SubContractFor[D] {

  import dsentric.operators.StandardOperators.*

  val _createdAt = \?[Long](reserved)
  val _createdBy = \?[String](reserved)
}

trait HasMoreDynamicAttributes extends MoreDynamicAttributes[CustomContract] {
  this: SubTypeTraitContract => }

sealed trait TraitContract extends ContractFor[CustomContract] with MoreAttributes[CustomContract] {
  _internal =>
  lazy val aspect =
    new AspectFor[CustomContract, GlobalClass](_internal)(PartialFunction.empty)
      with Open
      with AuditContract[GlobalClass] {
      val property = \?[Int]
    }
}

trait SubTypeTraitContract extends TraitContract {
  _internal =>

  import dsentric.operators.StandardOperators.*

  @Description("subTypeProperty description")
  val subTypeProperty = \?[String]

  lazy val documentAspect =
    new AspectFor[CustomContract, AnotherClass](_internal)(PartialFunction.empty) with Open {
      @Description("Attribute description")
      val attributes0 = new\\?(internal) with Open {}
      val attributes1 = new\\?(internal) with Open {}
      val moreAttributes = \\(_internal.moreAttributes)(PartialFunction.empty)
      val moreDynamicAttributes = \?[Map[String, String]](internal)
    }
}

object TypeContract extends Contract {
  val low  = \[Int]
  val high = \[Int]
}

sealed trait CustomSubContract[D <: DObject] extends SubContractFor[D] {
  def property: DefaultProperty[D, Map[String, DObject]]
}

sealed abstract class TestClassFixture(codec: DCodec[DObject]) extends SubTypeTraitContract {
  val attributes: \\ & CustomSubContract[CustomContract] & Closed = new \\ with CustomSubContract[CustomContract] with Closed {
    override val property =
      \![Map[String, DObject]](Map.empty[String, DObject])(valueCodec2MapCodec[String, DObject](codec))
  }
}

object TypeConfiguration extends Contract {
  val roles = \?[Vector[String]]
  val enabled = \?[Boolean]
}

object TestFixture extends SubTypeTraitContract with HasMoreDynamicAttributes {
  trait Attributes extends SubContractFor[CustomContract] {
    @Description("bool description")
    val bool = \?[Boolean]
  }

  val moreAttributes: \\ & Attributes = new\\ with Attributes

  def withProperty(num: Int): PathSetter[GlobalClass] =
    this.aspect.property.$set(num)
}

object TestFixture2 extends SubTypeTraitContract with HasMoreDynamicAttributes {
  trait Attributes extends SubContractFor[CustomContract] {
    val bool = \[Boolean]
  }

  val moreAttributes: \\ & Attributes = new\\ with Attributes

  def withProperty(num: Int): PathSetter[GlobalClass] =
    this.aspect.property.$set(num)
}

trait OuterAttributes extends SubContractFor[CustomContract] {
  val string = \[String]
  val int    = \[Int]
}

object TestFixture3 extends SubTypeTraitContract with HasMoreDynamicAttributes {
  trait Attributes extends SubContractFor[CustomContract] {
    val bool = \[Boolean]
  }

  val moreAttributes: \\ & Attributes = new\\ with Attributes
  val otherAttributes: \\ & OuterAttributes = new\\ with OuterAttributes

  def withProperty(num: Int): PathSetter[GlobalClass] =
    this.aspect.property.$set(num)
}

object TestFixture4 extends TestClassFixture(DTypeContractCodec {
  case _ => TypeContract
}) {
  val extras: MaybeProperty[CustomContract, Map[String, DObject]] =
    \?[Map[String, DObject]](valueCodec2MapCodec[String, DObject](TypeConfiguration))

  val moreAttributes: \\ & CustomSubContract[CustomContract] = new \\ with CustomSubContract[CustomContract] {
    override val property =
      \![Map[String, DObject]](Map.empty[String, DObject])(valueCodec2MapCodec[String, DObject](TypeContract))
  }
}
