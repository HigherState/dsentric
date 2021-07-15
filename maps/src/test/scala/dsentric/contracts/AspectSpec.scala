package dsentric.contracts

import dsentric.{DObject, RawObject}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AspectSpec extends AnyFunSpec with Matchers with EitherValues {
  import dsentric.codecs.std.DCodecs._
  import dsentric.Dsentric._

  trait Parent extends DObject
  trait Child  extends Parent

  object AspectContract extends Contract {
    val required = \[Int]
  }

  object ParentContract extends ContractFor[Parent] {
    val required = \[Int]
  }

  object Nested extends Contract {
    val nested = new \\ {
      val property1 = \[String]
      val property2 = \[Int]
    }
  }

  describe("Aspect structure") {
    import DAspectSyntax._
    val f: PartialFunction[Property[DObject, _], Option[AspectProperty[DObject, _]]] = {
      case p: ValueProperty[DObject, _] =>
        Some(p.$asExpected())
    }
    lazy val a                                                                       =
      new Aspect[DObject](AspectContract)(f) {
        val myProperty = \[String]
      }
    it("Should verify") {
      println(a.$verify(DObject.empty))
    }
  }
  describe("Child structure") {
    import DAspectSyntax._
    val f: PartialFunction[Property[Parent, _], Option[AspectProperty[Child, _]]] = {
      case p: ValueProperty[Parent, _] =>
        Some(p.$asExpected())
    }
    lazy val a                                                                    =
      new AspectFor[Parent, Child](ParentContract)(f) {
        val myProperty = \[String]
      }
    it("Should verify") {
      println(a.$verify(new Child {
        def value: RawObject = RawObject.empty

        protected def wrap(value: RawObject): DObject = this
      }))
      a.myProperty._path
    }
  }
  describe("Nested structure") {
    import DAspectSyntax._
    val a =
      new Aspect(Nested)() {
        val nested = \\(Nested.nested) {
          case p: ValueProperty[DObject, _] if p._path.tailKeyOption.contains("property1") =>
            Some(p.$asMaybe())
        }
      }

    val b =
      new Aspect(Nested)() {
        val nested = \\(Nested.nested)(p => Some(p.$asMaybe()))
      }

    it("Should verify") {
      println(a.$verify(DObject("nested" ::= ("property2" := 1))))
      println(a.nested.$get(DObject("nested" ::= ("property2" := 1))))
      println(a.$verify(DObject("nested" ::= ("property1" := 1))))
      println(a.nested.$get(DObject("nested" ::= ("property1" := 1))))
      println(b.nested.$get(DObject("nested" ::= ("property1" := 1))))
    }
  }
}
