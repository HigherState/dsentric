package dsentricTests

import dsentric._
import org.scalatest.{FlatSpec, FunSpec, Matchers}

class ContractInjectionTests extends FlatSpec with Matchers {

  import PessimisticCodecs._
  import Dsentric._

  object Flat extends Contract {
    val one = \?[String]
    val two = \![Int](123)
    val _three = \![String]("three", "bob")
  }

  object FlatNoDefaults extends Contract {
    val one = \?[String]
  }

  object Nested extends Contract {
    val nest = new \\ {
      val one = \?[String]
      val two = \![Int](123)
      val _three = \![String]("three", "bob")
    }
  }

  object NestedNoDefaults extends Contract {
    val nest = new \\ {
      val one = \?[String]
    }
  }

  "injectDefaults" should "add a missing value to a dobject only if it has a default" in {
    val dObject = DObject.empty
    val res = Flat.injectDefaults(dObject)
    res.get("one").flatMap(_.decode[String]) shouldBe None
    res.get("two").flatMap(_.decode[Int]) shouldBe Some(123)
  }
  "injectDefaults" should "does nothing if no defaults in flat structure" in {
    val dObject = DObject.empty
    val res = FlatNoDefaults.injectDefaults(dObject)
    res shouldBe DObject.empty
  }
  "injectDefaults" should "not override an existing property in a dobject with a default" in {
    val dObject = DObject("two" -> Data(321))
    val res = Flat.injectDefaults(dObject)
    res.get("two").flatMap(_.decode[Int]) shouldBe Some(321)
  }
  "injectDefaults" should "work when there is a name override" in {
    val dObject1 = DObject.empty
    val res1 = Flat.injectDefaults(dObject1)
    res1.get("three").flatMap(_.decode[String]) shouldBe Some("bob")

    val dObject2 = DObject("three" -> Data("sally"))
    val res2 = Flat.injectDefaults(dObject2)
    res2.get("three").flatMap(_.decode[String]) shouldBe Some("sally")
  }

  "injectDefaults" should "add a missing value in a nested structure" in {
    val dObject = DObject.empty
    val res = Nested.injectDefaults(dObject)
    res.get(Path("nest", "two")).flatMap(_.decode[Int]) shouldBe Some(123)
  }
  "injectDefaults" should "work when there is a name override in a nested structure" in {
    val dObject1 = DObject.empty
    val res1 = Nested.injectDefaults(dObject1)
    res1.get(Path("nest", "three")).flatMap(_.decode[String]) shouldBe Some("bob")

    val dObject2 = DObject("nest" -> DObject("three" -> Data("sally")))
    val res2 = Nested.injectDefaults(dObject2)
    res2.get(Path("nest", "three")).flatMap(_.decode[String]) shouldBe Some("sally")
  }
  "injectDefaults" should "does nothing if no defaults in nested structure" in {
    val dObject = DObject("non" -> Data("bob"))
    val res = NestedNoDefaults.injectDefaults(dObject)
    res shouldBe dObject
  }


}
