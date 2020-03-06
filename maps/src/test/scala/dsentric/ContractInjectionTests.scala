package dsentric

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

  "$applyDefaults" should "add a missing value to a dobject only if it has a default" in {
    val dObject = DObject.empty
    val res = Flat.$applyDefaults(dObject)
    Flat.one.$get(res) shouldBe None
    Flat.two.$get(res) shouldBe 123
  }
  "$applyDefaults" should "does nothing if no defaults in flat structure" in {
    val dObject = DObject.empty
    val res = FlatNoDefaults.$applyDefaults(dObject)
    res shouldBe DObject.empty
  }
  "$applyDefaults" should "not override an existing property in a dobject with a default" in {
    val dObject = DObject("two" -> Data(321))
    val res = Flat.$applyDefaults(dObject)
    Flat.two.$get(res) shouldBe 321
  }
  "$applyDefaults" should "work when there is a name override" in {
    val dObject1 = DObject.empty
    val res1 = Flat.$applyDefaults(dObject1)
    Flat._three.$get(res1) shouldBe "bob"

    val dObject2 = DObject("three" -> Data("sally"))
    val res2 = Flat.$applyDefaults(dObject2)
    Flat._three.$get(res2) shouldBe "sally"
  }

  "$applyDefaults" should "add a missing value in a nested structure" in {
    val dObject = DObject.empty
    val res = Nested.$applyDefaults(dObject)
    Nested.nest.two.$get(res) shouldBe 123
  }
  "$applyDefaults" should "work when there is a name override in a nested structure" in {
    val dObject1 = DObject.empty
    val res1 = Nested.$applyDefaults(dObject1)
    Nested.nest._three.$get(res1) shouldBe "bob"

    val dObject2 = DObject("nest" -> DObject("three" -> Data("sally")))
    val res2 = Nested.$applyDefaults(dObject2)
    Nested.nest._three.$get(res2) shouldBe "sally"
  }
  "$applyDefaults" should "does nothing if no defaults in nested structure" in {
    val dObject = DObject("non" -> Data("bob"))
    val res = NestedNoDefaults.$applyDefaults(dObject)
    res shouldBe dObject
  }


}
