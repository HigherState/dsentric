package dsentricTests

import org.scalatest.{Matchers, FunSuite}

class StringContextOpsTests extends FunSuite with Matchers {

  import dsentric.util.ToStringContextOps._

  test("Ignore case pattern match") {

    ("hello" match { case i"Hello" => true }) shouldBe true
    ("hello" match { case i"Hell0" => true case _ => false}) shouldBe false
  }
}
