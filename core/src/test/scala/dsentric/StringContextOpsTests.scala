package dsentric

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StringContextOpsTests extends AnyFunSuite with Matchers {

  import dsentric.util.ToStringContextOps._

  test("Ignore case pattern match") {

    ("hello" match { case i"Hello" => true; case _ => false }) shouldBe true
    ("hello" match {
      case i"Hell0" => true
      case _        => false
    }) shouldBe false
  }
}
