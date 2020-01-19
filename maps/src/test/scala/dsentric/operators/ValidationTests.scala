package dsentric.operators

import dsentric.PessimisticCodecs._
import dsentric._
import dsentric.contracts.Contract
import dsentric.failure.{MaximumLengthFailure, MinimumLengthFailure, ValidationFailures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ValidationTests extends AnyFunSuite with Matchers {

  object TestContract extends Contract

  test("Numeric validators") {
    Validators.>(4)(TestContract, Path.empty, 5, None) should be (ValidationFailures.empty)
    Validators.>(4)(TestContract, Path.empty, 4, None) should not be ValidationFailures.empty
    Validators.>(4)(TestContract, Path.empty, 3.5, None) should not be ValidationFailures.empty
    Validators.>(4)(TestContract, Path.empty, 12.5, None) should be (ValidationFailures.empty)

    Validators.<(-2)(TestContract, Path.empty, Some(-4), None) should be (ValidationFailures.empty)
    Validators.<(-2)(TestContract, Path.empty, Some(-2), None) should not be ValidationFailures.empty
    Validators.<(-2.5)(TestContract, Path.empty, Some(3), None) should not be ValidationFailures.empty

    Validators.>=(4.5)(TestContract, Path.empty, Some(5), None) should be (ValidationFailures.empty)
    Validators.>=(4)(TestContract, Path.empty, Some(4), None) should be (ValidationFailures.empty)
    Validators.>=(4.2)(TestContract, Path.empty, Some(3.5), None) should not be ValidationFailures.empty

    Validators.<=(-2)(TestContract, Path.empty, Some(-4), None) should be (ValidationFailures.empty)
    Validators.<=(-2)(TestContract, Path.empty, Some(-2), None) should be (ValidationFailures.empty)
    Validators.<=(-2.5)(TestContract, Path.empty, Some(3), None) should not be ValidationFailures.empty

    Validators.increment(TestContract, Path.empty, Some(3), Some(4)) should not be ValidationFailures.empty
    Validators.increment(TestContract, Path.empty, Some(3), Some(3)) shouldBe ValidationFailures.empty
    Validators.increment(TestContract, Path.empty, Some(4), Some(3)) should be (ValidationFailures.empty)

    Validators.decrement(TestContract, Path.empty, Some(4), Some(3)) should not be ValidationFailures.empty
    Validators.decrement(TestContract, Path.empty, Some(3), Some(3)) shouldBe ValidationFailures.empty
    Validators.decrement(TestContract, Path.empty, Some(3), Some(4)) should be (ValidationFailures.empty)
  }

  test("Length validators") {
    Validators.minLength(4)(TestContract, Path.empty, Some(List(1,2,3,4,5,6)), None) should be (ValidationFailures.empty)
    Validators.minLength(4)(TestContract, Path.empty, Some(Vector(1,2)), None) should contain (MinimumLengthFailure(TestContract, Path.empty, 4, 2))
    Validators.maxLength(12)(TestContract, Path.empty, Some(Some(Iterable(3,2,2,1))), None) should be (ValidationFailures.empty)
    Validators.maxLength(3)(TestContract, Path.empty, Some(Iterable(3,2,2,1)), None) should contain (MaximumLengthFailure(TestContract, Path.empty, 3, 4))
    Validators.maxLength(3)(TestContract, Path.empty, Some("dashj"), None) should not be ValidationFailures.empty
  }
  test("Length validators with DObject") {
    import Dsentric._
    Validators.minLength(2)(TestContract, Path.empty, Some(DObject("one" := 1, "two" := 2)), None) should be (ValidationFailures.empty)
    Validators.minLength(2)(TestContract, Path.empty, Some(DObject("one" := 1)), None) should contain (MinimumLengthFailure(TestContract, Path.empty, 2, 1))
    Validators.maxLength(2)(TestContract, Path.empty, Some(DObject("one" := 1, "two" := 2)), None) should be (ValidationFailures.empty)
    Validators.maxLength(2)(TestContract, Path.empty, Some(DObject("one" := 1, "two" := 2, "three" := 3)), None) should contain (MaximumLengthFailure(TestContract, Path.empty, 2, 3))
  }
  test("Length validators with DObject under delta") {
    import Dsentric._
    Validators.minLength(2)(TestContract, Path.empty, Some(DObject("one" := 1)), Some(DObject("two" := 1, "three" := 2))) should be (ValidationFailures.empty)
    Validators.minLength(2)(TestContract, Path.empty, Some(DObject("one" := 1, "two" := DNull)), Some(DObject("two" := 1, "one" := 3))) should contain (MinimumLengthFailure(TestContract, Path.empty, 2, 1))
    Validators.maxLength(2)(TestContract, Path.empty, Some(DObject("one" := 1)), Some(DObject("two" := 1, "three" := 2))) should contain (MaximumLengthFailure(TestContract, Path.empty, 2, 3))
    Validators.maxLength(2)(TestContract, Path.empty, Some(DObject("one" := 1, "two" := DNull)), Some(DObject("two" := 1, "three" := 2))) should be (ValidationFailures.empty)
  }


  test("in/nin validators") {
    Validators.in("one", "two", "three").apply(TestContract, Path.empty, Some("one"), None) should be (ValidationFailures.empty)
    Validators.in("one", "two", "three").apply(TestContract, Path.empty, "two", None) should be (ValidationFailures.empty)
    Validators.in("one", "two", "three").apply(TestContract, Path.empty, Some("four"), None) should not be ValidationFailures.empty
    Validators.nin(1,2,3).apply(TestContract, Path.empty, Some(4), None) should be (ValidationFailures.empty)
    Validators.nin(1,2,3).apply(TestContract, Path.empty, Some(2), None) should not be ValidationFailures.empty
  }

  test("regex validator") {
    Validators.regex("abc".r)(TestContract, Path.empty, Some("abc"), None) should be (ValidationFailures.empty)
    Validators.regex("abc".r)(TestContract, Path.empty, Some("def"), None) should not be ValidationFailures.empty
    Validators.regex("abc".r)(TestContract, Path.empty, "def", None) should not be ValidationFailures.empty
  }

  test("immutable validator") {
    Validators.immutable(TestContract, Path.empty, Some("value"), Some("value")) should be (ValidationFailures.empty)
    Validators.immutable(TestContract, Path.empty, Some("value"), None) should be (ValidationFailures.empty)
    Validators.immutable(TestContract, Path.empty, None, Some("value")) should be (ValidationFailures.empty)
    Validators.immutable(TestContract, Path.empty, Some("newValue"), Some("value")) should not be ValidationFailures.empty
  }

  test("keyMatcher") {
    import PessimisticCodecs._
    import dsentric.Dsentric._

    val r = "[a-z]*".r
    val validator = Validators.keyValidator(r, "Invalid Key")

    validator(TestContract, Path.empty, Some(DObject("key" := 1)), None) should be (ValidationFailures.empty)
    validator(TestContract, Path.empty, Some(DObject("key2" := 1)), None) should not be ValidationFailures.empty

  }
}
