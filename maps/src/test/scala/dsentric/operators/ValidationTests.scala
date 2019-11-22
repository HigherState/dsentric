package dsentric.operators

import dsentric.PessimisticCodecs._
import dsentric._
import dsentric.contracts.Contract
import org.scalatest.{FunSuite, Matchers}

class ValidationTests extends FunSuite with Matchers {
  
  object TestContract extends Contract

  test("Numeric validators") {
    Validators.>(4)(TestContract, Path.empty, Some(5), None) should be (Vector.empty)
    Validators.>(4)(TestContract, Path.empty, Some(4), None) should not be Vector.empty
    Validators.>(4)(TestContract, Path.empty, Some(3.5), None) should not be Vector.empty
    Validators.>(4)(TestContract, Path.empty, Some(Some(12.5)), None) should be (Vector.empty)

    Validators.<(-2)(TestContract, Path.empty, Some(-4), None) should be (Vector.empty)
    Validators.<(-2)(TestContract, Path.empty, Some(-2), None) should not be Vector.empty
    Validators.<(-2.5)(TestContract, Path.empty, Some(3), None) should not be Vector.empty

    Validators.>=(4.5)(TestContract, Path.empty, Some(5), None) should be (Vector.empty)
    Validators.>=(4)(TestContract, Path.empty, Some(4), None) should be (Vector.empty)
    Validators.>=(4.2)(TestContract, Path.empty, Some(3.5), None) should not be Vector.empty

    Validators.<=(-2)(TestContract, Path.empty, Some(-4), None) should be (Vector.empty)
    Validators.<=(-2)(TestContract, Path.empty, Some(-2), None) should be (Vector.empty)
    Validators.<=(-2.5)(TestContract, Path.empty, Some(3), None) should not be Vector.empty

    Validators.increment(TestContract, Path.empty, Some(3), Some(4)) should not be Vector.empty
    Validators.increment(TestContract, Path.empty, Some(3), Some(3)) should not be Vector.empty
    Validators.increment(TestContract, Path.empty, Some(4), Some(3)) should be (Vector.empty)

    Validators.decrement(TestContract, Path.empty, Some(4), Some(3)) should not be Vector.empty
    Validators.decrement(TestContract, Path.empty, Some(3), Some(3)) should not be Vector.empty
    Validators.decrement(TestContract, Path.empty, Some(3), Some(4)) should be (Vector.empty)
  }

  test("Length validators") {
    Validators.minLength(4)(TestContract, Path.empty, Some(List(1,2,3,4,5,6)), None) should be (Vector.empty)
    Validators.minLength(4)(TestContract, Path.empty, Some(Vector(1,2)), None) should not be Vector.empty
    Validators.maxLength(12)(TestContract, Path.empty, Some(Some(Iterable(3,2,2,1))), None) should be (Vector.empty)
    Validators.maxLength(3)(TestContract, Path.empty, Some(Iterable(3,2,2,1)), None) should not be Vector.empty
    Validators.maxLength(3)(TestContract, Path.empty, Some("dashj"), None) should not be Vector.empty
  }

  test("in/nin validators") {
    Validators.in("one", "two", "three").apply(TestContract, Path.empty, Some("one"), None) should be (Vector.empty)
    Validators.in("one", "two", "three").apply(TestContract, Path.empty, Some(Some("two")), None) should be (Vector.empty)
    Validators.in("one", "two", "three").apply(TestContract, Path.empty, Some("four"), None) should not be Vector.empty

    Validators.nin(1,2,3).apply(TestContract, Path.empty, Some(4), None) should be (Vector.empty)
    Validators.nin(1,2,3).apply(TestContract, Path.empty, Some(2), None) should not be Vector.empty
  }

  test("regex validator") {
    Validators.regex("abc".r)(TestContract, Path.empty, Some("abc"), None) should be (Vector.empty)
    Validators.regex("abc".r)(TestContract, Path.empty, Some("def"), None) should not be Vector.empty
    Validators.regex("abc".r)(TestContract, Path.empty, Some(Some("def")), None) should not be Vector.empty
  }

  test("immutable validator") {
    Validators.immutable(TestContract, Path.empty, Some("value"), Some("value")) should be (Vector.empty)
    Validators.immutable(TestContract, Path.empty, Some("value"), None) should be (Vector.empty)
    Validators.immutable(TestContract, Path.empty, None, Some("value")) should be (Vector.empty)
    Validators.immutable(TestContract, Path.empty, Some("newValue"), Some("value")) should not be Vector.empty
  }

  test("keyMatcher") {
    import PessimisticCodecs._
    import dsentric.Dsentric._

    val r = "[a-z]*".r
    val validator = Validators.keyValidator(r, "Invalid Key")

    validator(TestContract, Path.empty, Some(DObject("key" := 1)), None) should be (Vector.empty)
    validator(TestContract, Path.empty, Some(DObject("key2" := 1)), None) should not be Vector.empty

  }
}
