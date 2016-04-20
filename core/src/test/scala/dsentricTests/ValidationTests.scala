package dsentricTests

import dsentric._
import org.scalatest.{Matchers, FunSuite}

class ValidationTests extends FunSuite with Matchers {

  test("Numeric validators") {
    Validator.>(4)(Path.empty, Some(5), None) should be (Vector.empty)
    Validator.>(4)(Path.empty, Some(4), None) should not be Vector.empty
    Validator.>(4)(Path.empty, Some(3.5), None) should not be Vector.empty
    Validator.>(4)(Path.empty, Some(Some(12.5)), None) should be (Vector.empty)

    Validator.<(-2)(Path.empty, Some(-4), None) should be (Vector.empty)
    Validator.<(-2)(Path.empty, Some(-2), None) should not be Vector.empty
    Validator.<(-2.5)(Path.empty, Some(3), None) should not be Vector.empty

    Validator.>=(4.5)(Path.empty, Some(5), None) should be (Vector.empty)
    Validator.>=(4)(Path.empty, Some(4), None) should be (Vector.empty)
    Validator.>=(4.2)(Path.empty, Some(3.5), None) should not be Vector.empty

    Validator.<=(-2)(Path.empty, Some(-4), None) should be (Vector.empty)
    Validator.<=(-2)(Path.empty, Some(-2), None) should be (Vector.empty)
    Validator.<=(-2.5)(Path.empty, Some(3), None) should not be Vector.empty

    Validator.increment(Path.empty, Some(3), Some(4)) should not be Vector.empty
    Validator.increment(Path.empty, Some(3), Some(3)) should not be Vector.empty
    Validator.increment(Path.empty, Some(4), Some(3)) should be (Vector.empty)

    Validator.decrement(Path.empty, Some(4), Some(3)) should not be Vector.empty
    Validator.decrement(Path.empty, Some(3), Some(3)) should not be Vector.empty
    Validator.decrement(Path.empty, Some(3), Some(4)) should be (Vector.empty)
  }

  test("Length validators") {
    Validator.minLength(4)(Path.empty, Some(List(1,2,3,4,5,6)), None) should be (Vector.empty)
    Validator.minLength(4)(Path.empty, Some(Vector(1,2)), None) should not be Vector.empty
    Validator.maxLength(12)(Path.empty, Some(Some(Iterable(3,2,2,1))), None) should be (Vector.empty)
    Validator.maxLength(3)(Path.empty, Some(Iterable(3,2,2,1)), None) should not be Vector.empty
    Validator.maxLength(3)(Path.empty, Some("dashj"), None) should not be Vector.empty
  }

  test("in/nin validators") {
    Validator.in("one", "two", "three")(Path.empty, Some("one"), None) should be (Vector.empty)
    Validator.in("one", "two", "three")(Path.empty, Some(Some("two")), None) should be (Vector.empty)
    Validator.in("one", "two", "three")(Path.empty, Some("four"), None) should not be Vector.empty

    Validator.nin(1,2,3)(Path.empty, Some(4), None) should be (Vector.empty)
    Validator.nin(1,2,3)(Path.empty, Some(2), None) should not be Vector.empty
  }

  test("regex validator") {
    Validator.regex("abc".r)(Path.empty, Some("abc"), None) should be (Vector.empty)
    Validator.regex("abc".r)(Path.empty, Some("def"), None) should not be Vector.empty
    Validator.regex("abc".r)(Path.empty, Some(Some("def")), None) should not be Vector.empty
  }

  test("immutable validator") {
    Validator.immutable(Path.empty, Some("value"), Some("value")) should be (Vector.empty)
    Validator.immutable(Path.empty, Some("value"), None) should be (Vector.empty)
    Validator.immutable(Path.empty, None, Some("value")) should be (Vector.empty)
    Validator.immutable(Path.empty, Some("newValue"), Some("value")) should not be Vector.empty
  }
}
