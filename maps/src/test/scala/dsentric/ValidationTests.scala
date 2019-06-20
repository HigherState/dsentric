package dsentric

import dsentric._
import org.scalatest.{FunSuite, Matchers}
import PessimisticCodecs._

class ValidationTests extends FunSuite with Matchers {

  test("Numeric validators") {
    Validators.>(4)(Path.empty, Some(5), None) should be (Vector.empty)
    Validators.>(4)(Path.empty, Some(4), None) should not be Vector.empty
    Validators.>(4)(Path.empty, Some(3.5), None) should not be Vector.empty
    Validators.>(4)(Path.empty, Some(Some(12.5)), None) should be (Vector.empty)

    Validators.<(-2)(Path.empty, Some(-4), None) should be (Vector.empty)
    Validators.<(-2)(Path.empty, Some(-2), None) should not be Vector.empty
    Validators.<(-2.5)(Path.empty, Some(3), None) should not be Vector.empty

    Validators.>=(4.5)(Path.empty, Some(5), None) should be (Vector.empty)
    Validators.>=(4)(Path.empty, Some(4), None) should be (Vector.empty)
    Validators.>=(4.2)(Path.empty, Some(3.5), None) should not be Vector.empty

    Validators.<=(-2)(Path.empty, Some(-4), None) should be (Vector.empty)
    Validators.<=(-2)(Path.empty, Some(-2), None) should be (Vector.empty)
    Validators.<=(-2.5)(Path.empty, Some(3), None) should not be Vector.empty

    Validators.increment(Path.empty, Some(3), Some(4)) should not be Vector.empty
    Validators.increment(Path.empty, Some(3), Some(3)) should not be Vector.empty
    Validators.increment(Path.empty, Some(4), Some(3)) should be (Vector.empty)

    Validators.decrement(Path.empty, Some(4), Some(3)) should not be Vector.empty
    Validators.decrement(Path.empty, Some(3), Some(3)) should not be Vector.empty
    Validators.decrement(Path.empty, Some(3), Some(4)) should be (Vector.empty)
  }

  test("Length validators") {
    Validators.minLength(4)(Path.empty, Some(List(1,2,3,4,5,6)), None) should be (Vector.empty)
    Validators.minLength(4)(Path.empty, Some(Vector(1,2)), None) should not be Vector.empty
    Validators.maxLength(12)(Path.empty, Some(Some(Iterable(3,2,2,1))), None) should be (Vector.empty)
    Validators.maxLength(3)(Path.empty, Some(Iterable(3,2,2,1)), None) should not be Vector.empty
    Validators.maxLength(3)(Path.empty, Some("dashj"), None) should not be Vector.empty
  }

  test("in/nin validators") {
    Validators.in("one", "two", "three").apply(Path.empty, Some("one"), None) should be (Vector.empty)
    Validators.in("one", "two", "three").apply(Path.empty, Some(Some("two")), None) should be (Vector.empty)
    Validators.in("one", "two", "three").apply(Path.empty, Some("four"), None) should not be Vector.empty

    Validators.nin(1,2,3).apply(Path.empty, Some(4), None) should be (Vector.empty)
    Validators.nin(1,2,3).apply(Path.empty, Some(2), None) should not be Vector.empty
  }

  test("regex validator") {
    Validators.regex("abc".r)(Path.empty, Some("abc"), None) should be (Vector.empty)
    Validators.regex("abc".r)(Path.empty, Some("def"), None) should not be Vector.empty
    Validators.regex("abc".r)(Path.empty, Some(Some("def")), None) should not be Vector.empty
  }

  test("immutable validator") {
    Validators.immutable(Path.empty, Some("value"), Some("value")) should be (Vector.empty)
    Validators.immutable(Path.empty, Some("value"), None) should be (Vector.empty)
    Validators.immutable(Path.empty, None, Some("value")) should be (Vector.empty)
    Validators.immutable(Path.empty, Some("newValue"), Some("value")) should not be Vector.empty
  }

  test("keyMatcher") {
    import PessimisticCodecs._
    import dsentric.Dsentric._
    val r = "[a-z]*".r
    val validator = Validators.keyValidator(r, "Invalid Key")

    validator(Path.empty, Some(DObject("key" := 1)), None) should be (Vector.empty)
    validator(Path.empty, Some(DObject("key2" := 1)), None) should not be Vector.empty

  }
}
