package dsentricTests

import dsentric.{Validator, MaybePessimistic}
import org.scalatest.{Matchers, FunSuite}

/**
 * Created by Jamie Pullar on 26/10/2015.
 */
class ValidationTests extends FunSuite with Matchers {

  implicit val strictness = MaybePessimistic

  test("Numeric validators") {
    Validator.>(4)(Some(5), None) should be (Seq.empty)
    Validator.>(4)(Some(4), None) should not be Seq.empty
    Validator.>(4)(Some(3.5), None) should not be Seq.empty
    Validator.>(4)(Some(Some(12.5)), None) should be (Seq.empty)

    Validator.<(-2)(Some(-4), None) should be (Seq.empty)
    Validator.<(-2)(Some(-2), None) should not be Seq.empty
    Validator.<(-2.5)(Some(3), None) should not be Seq.empty

    Validator.>=(4.5)(Some(5), None) should be (Seq.empty)
    Validator.>=(4)(Some(4), None) should be (Seq.empty)
    Validator.>=(4.2)(Some(3.5), None) should not be Seq.empty

    Validator.<=(-2)(Some(-4), None) should be (Seq.empty)
    Validator.<=(-2)(Some(-2), None) should be (Seq.empty)
    Validator.<=(-2.5)(Some(3), None) should not be Seq.empty

    Validator.increment(Some(3), Some(4)) should not be Seq.empty
    Validator.increment(Some(3), Some(3)) should not be Seq.empty
    Validator.increment(Some(4), Some(3)) should be (Seq.empty)

    Validator.decrement(Some(4), Some(3)) should not be Seq.empty
    Validator.decrement(Some(3), Some(3)) should not be Seq.empty
    Validator.decrement(Some(3), Some(4)) should be (Seq.empty)
  }

  test("sequences") {
    Validator.minLength(4)(Some(List(1,2,3,4,5,6)), None) should be (Seq.empty)
    Validator.minLength(4)(Some(Seq(1,2)), None) should not be Seq.empty
    Validator.maxLength(12)(Some(Iterable(3,2,2,1)), None) should be (Seq.empty)
    Validator.maxLength(3)(Some(Iterable(3,2,2,1)), None) should not be Seq.empty
    Validator.maxLength(3)(Some("dashj"), None) should not be Seq.empty
  }

}
