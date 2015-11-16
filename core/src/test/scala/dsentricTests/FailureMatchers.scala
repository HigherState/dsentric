package dsentricTests

import org.scalatest.matchers.{MatchResult, Matcher}

import dsentric._

trait FailureMatchers {
  val failWith = new FailWord
}

final class FailWord {

  def apply(right: String): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val result = left match {
          case f:Failures @unchecked =>
            f.exists(_._2 == right)
          case _ => false
        }
        val message =
          if (result) ""
          else left match {
            case f:Failures @unchecked =>
               "expected failure not found"
            case _ =>
              "expected failure"
          }

        MatchResult(
          result,
          message,
          "should not have failed with " + right
        )
      }
      override def toString: String = "failWith (" + right.toString + ")"
    }

  def apply(right: Path): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val result = left match {
          case f:Failures @unchecked  =>
            f.exists(_._1 == right)
          case _ => false
        }
        val message =
          if (result) ""
          else left match {
            case f:Failures @unchecked  =>
              "expected failure at path not found"
            case _ =>
              "expected failure"
          }

        MatchResult(
          result,
          message,
          "should not have failed at " + right
        )
      }
      override def toString: String = "failWith (" + right.toString + ")"
    }

  def apply(right: (Path, String)): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val result = left match {
          case f:Failures @unchecked =>
            f.contains(right)
          case _ => false
        }
        val message =
          if (result) ""
          else left match {
            case f:Failures @unchecked  =>
              "expected failure not found"
            case _ =>
              "expected failure"
          }

        MatchResult(
          result,
          message,
          "should not have failed with " + right
        )
      }
      override def toString: String = "failWith (" + right.toString + ")"
    }
}
