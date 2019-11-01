package dsentric

import cats.data.NonEmptyList
import org.scalatest.enablers.Containing

object Implicits {

  implicit def nonEmptyListContaining[T]:Containing[NonEmptyList[T]] =
    new Containing[NonEmptyList[T]] {
      def contains(container: NonEmptyList[T], element: Any): Boolean =
        container.head == element || container.tail.contains(element)

      def containsOneOf(container: NonEmptyList[T], elements: Seq[Any]): Boolean =
        elements.exists(e => contains(container, e))

      def containsNoneOf(container: NonEmptyList[T], elements: Seq[Any]): Boolean =
        !elements.exists(e => contains(container, e))
    }
}
