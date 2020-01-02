package dsentric

import cats.data.NonEmptyList
import org.scalatest.enablers.{Aggregating, Containing}
import scala.collection.GenTraversable

object Implicits {

  implicit def nonEmptyListContaining[T]:Containing[NonEmptyList[T]] =
    new Containing[NonEmptyList[T]] {
      def contains(container: NonEmptyList[T], element: Any): Boolean =
        container.head == element || container.tail.contains(element)

      def containsOneOf(container: NonEmptyList[T], elements: collection.Seq[Any]): Boolean =
        elements.exists(e => contains(container, e))

      def containsNoneOf(container: NonEmptyList[T], elements: collection.Seq[Any]): Boolean =
        !elements.exists(e => contains(container, e))
    }

  implicit def nonEmptyListAggregating[T]:Aggregating[NonEmptyList[T]] =
    new Aggregating[NonEmptyList[T]] {
      def containsAtLeastOneOf(aggregation: NonEmptyList[T], eles: collection.Seq[Any]): Boolean =
        eles.exists(e => aggregation.head == e || aggregation.tail.contains(e))

      @deprecated("Update when EitherValues is updated and undeprecate.", "")
      def containsTheSameElementsAs(leftAggregation: NonEmptyList[T], rightAggregation: GenTraversable[Any]): Boolean =
        if (leftAggregation.size == rightAggregation.size) {
          leftAggregation.toList.zip(rightAggregation).forall(p => p._1 == p._2)
        }
        else false

      def containsOnly(aggregation: NonEmptyList[T], eles: collection.Seq[Any]): Boolean =
        aggregation.size == eles.size && containsAllOf(aggregation, eles)

      def containsAllOf(aggregation: NonEmptyList[T], eles: collection.Seq[Any]): Boolean =
        eles.forall(e => aggregation.head == e || aggregation.tail.contains(e))

      def containsAtMostOneOf(aggregation: NonEmptyList[T], eles: collection.Seq[Any]): Boolean =
        eles.foldLeft(0){
          case (2, _) => 2
          case (i, e) =>
            if (aggregation.head == e || aggregation.tail.contains(e)) i + 1
            else i
        } < 2

    }
}
