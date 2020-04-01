import scala.collection.immutable.Iterable

package object dsentric {

  type Numeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type Length[T] = String with Iterable[T] with Vector[T] with Set[T] with List[T] with Map[T, Nothing]
  type Optionable[T] = T with Option[T]
  type Collectable[T] = T with Option[T] with Set[T]

  type Failures = Vector[(Path, String)]


}
