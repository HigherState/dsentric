import scala.collection.immutable.Iterable

package object dsentric {

  type Numeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type Length = String with Iterable[Nothing] with Vector[Nothing]
  type Optionable[T] = T with Option[T]
  type Collectable[T] = T with Option[T] with Set[T]

  type Failures = Vector[(Path, String)]


}
