package dsentric

package object operators {

  type Numeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type Length = String with Iterable[Nothing] with Vector[Nothing] with DArray with Map[String, Nothing] with DObject
  type Optionable[T] = T with Option[T]
  type Keyable = DObject with Map[String, Nothing]

}
