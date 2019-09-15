package object dsentric {

  type Raw = Any
  type RawObject = Map[String, Any]
  type RawArray = Vector[Any]

  type Numeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type Length = String with Iterable[Nothing] with Vector[Nothing]
  type Optionable[T] = T with Option[T]

  type PathFailures = Vector[(Path, String)]
}
