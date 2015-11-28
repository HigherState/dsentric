package object dsentric {

  type Numeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type Length = String with Iterable[Nothing]
  type Optionable[T] = T with Option[T]

  type Path = Vector[Either[Int, String]]
  type Failures = Vector[(Path, String)]

  def idFunc[Data]:Data => Data =
    (d:Data) => d

  implicit val strictness = MaybeOptimistic
}
