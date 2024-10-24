package dsentric

package object operators {

  type Numeric = Long & Int & Float & Double & Option[Long] & Option[Int] & Option[Float] & Option[Double]
  type Length = String & Iterable[Nothing] & Vector[Nothing] & DArray & Map[String, Nothing] & DObject
  type Optionable[T] = T & Option[T]
  type Keyable = DObject & Map[String, Nothing]

}
