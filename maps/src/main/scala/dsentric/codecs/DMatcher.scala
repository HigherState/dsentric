package dsentric.codecs

import dsentric.Data

//Enables pattern matching on Data types
final case class DMatcher[T]()(implicit DC: DCodec[T]) {
  def apply(value: T): Data           =
    Data(value)
  def unapply(value: Data): Option[T] =
    DC.unapply(value.value)
}