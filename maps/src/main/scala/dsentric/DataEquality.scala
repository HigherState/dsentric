package dsentric

import dsentric.codecs.DCodec


//TODO look into Implicit Ordering on the DCodec
sealed trait DataEquality {
  def apply(x:Data, y:Data):Option[Boolean]

  def +(equality:DataEquality) =
    ComposeEquality(this, equality)
}

case class Compare2Equality[T](f:Int => Boolean)(implicit D:DCodec[T], O:Ordering[T]) extends DataEquality {
  def apply(x: Data, y: Data): Option[Boolean] =
    for {
      xs <- D.unapply(x.value)
      ys <- D.unapply(y.value)
    } yield f(O.compare(xs, ys))
}

case class ComposeEquality[T](left:DataEquality, right:DataEquality) extends DataEquality {
  def apply(x: Data, y: Data): Option[Boolean] =
    left(x, y)
      .orElse(right(x, y))
}
