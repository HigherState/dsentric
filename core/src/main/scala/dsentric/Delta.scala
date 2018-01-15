package dsentric

sealed trait DeltaValue[+T] {
  def toOption:Option[T] =
    this match {
      case DeltaRemove =>
        None
      case DeltaSet(t) =>
        Some(t)
    }
}

case object DeltaRemove extends DeltaValue[Nothing]

case class DeltaSet[+T](value:T) extends DeltaValue[T]