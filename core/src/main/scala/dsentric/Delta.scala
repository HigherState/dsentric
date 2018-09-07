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

sealed trait DeltaDefaultValue[+T] {
  def getValue:T =
    this match {
      case DeltaDefaultReset(t) =>
        t
      case DeltaDefaultSet(t) =>
        t
    }

  def toOption:Option[T] =
    this match {
      case DeltaDefaultReset(_) =>
        None
      case DeltaDefaultSet(t) =>
        Some(t)
    }
}

case object DeltaRemove extends DeltaValue[Nothing]

final case class DeltaSet[+T](value:T) extends DeltaValue[T]


final case class DeltaDefaultReset[+T](value:T) extends DeltaDefaultValue[T]

final case class DeltaDefaultSet[+T](value:T) extends DeltaDefaultValue[T]
