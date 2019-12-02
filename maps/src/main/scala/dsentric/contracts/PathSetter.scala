package dsentric.contracts

import cats.data.NonEmptyList
import dsentric.failure.{Failure, ValidResult}
import dsentric.{DObject, Path, PathLensOps, Raw, RawObject}

sealed trait PathSetter[D <: DObject] extends Function[D, D] {
  def ~(pathSetter:PathSetter[D]):PathSetter[D] =
    CompositeSetter(this, pathSetter)

  def ~(pathSetter:ValidPathSetter[D]):ValidPathSetter[D] =
    CompositeValidSetter(LiftedSetter(this), pathSetter)

  def apply(v1: D): D

  @inline
  final protected def asD(d:DObject):D = d.asInstanceOf[D]

  def lift:ValidPathSetter[D] =
    LiftedSetter(this)
}

sealed trait ValidPathSetter[D <: DObject] extends Function[D, ValidResult[D]] {
  def ~(pathSetter:PathSetter[D]):ValidPathSetter[D] =
    CompositeValidSetter(this, LiftedSetter(pathSetter))

  @inline
  final protected def asD(d:DObject):D = d.asInstanceOf[D]

  def apply(v1: D): ValidResult[D]
}

private case class IdentitySetter[D <: DObject]() extends PathSetter[D] {
  def apply(v1:D): D = v1
}

private case class IdentityValidSetter[D <: DObject]() extends ValidPathSetter[D] {
  def apply(v1:D): ValidResult[D] = Right(v1)
}

private final case class LiftedSetter[D <: DObject](pathSetter:PathSetter[D]) extends ValidPathSetter[D] {
  def apply(v1: D): ValidResult[D] =
    ValidResult.success(pathSetter(v1))
}

private final case class ValueSetter[D <: DObject](path:Path, value:Raw) extends PathSetter[D] {

  def apply(v1: D): D =
    asD(v1.internalWrap(PathLensOps.set(v1.value, path, value)))

}

private final case class ValidValueSetter[D <: DObject](path:Path, value:ValidResult[Raw]) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    value.map(r => asD(v1.internalWrap(PathLensOps.set(v1.value, path, r))))

}

private final case class VerifyValueSetter[D <: DObject](path:Path, value:Raw, verify:D => List[Failure]) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] = {
    val r = asD(v1.internalWrap(PathLensOps.set(v1.value, path, value)))
    verify(r) match {
      case head :: tail =>
        Left(NonEmptyList(head, tail))
      case Nil =>
        Right(r)
    }
  }

}

private final case class ValueDrop[D <: DObject](path:Path) extends PathSetter[D] {

  def apply(v1: D): D =
    PathLensOps.drop(v1.value, path).fold(v1)(r => asD(v1.internalWrap(r)))
}

private final case class CompositeSetter[D <: DObject](leftSetter:PathSetter[D], rightSetter:PathSetter[D]) extends PathSetter[D] {

  def apply(v1: D): D =
    rightSetter(leftSetter(v1))
}

private final case class CompositeValidSetter[D <: DObject](leftSetter:ValidPathSetter[D], rightSetter:ValidPathSetter[D]) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    leftSetter(v1).flatMap(rightSetter)
}
/*
Option on f Raw result is case of codec failure
 */
private final case class ModifySetter[D <: DObject, T](getter:D => ValidResult[Option[T]], f:T => T, setter:(D, T) => D) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    getter(v1)
      .map{
        case None => v1
        case Some(t) =>
          setter(v1, f(t))
      }
}

private final case class ModifyValidSetter[D <: DObject, T](getter:D => ValidResult[Option[T]], f:T => ValidResult[T], setter:(D, T) => D) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    getter(v1)
      .flatMap{
        case None =>
          Right(v1)
        case Some(t) =>
          f(t).map(t => setter(v1, t))
      }
}

private final case class RawModifySetter[D <: DObject, T](modifier:D => ValidResult[Raw],  path:Path) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    modifier(v1).map{r =>
      asD(v1.internalWrap(PathLensOps.set(v1.value, path, r)))
    }
}

private final case class RawModifyOrDropSetter[D <: DObject, T](modifier:D => ValidResult[Option[Raw]],  path:Path) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    modifier(v1).map{
      case None =>
        asD(v1.internalWrap(PathLensOps.drop(v1.value, path).getOrElse(RawObject.empty)))
      case Some(r) =>
        asD(v1.internalWrap(PathLensOps.set(v1.value, path, r)))
    }
}

private final case class RawModifyOrIgnoreSetter[D <: DObject, T](modifier:D => ValidResult[Option[Raw]],  path:Path) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    modifier(v1).map{
      case None =>
        v1
      case Some(r) =>
        asD(v1.internalWrap(PathLensOps.set(v1.value, path, r)))
    }
}

private final case class RawModifyDropOrIgnoreSetter[D <: DObject, T](modifier:D => ValidResult[Option[Option[Raw]]],  path:Path) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    modifier(v1).map{
      case None =>
        v1
      case Some(None) =>
        asD(v1.internalWrap(PathLensOps.drop(v1.value, path).getOrElse(RawObject.empty)))
      case Some(Some(r)) =>
        asD(v1.internalWrap(PathLensOps.set(v1.value, path, r)))
    }
}

/*
Option on f Raw result is case of codec failure
 */
private final case class MaybeModifySetter[D <: DObject, T](getter:D => ValidResult[Option[T]], f:Option[T] => T, setter:(D, T) => D) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    getter(v1).map(maybeT => setter(v1, f(maybeT)))
}

/*
First Option on f Raw result is case of codec failure
 */
private final case class ModifyOrDropSetter[D <: DObject, T](getter:D => ValidResult[Option[T]], f:Option[T] => Option[T], setter:(D, Option[T]) => D) extends ValidPathSetter[D] {

  def apply(v1: D): ValidResult[D] =
    getter(v1).map(maybeT => setter(v1, f(maybeT)))
}