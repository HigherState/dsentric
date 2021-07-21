package dsentric.contracts

import dsentric.codecs.DCodec
import dsentric.failure.ValidResult
import dsentric._

sealed trait PathSetter[D <: DObject] extends Function[D, D] {
  def ~[D0 >: D <: DObject](pathSetter: PathSetter[D0]): PathSetter[D] =
    CompositeSetter(this, pathSetter)

  def ~[D0 >: D <: DObject](pathSetter: ValidPathSetter[D0]): ValidPathSetter[D] =
    CompositeValidSetter(LiftedSetter(this), pathSetter)

  def ~+(kv: (String, Data)): PathSetter[D] =
    CompositeSetter(this, ValueSetter(Path(kv._1), kv._2.value))

  def ~+(path: Path, data: Data): PathSetter[D] =
    CompositeSetter(this, ValueSetter(path, data.value))

  def ~++(kv: Iterator[(String, Data)]): PathSetter[D] =
    CompositeSetter(this, ConcatSetter(kv.map(p => p._1 -> p._2.value)))

  def |>[D2 <: D](obj: D2): D2                         =
    applyAs(obj)

  private[contracts] def rawApply(rawObject: RawObject): RawObject

  private[contracts] def rawDelta(rawObject: RawObject): RawObject

  def apply(v1: D): D =
    v1.internalWrap(rawApply(v1.value)).asInstanceOf[D]

  def applyAs[D2 <: D](obj: D2): D2 =
    obj.internalWrap(rawApply(obj.value)).asInstanceOf[D2]

  def apply(delta: Delta): Delta =
    new DeltaInst(rawApply(delta.value))

  def asDelta(v1: D): Delta =
    new DeltaInst(rawDelta(v1.value))

  def lift: ValidPathSetter[D] =
    LiftedSetter(this)
}

sealed trait ValidPathSetter[D <: DObject] extends Function[D, ValidResult[D]] {
  def ~[D0 >: D <: DObject](pathSetter: PathSetter[D0]): ValidPathSetter[D] =
    CompositeValidSetter(this, LiftedSetter(pathSetter))

  def ~[D0 >: D <: DObject](pathSetter: ValidPathSetter[D0]): ValidPathSetter[D] =
    CompositeValidSetter(this, pathSetter)

  def ~+(kv: (String, Data)): ValidPathSetter[D] =
    CompositeValidSetter(this, LiftedSetter(ValueSetter(Path(kv._1), kv._2.value)))

  def ~+(path: Path, data: Data): ValidPathSetter[D] =
    CompositeValidSetter(this, LiftedSetter(ValueSetter(path, data.value)))

  def ~++(kv: Iterator[(String, Data)]): ValidPathSetter[D] =
    CompositeValidSetter(this, LiftedSetter(ConcatSetter(kv.map(p => p._1 -> p._2.value))))

  def |>[D2 <: D](obj: D2): ValidResult[D2]                 =
    applyAs(obj)

  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject]

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject]

  def apply(v1: D): ValidResult[D] =
    rawApply(v1.value).map(result => v1.internalWrap(result).asInstanceOf[D])

  def applyAs[D2 <: D](v1: D2): ValidResult[D2] =
    rawApply(v1.value).map(result => v1.internalWrap(result).asInstanceOf[D2])

  def apply(delta: Delta): ValidResult[Delta] =
    rawApply(delta.value).map(result => new DeltaInst(result))

  def asDelta(v1: D): ValidResult[Delta] =
    rawApply(v1.value).map(result => new DeltaInst(result))
}

private case class IdentitySetter[D <: DObject]() extends PathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): RawObject =
    rawObject
  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    RawObject.empty
  override def apply(v1: D): D                                     = v1
  override def apply(delta: Delta): Delta                          = delta
}

private case class IdentityValidSetter[D <: DObject]() extends ValidPathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    ValidResult.success(rawObject)
  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    ValidResult.empty
  override def apply(v1: D): ValidResult[D]                                     = ValidResult.success(v1)
  override def apply(delta: Delta): ValidResult[Delta]                          = ValidResult.success(delta)
}

final private case class LiftedSetter[D <: DObject](pathSetter: PathSetter[D]) extends ValidPathSetter[D] {
  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    ValidResult.success(pathSetter.rawApply(rawObject))

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    ValidResult.success(pathSetter.rawDelta(rawObject))
}

final private case class ValueSetter[D <: DObject](path: Path, value: Raw) extends PathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): RawObject =
    PathLensOps.set(rawObject, path, value)

  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    PathLensOps.pathToMap(path, value)
}

final private case class ConcatSetter[D <: DObject](values: Iterator[(String, Raw)]) extends PathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): RawObject =
    rawObject ++ values

  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    values.filterNot(p => rawObject.get(p._1).contains(p._2)).toMap
}

final private case class ValueIfEmptySetter[D <: DObject](path: Path, value: Raw) extends PathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): RawObject =
    PathLensOps.traverse(rawObject, path) match {
      case None =>
        PathLensOps.set(rawObject, path, value)
      case _    =>
        rawObject
    }

  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    PathLensOps.traverse(rawObject, path) match {
      case None =>
        PathLensOps.pathToMap(path, value)
      case _    =>
        RawObject.empty
    }
}

final private case class ValidValueSetter[D <: DObject](path: Path, value: ValidResult[Raw])
    extends ValidPathSetter[D] {
  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    value.map(v => PathLensOps.set(rawObject, path, v))

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    value.map(v => PathLensOps.pathToMap(path, v))
}

//If object is empty then drops the value
final private case class ValidObjectSetter[D <: DObject](path: Path, value: ValidResult[RawObject])
    extends ValidPathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    value.map {
      case r if r.isEmpty =>
        PathLensOps.drop(rawObject, path).getOrElse(rawObject)
      case r              =>
        PathLensOps.set(rawObject, path, r)
    }

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    value.map(PathLensOps.pathToMap(path, _))
}

final private case class ValueDrop[D <: DObject](path: Path) extends PathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): RawObject =
    PathLensOps.drop(rawObject, path).getOrElse(rawObject)

  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    Setter.deltaIgnoreOrNull(rawObject, path)
}

final private case class ValidDrop[D <: DObject](validPath: ValidResult[Path]) extends ValidPathSetter[D] {
  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    validPath.map(path => PathLensOps.drop(rawObject, path).getOrElse(rawObject))

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    validPath.map(Setter.deltaIgnoreOrNull(rawObject, _))
}

final private case class CompositeSetter[D <: DObject, D0 >: D <: DObject](
  leftSetter: PathSetter[D],
  rightSetter: PathSetter[D0]
) extends PathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): RawObject =
    rightSetter.rawApply(leftSetter.rawApply(rawObject))

  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    RawObjectOps.traverseConcat(leftSetter.rawDelta(rawObject), rightSetter.rawDelta(rawObject))

}

final private case class CompositeValidSetter[D <: DObject, D0 >: D <: DObject](
  leftSetter: ValidPathSetter[D],
  rightSetter: ValidPathSetter[D0]
) extends ValidPathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    leftSetter.rawApply(rawObject).flatMap(rightSetter.rawApply)

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    for {
      left  <- leftSetter.rawDelta(rawObject)
      right <- rightSetter.rawDelta(rawObject)
    } yield RawObjectOps.traverseConcat(left, right)

}
/*
Option on f Raw result is case of codec failure
 */
final private case class ModifySetter[D <: DObject, T](
  getter: RawObject => ValidResult[Option[T]],
  f: T => T,
  codec: DCodec[T],
  path: Path
) extends ValidPathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject).map {
      case None         => rawObject
      case Some(result) =>
        Setter(rawObject, codec(f(result)), path)
    }

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject).map {
      case None         =>
        RawObject.empty
      case Some(result) =>
        Setter.deltaApply(rawObject, codec(f(result)), path)
    }
}

final private case class ModifyValidSetter[D <: DObject, T](
  getter: RawObject => ValidResult[Option[T]],
  f: T => ValidResult[T],
  codec: DCodec[T],
  path: Path
) extends ValidPathSetter[D] {

  private[contracts] def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject).flatMap {
      case None         =>
        ValidResult.success(rawObject)
      case Some(result) =>
        f(result).map(r => Setter(rawObject, codec(r), path))
    }

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject).flatMap {
      case None         =>
        ValidResult.empty
      case Some(result) =>
        f(result).map(r => Setter.deltaApply(rawObject, codec(r), path))
    }
}

/*
Option on f Raw result is case of codec failure
 */
final private case class TraversedModifySetter[D <: DObject, T](
  getter: RawObject => MaybeAvailable[T],
  f: Option[T] => T,
  codec: DCodec[T],
  path: Path
) extends ValidPathSetter[D] {

  def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case PathEmptyMaybe  => ValidResult.success(rawObject)
      case NotFound        => ValidResult.success(Setter(rawObject, codec(f(None)), path))
      case Found(t)        => ValidResult.success(Setter(rawObject, codec(f(Some(t))), path))
      case Failed(f, tail) => ValidResult.failure(f, tail)
    }

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case PathEmptyMaybe  =>
        ValidResult.empty
      case NotFound        =>
        ValidResult.success(Setter.deltaApply(rawObject, codec(f(None)), path))
      case Found(t)        =>
        ValidResult.success(Setter.deltaApply(rawObject, codec(f(Some(t))), path))
      case Failed(f, tail) =>
        ValidResult.failure(f, tail)
    }
}

final private case class TraversedModifyValidSetter[D <: DObject, T](
  getter: RawObject => MaybeAvailable[T],
  f: Option[T] => ValidResult[T],
  codec: DCodec[T],
  path: Path
) extends ValidPathSetter[D] {

  def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case PathEmptyMaybe  =>
        ValidResult.success(rawObject)
      case NotFound        =>
        f(None).map(r => Setter(rawObject, codec(r), path))
      case Found(t)        =>
        f(Some(t)).map(r => Setter(rawObject, codec(r), path))
      case Failed(f, tail) =>
        ValidResult.failure(f, tail)
    }

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case PathEmptyMaybe  =>
        ValidResult.empty
      case NotFound        =>
        f(None).map(r => Setter.deltaApply(rawObject, codec(r), path))
      case Found(t)        =>
        f(Some(t)).map(r => Setter.deltaApply(rawObject, codec(r), path))
      case Failed(f, tail) =>
        ValidResult.failure(f, tail)
    }
}

final private case class RawTraversedModifyValidSetter[D <: DObject](
  getter: RawObject => MaybeAvailable[RawObject],
  f: RawObject => ValidResult[RawObject],
  path: Path
) extends ValidPathSetter[D] {

  def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case NotFound | PathEmptyMaybe =>
        ValidResult.success(rawObject)
      case Found(t)                  =>
        f(t).map(Setter.apply(rawObject, _, path))
      case Failed(f, tail)           =>
        ValidResult.failure(f, tail)
    }

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case NotFound | PathEmptyMaybe =>
        ValidResult.empty
      case Found(t)                  =>
        f(t).map(r => Setter.deltaApply(rawObject, r, path))
      case Failed(f, tail)           =>
        ValidResult.failure(f, tail)
    }
}

/**
 * Doesnt do anything if path doesnt lead to a Raw Object
 * @param getter
 * @param f
 * @param path
 * @tparam D
 */
final private case class RawTraversedIgnoredModifySetter[D <: DObject](f: RawObject => RawObject, path: Path)
    extends PathSetter[D] {

  def rawApply(rawObject: RawObject): RawObject =
    PathLensOps
      .traverseObject(rawObject, path)
      .fold(rawObject) { r =>
        Setter.apply(rawObject, f(r), path)
      }

  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    PathLensOps
      .traverseObject(rawObject, path)
      .fold(RawObject.empty) { r =>
        Setter.deltaApply(rawObject, f(r), path)
      }
}

/*
First Option on f Raw result is case of codec failure
 */
final private case class TraversedModifyOrDropSetter[D <: DObject, T](
  getter: RawObject => MaybeAvailable[T],
  f: Option[T] => Option[T],
  codec: DCodec[T],
  path: Path
) extends ValidPathSetter[D] {

  def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case PathEmptyMaybe  =>
        ValidResult.success(rawObject)
      case NotFound        =>
        ValidResult.success {
          f(None).fold(PathLensOps.drop(rawObject, path).getOrElse(rawObject)) { r =>
            Setter(rawObject, codec(r), path)
          }
        }
      case Found(t)        =>
        ValidResult.success {
          f(Some(t)).fold(PathLensOps.drop(rawObject, path).getOrElse(rawObject)) { r =>
            Setter(rawObject, codec(r), path)
          }
        }
      case Failed(f, tail) =>
        ValidResult.failure(f, tail)
    }

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case PathEmptyMaybe =>
        ValidResult.empty
      case NotFound       =>
        f(None) match {
          case None    =>
            ValidResult.success(Setter.deltaIgnoreOrNull(rawObject, path))
          case Some(r) =>
            ValidResult.success(Setter.deltaApply(rawObject, codec(r), path))
        }
      case Found(_)       =>
        f(None) match {
          case None    =>
            ValidResult.success(Setter.deltaIgnoreOrNull(rawObject, path))
          case Some(r) =>
            ValidResult.success(Setter.deltaApply(rawObject, codec(r), path))
        }

      case Failed(f, tail) =>
        ValidResult.failure(f, tail)
    }
}

final private case class TraversedModifyOrDropValidSetter[D <: DObject, T](
  getter: RawObject => MaybeAvailable[T],
  f: Option[T] => Option[ValidResult[T]],
  codec: DCodec[T],
  path: Path
) extends ValidPathSetter[D] {

  def rawApply(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case PathEmptyMaybe  =>
        ValidResult.success(rawObject)
      case NotFound        =>
        f(None) match {
          case None     =>
            ValidResult.success(PathLensOps.drop(rawObject, path).getOrElse(rawObject))
          case Some(vr) =>
            vr.map(t => Setter(rawObject, codec(t), path))
        }
      case Found(t)        =>
        f(Some(t)) match {
          case None     =>
            ValidResult.success(PathLensOps.drop(rawObject, path).getOrElse(rawObject))
          case Some(vr) =>
            vr.map(t => Setter(rawObject, codec(t), path))
        }
      case Failed(f, tail) =>
        ValidResult.failure(f, tail)
    }

  private[contracts] def rawDelta(rawObject: RawObject): ValidResult[RawObject] =
    getter(rawObject) match {
      case PathEmptyMaybe  =>
        ValidResult.empty
      case NotFound        =>
        f(None) match {
          case None     =>
            ValidResult.success(Setter.deltaIgnoreOrNull(rawObject, path))
          case Some(vr) =>
            vr.map(t => Setter.deltaApply(rawObject, codec(t), path))
        }
      case Found(t)        =>
        f(Some(t)) match {
          case None     =>
            ValidResult.success(Setter.deltaIgnoreOrNull(rawObject, path))
          case Some(vr) =>
            vr.map(t => Setter.deltaApply(rawObject, codec(t), path))
        }
      case Failed(f, tail) =>
        ValidResult.failure(f, tail)
    }
}

final case class SelectPathSetter[D <: DObject](projection: DProjection) extends PathSetter[D] {
  private[contracts] def rawApply(rawObject: RawObject): RawObject =
    RawObjectOps.selectMap(rawObject, projection.value)

  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    ???
}

final case class OmitPathSetter[D <: DObject](projection: DProjection) extends PathSetter[D] {
  private[contracts] def rawApply(rawObject: RawObject): RawObject =
    RawObjectOps.omitMap(rawObject, projection.value)

  private[contracts] def rawDelta(rawObject: RawObject): RawObject =
    ???
}

final case class CustomPathSetter[D <: DObject](_rawApply: RawObject => RawObject, _rawDelta: RawObject => RawObject)
    extends PathSetter[D] {
  private[contracts] def rawApply(rawObject: RawObject): RawObject = _rawApply(rawObject)

  private[contracts] def rawDelta(rawObject: RawObject): RawObject = _rawDelta(rawObject)
}

private[contracts] object Setter {

  def apply[T](obj: RawObject, value: Raw, path: Path): RawObject =
    value match {
      case r: RawObject @unchecked if path.isEmpty =>
        r
      case r: RawObject @unchecked if r.isEmpty    =>
        PathLensOps.drop(obj, path).getOrElse(Map.empty)
      case r                                       =>
        PathLensOps.set(obj, path, r)
    }

  /**
   * Traverses the obj to compare the new Delta entry
   * We want to traverse as we cant guarantee the target and destination paths were the same
   * @param obj
   * @param value
   * @param path
   * @return
   */
  def deltaApply(obj: RawObject, value: Raw, path: Path): RawObject =
    PathLensOps.traverse(obj, path) -> value match {
      case (Some(r), v) if r == v                                   =>
        RawObject.empty
      case (Some(_), v: RawObject @unchecked) if v.isEmpty          =>
        PathLensOps.pathToMap(path, DNull)
      case (None, v: RawObject @unchecked) if v.isEmpty             =>
        RawObject.empty
      case (Some(r: RawObject @unchecked), v: RawObject @unchecked) =>
        RawObjectOps
          .calculateDelta(r -> v)
          .map(d => PathLensOps.pathToMap(path, d))
          .getOrElse(RawObject.empty)
      case _                                                        =>
        PathLensOps.pathToMap(path, value)
    }

  def deltaIgnoreOrNull(obj: RawObject, path: Path): RawObject =
    if (path.isEmpty) RawObject.empty
    else if (PathLensOps.traverse(obj, path).isEmpty)
      RawObject.empty
    else
      PathLensOps.pathToMap(path, DNull)

}
