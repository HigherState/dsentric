package dsentric

sealed trait PathSetter[D <: DObject] extends Function[D, D] {
  def ~(pathSetter:PathSetter[D]):PathSetter[D] =
    CompositeSetter(this, pathSetter)

  def apply(v1: D): D =
    set(v1).asInstanceOf[D]

  def applyAs[G <: D](v1:G): G =
    set(v1).asInstanceOf[G]

  def set:DObject => DObject
}

private case class ValueSetter[T, D <: DObject](path:Path, value:Any) extends PathSetter[D] {

  def set:DObject => DObject =
    v1 => v1.internalWrap(PathLensOps.set(v1.value, path, value))

}

private case class ValueIfEmptySetter[T, D <: DObject](path:Path, value:Any) extends PathSetter[D] {

  def set:DObject => DObject =
    v1 => PathLensOps.setIfEmpty(v1.value, path, value).fold(v1)(v1.internalWrap)

}

private case class MaybeValueSetter[T, D <: DObject](path:Path, value:Option[Any]) extends PathSetter[D] {

  def set:DObject => DObject =
    v1 => value.fold(v1)(v => v1.internalWrap(PathLensOps.set(v1.value, path, v)))
}

private case class MaybeValueIfEmptySetter[T, D <: DObject](path:Path, value:Option[Any]) extends PathSetter[D] {

  def set:DObject => DObject =
    v1 => value.fold(v1)(v => PathLensOps.setIfEmpty(v1.value, path, v).fold(v1)(v1.internalWrap))
}

private case class ValueDrop[D <: DObject](path:Path) extends PathSetter[D] {

  def set:DObject => DObject =
    v1 => PathLensOps.drop(v1.value, path).fold(v1)(v1.internalWrap)
}

private case class CompositeSetter[D <: DObject](leftSetter:PathSetter[D], rightSetter:PathSetter[D]) extends PathSetter[D] {

  def set:DObject => DObject =
    v1 => rightSetter.set(leftSetter.set(v1))
}

private case class ModifySetter[T, D <: DObject](path:Path, f:T => T, codec:DCodec[T]) extends PathSetter[D] {

  def set:DObject => DObject=
    v1 => PathLensOps.modify(v1.value, path, codec, f).fold(v1)(v1.internalWrap)
}

private case class MaybeModifySetter[T, D <: DObject](path:Path, f:Option[T] => T, codec:DCodec[T], strictness:Strictness) extends PathSetter[D] {
  def set:DObject => DObject=
    v1 => PathLensOps.maybeModify(v1.value, path, codec, strictness, f).fold(v1)(v1.internalWrap)
}

private case class ModifyOrDropSetter[T, D <: DObject](path:Path, f:Option[T] => Option[T], codec:DCodec[T], strictness:Strictness) extends PathSetter[D] {
  def set:DObject => DObject =
    v1 => PathLensOps.maybeModifyOrDrop(v1.value, path, codec, strictness, f).fold(v1)(v1.internalWrap)
}



sealed trait PropertyLens[D <: DObject, T] {

  def _path:Path
  private[dsentric] def _codec: DCodec[T]

  private[dsentric] def _strictGet(data:D):Option[Option[T]]

  def $set(value:T):PathSetter[D] =
    ValueSetter(_path, _codec(value).value)

  def $setIfEmpty(value:T):PathSetter[D] =
    ValueIfEmptySetter(_path, _codec(value).value)

  def $maybeSet(value:Option[T]):PathSetter[D] =
    MaybeValueSetter(_path, value.map(_codec(_).value))

  def $maybeSetIfEmpty(value:Option[T]):PathSetter[D] =
    MaybeValueIfEmptySetter(_path, value.map(_codec(_).value))
}

trait ExpectedLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T] {

  def $get(data:D):Option[T] =
    PathLensOps
      .traverse(data.value, _path)
      .flatMap(_codec.unapply)

  def $getOrElse(data:D, default: => T):T =
    $get(data).getOrElse(default)

  def $modify(f:T => T):PathSetter[D] =
    ModifySetter(_path, f, _codec)

  def $copy(p:PropertyLens[D, T]):D => D =
    d => {
      p._strictGet(d).flatten.fold(d){p =>
        $set(p)(d)
      }
    }

  def $maybeCopy(p:MaybeLens[D, T]):D => D =
    d => {
      p._strictGet(d).flatten.fold(d){p =>
        $set(p)(d)
      }
    }

  private[dsentric] def _strictDeltaGet(data:D):Option[Option[T]] =
    PathLensOps
      .traverse(data.value, _path) match {
      case None =>
        Some(None)
      case Some(DNull) =>
        None // Not allowed
      case Some(v) =>
        _codec.unapply(v).map(Some(_))
    }

  def $forceDrop: PathSetter[D] =
    ValueDrop(_path)

  //both empty or wrong value are bad values
  private[dsentric] def _strictGet(data:D):Option[Option[T]] =
    $get(data).map(v => Some(v))
}

trait MaybeLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, Option[T]] {

  private[dsentric] def _strictness:Strictness

  def $get(data:D):Option[T] =
    PathLensOps
      .traverse(data.value, _path)
      .flatMap(_codec.unapply)

  def $getOrElse(data:D, default: => T):T =
    $get(data).getOrElse(default)

  def $deltaGet(data:D):Option[DeltaValue[T]] =
    PathLensOps
      .traverse(data.value, _path)
      .flatMap{
        case DNull =>
          Some(DeltaRemove)
        case v =>
          _codec.unapply(v).map(t => DeltaSet(t))
      }

  def $modify(f:Option[T] => T):PathSetter[D] =
    MaybeModifySetter(_path, f, _codec, _strictness)

  def $modifyOrDrop(f:Option[T] => Option[T]):PathSetter[D] =
    ModifyOrDropSetter(_path, f, _codec, _strictness)

  def $drop: PathSetter[D] =
    ValueDrop(_path)

  def $setOrDrop(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v).value))

  def $copy(p:PropertyLens[D, T]):D => D =
    (d) => {
      p._strictGet(d)
        .fold(d)(v => $setOrDrop(v)(d))
    }

  def $setNull: PathSetter[D] =
    ValueSetter(_path, DNull)

  private[dsentric] def _strictGet(data:D):Option[Option[T]] =
    PathLensOps
      .traverse(data.value, _path) match {
        case None => Some(None)
        case Some(v) => _strictness(v, _codec)
    }

  private[dsentric] def _strictDeltaGet(data:D):Option[Option[DeltaValue[T]]] =
    PathLensOps
      .traverse(data.value, _path) match {
        case None =>
          Some(None)
        case Some(DNull) =>
          Some(Some(DeltaRemove))
        case Some(v) => _strictness(v, _codec).map(_.map(DeltaSet(_)))
      }

}

trait DefaultLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T]{

  def _default:T

  private[dsentric] def _strictness:Strictness

  private val toDefault =
    (maybe:Option[T]) => maybe.getOrElse(_default)

  def $get(data:D):T =
    PathLensOps
      .traverse(data.value, _path)
      .fold(_default) { t =>
        _codec.unapply(t).getOrElse(_default)
      }


  def $deltaGet(data:D):DeltaDefaultValue[T] =
    PathLensOps
      .traverse(data.value, _path)
      .fold[DeltaDefaultValue[T]](DeltaDefaultReset(_default)) {
        case DNull =>
          DeltaDefaultReset(_default)
        case v =>
          _codec.unapply(v).fold[DeltaDefaultValue[T]](DeltaDefaultReset(_default))(t => DeltaDefaultSet(t))
      }

  def $restore: PathSetter[D] =
    ValueDrop(_path)

  def $modify(f:T => T):PathSetter[D] =
    MaybeModifySetter(_path, toDefault.andThen(f), _codec, _strictness)


  def $setOrRestore(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v).value))

  def $copy(p:PropertyLens[D, T]):D => D =
    (d) => {
      p._strictGet(d)
        .fold(d)(v => $setOrRestore(v)(d))
    }

  def $maybeCopy(p:MaybeLens[D, T]):D => D =
    d => {
      p._strictGet(d).flatten.fold(d){p =>
        $set(p)(d)
      }
    }

  def $setNull: PathSetter[D] =
    ValueSetter(_path, DNull)

  private[dsentric] def _strictGet(data:D):Option[Option[T]] =
    PathLensOps
      .traverse(data.value, _path) match {
      case None => Some(Some(_default))
      case Some(v) => _strictness(v, _codec).map(v2 => Some(v2.getOrElse(_default)))
    }

  private[dsentric] def _strictDeltaGet(data:D):Option[Option[DeltaDefaultValue[T]]] =
    PathLensOps
      .traverse(data.value, _path) match {
      case None =>
        Some(None)
      case Some(DNull) =>
        Some(Some(DeltaDefaultReset(_default)))
      case Some(v) => _strictness(v, _codec).map(_.map(DeltaDefaultSet(_)))
    }
}

case class IdentityLens[D]() extends (D => D) {
  def apply(v1: D): D = v1
}
