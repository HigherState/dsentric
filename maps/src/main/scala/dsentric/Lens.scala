package dsentric

sealed trait PropertyLens[D <: DObject, T] {

  def _path:Path
  private[dsentric] def _codec: DCodec[T]

  private[dsentric] def _strictGet(data:D):Option[Option[T]]

  def $set(value:T):D => D =
    d => d.lensWrap(PathLensOps.set(d.value, _path, _codec(value).value)).asInstanceOf[D]

  def $maybeSet(value:Option[T]):D => D =
    value.fold((d:D) => d){ v =>
      $set(v)
    }
}

trait ExpectedLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T] {

  def $get(data:D):Option[T] =
    PathLensOps
      .traverse(data.value, _path)
      .flatMap(_codec.unapply)

  def $getOrElse(data:D, default: => T):T =
    $get(data).getOrElse(default)

  def $modify(f:T => T):D => D =
    d => PathLensOps.modify(d.value, _path, _codec, f).fold(d)(d.lensWrap(_).asInstanceOf[D])

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
      case Some(_:DNull) =>
        None // Not allowed
      case Some(v) =>
        _codec.unapply(v).map(Some(_))
    }

  def $forceDrop:D => D =
    d => PathLensOps.drop(d.value, _path).fold(d)(d.lensWrap(_).asInstanceOf[D])

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
        case _:DNull =>
          Some(DeltaRemove)
        case v =>
          _codec.unapply(v).map(t => DeltaSet(t))
      }

  def $modify(f:Option[T] => T):D => D =
    d => PathLensOps.maybeModify(d.value, _path, _codec, _strictness, f).fold(d)(d.lensWrap(_).asInstanceOf[D])

  def $modifyOrDrop(f:Option[T] => Option[T]):D => D =
    d => PathLensOps.maybeModifyOrDrop(d.value, _path, _codec, _strictness, f).fold(d)(d.lensWrap(_).asInstanceOf[D])

  def $drop:D => D =
    d => PathLensOps.drop(d.value, _path).fold(d)(d.lensWrap(_).asInstanceOf[D])

  def $setOrDrop(value:Option[T]):D => D =
    value.fold($drop)(v => $set(v))

  def $copy(p:PropertyLens[D, T]):D => D =
    (d) => {
      p._strictGet(d)
        .fold(d)(v => $setOrDrop(v)(d))
    }

  def $setNull: D => D =
    d => d.lensWrap(PathLensOps.set(d.value, _path, Dsentric.dNull)).asInstanceOf[D]

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
        case Some(_:DNull) =>
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
        case _:DNull =>
          DeltaDefaultReset(_default)
        case v =>
          _codec.unapply(v).fold[DeltaDefaultValue[T]](DeltaDefaultReset(_default))(t => DeltaDefaultSet(t))
      }

  def $modify(f:T => T):D => D =
    d => PathLensOps.maybeModify(d.value, _path, _codec, _strictness, toDefault.andThen(f)).fold(d)(d.lensWrap(_).asInstanceOf[D])

  def $restore:D=> D =
    d => PathLensOps.drop(d.value, _path).fold(d)(d.lensWrap(_).asInstanceOf[D])

  def $setOrRestore(value:Option[T]):D => D =
    value.fold($restore)(v => $set(v))

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

  def $setNull: D => D =
    d => d.lensWrap(PathLensOps.set(d.value, _path, Dsentric.dNull)).asInstanceOf[D]

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
      case Some(_:DNull) =>
        Some(Some(DeltaDefaultReset(_default)))
      case Some(v) => _strictness(v, _codec).map(_.map(DeltaDefaultSet(_)))
    }
}

case class IdentityLens[D]() extends (D => D) {
  def apply(v1: D): D = v1
}

//trait MaybeDeltaDelete[Data, IndexedData, T] extends Any {
//  def maybeProperty:Maybe[Data, IndexedData, T]
//  protected def deleteValue:Data
//
//  def $deltaDelete:Data => Data =
//    maybeProperty._path.set(Some(deleteValue))
//}
//
//trait DefaultDeltaDelete[Data, IndexedData, T] extends Any {
//  def defaultProperty:Default[Data, IndexedData, T]
//  protected def deleteValue:Data
//
//  def $deltaDelete:Data => Data =
//    defaultProperty._path.set(Some(deleteValue))
//}