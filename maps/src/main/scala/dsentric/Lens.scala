package dsentric

sealed trait PropertyLens[T] {

  def _path:Path
  private[dsentric] def _codec: DCodec[T]

  private[dsentric] def _strictGet(data:DObject):Option[Option[T]]

  def $set(value:T):DObject => DObject =
    d => new DObject(PathLensOps.set(d.value, _path, _codec(value).value))

  def $maybeSet(value:Option[T]):DObject => DObject =
    value.fold((d:DObject) => d){ v =>
      $set(v)
    }
}

trait ExpectedLens[T] extends PropertyLens[T] with ApplicativeLens[DObject, T] {

  def $get(data:DObject):Option[T] =
    PathLensOps
      .traverse(data.value, _path)
      .flatMap(_codec.unapply)

  def $modify(f:T => T):DObject => DObject =
    d => PathLensOps.modify(d.value, _path, _codec, f).fold(d)(new DObject(_))

  def $copy(p:PropertyLens[T]):DObject => DObject =
    d => {
      p._strictGet(d).flatten.fold(d){p =>
        $set(p)(d)
      }
    }

  def $forceDrop:DObject => DObject =
    d => PathLensOps.drop(d.value, _path).fold(d)(new DObject(_))

  //both empty or wrong value are bad values
  private[dsentric] def _strictGet(data:DObject):Option[Option[T]] =
    $get(data).map(v => Some(v))
}

trait MaybeLens[T] extends PropertyLens[T] with ApplicativeLens[DObject, Option[T]] {

  private[dsentric] def _strictness:Strictness

  def $get(data:DObject):Option[T] =
    PathLensOps
      .traverse(data.value, _path)
      .flatMap(_codec.unapply)

  def $modify(f:Option[T] => T):DObject => DObject =
    d => PathLensOps.maybeModify(d.value, _path, _codec, _strictness, f).fold(d)(new DObject(_))

  def $modifyOrDrop(f:Option[T] => Option[T]):DObject => DObject =
    d => PathLensOps.maybeModifyOrDrop(d.value, _path, _codec, _strictness, f).fold(d)(new DObject(_))

  def $drop:DObject => DObject =
    d => PathLensOps.drop(d.value, _path).fold(d)(new DObject(_))

  def $setOrDrop(value:Option[T]):DObject => DObject =
    value.fold($drop)(v => $set(v))

  def $copy(p:PropertyLens[T]):DObject => DObject =
    (d) => {
      p._strictGet(d)
        .fold(d)(v => $setOrDrop(v)(d))
    }

  private[dsentric] def _strictGet(data:DObject):Option[Option[T]] =
    PathLensOps
      .traverse(data.value, _path) match {
        case None => Some(None)
        case Some(v) => _strictness(v, _codec)
    }
}

trait DefaultLens[T] extends PropertyLens[T] with ApplicativeLens[DObject, T]{

  def _default:T

  private[dsentric] def _strictness:Strictness

  private val toDefault =
    (maybe:Option[T]) => maybe.getOrElse(_default)

  def $get(data:DObject):T =
    PathLensOps
      .traverse(data.value, _path)
      .fold(_default) { t =>
        _codec.unapply(t).getOrElse(_default)
      }

  def $modify(f:T => T):DObject => DObject =
    d => PathLensOps.maybeModify(d.value, _path, _codec, _strictness, toDefault.andThen(f)).fold(d)(new DObject(_))

  def $restore:DObject => DObject =
    d => PathLensOps.drop(d.value, _path).fold(d)(new DObject(_))

  def $setOrRestore(value:Option[T]):DObject => DObject =
    value.fold($restore)(v => $set(v))

  def $copy(p:PropertyLens[T]):DObject => DObject =
    (d) => {
      p._strictGet(d)
        .fold(d)(v => $setOrRestore(v)(d))
    }

  private[dsentric] def _strictGet(data:DObject):Option[Option[T]] =
    PathLensOps
      .traverse(data.value, _path) match {
      case None => Some(Some(_default))
      case Some(v) => _strictness(v, _codec).map(v2 => Some(v2.getOrElse(_default)))
    }
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