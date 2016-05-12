package dsentric

sealed trait PropertyLens[T] {

  def _path:Path
  private[dsentric] def _codec: JCodec[T]

  private[dsentric] def _strictGet(data:JObject):Option[Option[T]]

  def $set(value:T):JObject => JObject =
    d => new JObject(PathOps.set(d.value, _path, value))

  def $maybeSet(value:Option[T]):JObject => JObject =
    value.fold((d:JObject) => d){ v =>
      $set(v)
    }
}

trait ExpectedLens[T] extends PropertyLens[T] with ApplicativeLens[JObject, T, T] {

  def $get(data:JObject):Option[T] =
    PathOps
      .traverse(data.value, _path)
      .flatMap(_codec.unapply)

  def $modify(f:T => T):JObject => JObject =
    d => PathOps.modify(d.value, _path, _codec, f).fold(d)(JObject.apply)

  def $copy(p:PropertyLens[T]):JObject => JObject =
    d => {
      p._strictGet(d).flatten.fold(d){p =>
        $set(p)(d)
      }
    }

  //both empty or wrong value are bad values
  private[dsentric] def _strictGet(data:JObject):Option[Option[T]] =
    $get(data).map(v => Some(v))
}

trait MaybeLens[T] extends PropertyLens[T] with ApplicativeLens[JObject, Option[T], T] {

  private[dsentric] def _strictness:Strictness

  def $get(data:JObject):Option[T] =
    PathOps
      .traverse(data.value, _path)
      .flatMap(_codec.unapply)

  def $modify(f:Option[T] => T):JObject => JObject =
    d => PathOps.maybeModify(d.value, _path, _codec, _strictness, f).fold(d)(JObject.apply)

  def $modifyOrDrop(f:Option[T] => Option[T]):JObject => JObject =
    d => PathOps.maybeModifyOrDrop(d.value, _path, _codec, _strictness, f).fold(d)(JObject.apply)

  def $drop:JObject => JObject =
    d => PathOps.drop(d.value, _path).fold(d)(JObject.apply)

  def $setOrDrop(value:Option[T]):JObject => JObject =
    value.fold($drop)(v => $set(v))

  def $copy(p:PropertyLens[T]):JObject => JObject =
    (d) => {
      p._strictGet(d)
        .fold(d)(v => $setOrDrop(v)(d))
    }

  private[dsentric] def _strictGet(data:JObject):Option[Option[T]] =
    PathOps
      .traverse(data.value, _path) match {
        case None => Some(None)
        case Some(v) => _strictness(v, _codec)
    }
}

trait DefaultLens[T] extends PropertyLens[T] with ApplicativeLens[JObject, T, T]{

  def _default:T

  private[dsentric] def _strictness:Strictness

  private val toDefault =
    (maybe:Option[T]) => maybe.getOrElse(_default)

  def $get(data:JObject):T =
    PathOps
      .traverse(data.value, _path)
      .fold(_default) { t =>
        _codec.unapply(t).getOrElse(_default)
      }

  def $modify(f:T => T):JObject => JObject =
    d => PathOps.maybeModify(d.value, _path, _codec, _strictness, toDefault.andThen(f)).fold(d)(JObject.apply)

  def $restore:JObject => JObject =
    d => PathOps.drop(d.value, _path).fold(d)(JObject.apply)

  def $setOrRestore(value:Option[T]):JObject => JObject =
    value.fold($restore)(v => $set(v))

  def $copy(p:PropertyLens[T]):JObject => JObject =
    (d) => {
      p._strictGet(d)
        .fold(d)(v => $setOrRestore(v)(d))
    }

  private[dsentric] def _strictGet(data:JObject):Option[Option[T]] =
    PathOps
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