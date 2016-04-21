package dsentric

sealed trait PropertyLens[T] {

  def _path:Path
  private[dsentric] def _codec: JCodec[T]

  def $get(data:JObject):Option[T] =
    PathOps
      .traverse(data.value, _path)
      .flatMap(_codec.unapply)

  def $set(value:T):JObject => JObject =
    d => JObject(PathOps.set(d.value, _path, value))

  def $maybeSet(value:Option[T]):JObject => JObject =
    value.fold((d:JObject) => d){ v =>
      $set(v)
    }
}

trait ExpectedLens[T] extends PropertyLens[T] with ApplicativeLens[JObject, T, T] {

  def $modify(f:T => T):JObject => JObject =
    d => PathOps.modify(d.value, _path, _codec, f).fold(d)(JObject.apply)

  def $copy(p:PropertyLens[T]):JObject => JObject =
    d => {
      p.$get(d).fold(d){p =>
        $set(p)(d)
      }
    }
}
//
//trait MaybeLens[T] extends PropertyLens[T] with ApplicativeLens[JObject, Option[T], T] {
//
//  private[dsentric] def _strictness:Strictness
//
//  def $modify(f:Option[T] => Option[T]):JObject => JObject =
//    _path.modify{
//      case None =>
//        f(None).map(_prism.reverseGet)
//      case Some(v) =>
//        _strictness(v, _prism).fold[Option[Data]](Some(v)){ maybeT =>
//          f(maybeT).map(_prism.reverseGet)
//        }
//    }
//
//  def $drop:Data => Data =
//    _path.modify(_ => None)
//
//  def $setOrDrop(value:Option[T]):Data => Data =
//    _path.modify(_ => value.map(_prism.reverseGet))
//
//  //no enforcement of strict yet
//  def $copy(p:PropertyLens[Data, T]):Data => Data =
//    (d:Data) => {
//      p.$get(d).fold($drop(d)){p =>
//        $set(p)(d)
//      }
//    }
//}
//
//trait DefaultLens[Data, T] extends PropertyLens[Data, T] with ApplicativeLens[Data, T, T]{
//
//  def _default:T
//
//  private[dsentric] def _strictness:Strictness
//
//  override def $get(data:Data):Option[T] =
//    _path.getOption(data).flatMap{
//      case Some(v) =>
//        _strictness(v, _prism).map(_.getOrElse(_default))
//      case None =>
//        Some(_default)
//    }
//
//  def $modify(f:T => T):Data => Data =
//    _path.modify{
//      case None =>
//        Some(_prism.reverseGet(f(_default)))
//      case Some(v) =>
//        _strictness(v, _prism).fold[Option[Data]](Some(v)){ maybeT =>
//          Some(_prism.reverseGet(f(maybeT.getOrElse(_default))))
//        }
//    }
//
//  def $restore:Data => Data =
//    _path.modify(_ => None)
//
//  def $setOrRestore(value:Option[T]):Data => Data =
//    _path.modify(_ => value.map(_prism.reverseGet))
//
//  def $copy(p:PropertyLens[Data, T]):Data => Data =
//    (d:Data) => {
//      p.$get(d).fold(d){p =>
//        $set(p)(d)
//      }
//    }
//}
//
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