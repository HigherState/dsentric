package dsentric.contracts

import dsentric._
import dsentric.failure._

private[dsentric] trait PropertyLens[D <: DObject, T] {

  def _path:Path
  def _codec: DCodec[T]
  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour

  def $set(value:T):PathSetter[D] =
    ValueSetter(_path, _codec(value).value)

  def $maybeSet(value:Option[T]):PathSetter[D] =
    MaybeValueSetter(_path, value.map(_codec(_).value))

  private[contracts] def __get(obj:D):ValidResult[Option[T]]

  private[contracts] def __set(obj:D, value:T):D =
    obj.internalWrap(PathLensOps.set(obj.value, _path, _codec(value).value)).asInstanceOf[D]

  private[contracts] def __expected(obj:D):ValidResult[T] =
    __get(obj)
      .flatMap{
        case None =>
          ValidResult.failure[T](ExpectedFailure(_path))
        case Some(t) =>
          Right(t)
      }
}

private[dsentric] trait ExpectedLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T] {

  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour

  private[contracts] def __get(data:D):ValidResult[Option[T]] =
    expectedResult(__incorrectTypeBehaviour.traverse(data.value, _path, _codec), _path)

  private def expectedResult(v:ValidResult[Option[T]], path:Path): ValidResult[Option[T]] =
    v.flatMap{
      case None => ValidResult.failure(ExpectedFailure(path))
      case _ => v
    }

  def $get(obj:D):ValidResult[T] =
    __expected(obj)


  def $modify(f:T => T):ValidPathSetter[D] =
    ModifySetter(__get, f, __set)

  //When copying a maybe property with no value, then the copy does nothing
  def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    ModifySetter(d => expectedResult(p.__get(d), p._path), identity[T], __set)

  lazy val $delta:ExpectedDelta[T] =
    ExpectedDelta(_path, _codec, __incorrectTypeBehaviour)


  def unapply(obj:D):Option[T] =
    PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply)
}

private[dsentric] trait MaybeLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, Option[T]] {

  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour

  private[contracts] def __get(obj:D):ValidResult[Option[T]] =
    __incorrectTypeBehaviour.traverse(obj.value, _path, _codec)

  def $get(obj:D):ValidResult[Option[T]] =
    __get(obj)

  def $getOrElse(obj:D, default: => T):ValidResult[T] =
    __get(obj).map(_.getOrElse(default))

  def $drop: PathSetter[D] =
    ValueDrop(_path)

  def $setOrDrop(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v).value))

  def $modify(f:Option[T] => T):ValidPathSetter[D] =
    MaybeModifySetter(__get, f, __set)

  def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[D] =
    ModifyOrDropSetter[D, T](__get, f, (d, mt) => $setOrDrop(mt)(d))

  def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    ModifyOrDropSetter(p.__get, identity[Option[T]], (d:D, mt:Option[T]) => $setOrDrop(mt)(d))

  def unapply(obj:D):Option[Option[T]] =
    __incorrectTypeBehaviour.traverseMatcher(obj.value, _path, _codec)
}

private[dsentric] trait DefaultLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T]{

  def _default:T

  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour

  private[contracts] def __get(obj:D):ValidResult[Option[T]] =
    __incorrectTypeBehaviour.traverse(obj.value, _path, _codec)
      .map(_.orElse(Some(_default)))

  //Behaviour is default on incorrect type
  def $get(obj:D):ValidResult[T] =
    __incorrectTypeBehaviour.traverse(obj.value, _path, _codec)
      .map(_.getOrElse(_default))

  def $restore: PathSetter[D] =
    ValueDrop(_path)

  def $modify(f:T => T):ValidPathSetter[D] =
    ModifySetter(__get, f, __set)

  def $setOrRestore(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v).value))

  def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    ModifyOrDropSetter[D, T](p.__get, identity[Option[T]], (d, mt) => $setOrRestore(mt)(d))

  lazy val $delta:MaybeDelta[T] =
    new MaybeDelta[T](_path, _codec, __incorrectTypeBehaviour)

  def unapply(obj:D):Option[T] =
    __incorrectTypeBehaviour.traverseMatcher(obj.value, _path, _codec)
    .map(_.getOrElse(_default))
}




//Codec implies it doesnt actually have to be an array as raw type
private[dsentric] trait ObjectsLens[D <: DObject, T <: DObject] extends PropertyLens[D, Vector[T]] {
  def _path:Path
  def _contract:ContractFor[T]

  private[contracts] def __get(obj: D): ValidResult[Option[Vector[T]]] = ???

  def $get(obj:D):ValidResult[Vector[T]] = ???

  def $clear:PathSetter[D] = ???

  def $append(element:T):ValidPathSetter[D] = ???

  def $map(f:T => T):ValidPathSetter[D] = ???


}

private[dsentric] trait MapObjectsLens[D <: DObject, K, T <: DObject] extends PropertyLens[D, Map[K, T]] {
  def _path:Path
  def _contract:ContractFor[T]
  def _keyCodec:StringCodec[K]

  private[contracts] def __get(obj: D): ValidResult[Option[Map[K, T]]] = ???

  def $get(obj:D):ValidResult[Map[K, T]] = ???

  def $clear:PathSetter[D] = ???

  def $add(keyValue:(K, T)):ValidPathSetter[D] = ???

  def $map(f:T => T):ValidPathSetter[D] = ???
}








final case class ExpectedDelta[T](_path:Path, _codec:DCodec[T], _incorrectTypeBehaviour:IncorrectTypeBehaviour) {
  def $get(delta:DObject):ValidResult[Option[T]] =
    _incorrectTypeBehaviour.traverse(delta.value, _path, _codec)

  def $set(value:T):PathSetter[DObject] =
    ValueSetter[DObject](_path, _codec(value).value)

  def $setOrDrop(value:Option[T]):PathSetter[DObject] =
    value.fold[PathSetter[DObject]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v).value))

  def $modify(f:Option[T] => T):ValidPathSetter[DObject] =
    MaybeModifySetter[DObject, T](obj => _incorrectTypeBehaviour.traverse(obj.value, _path, _codec), f, (d, t) =>  $set(t)(d))

  def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[DObject] =
    ModifyOrDropSetter[DObject, T](obj => _incorrectTypeBehaviour.traverse(obj.value, _path, _codec), f, (d, mt) => $setOrDrop(mt)(d))

  def $drop:PathSetter[DObject] =
    ValueDrop[DObject](_path)

  def unapply(delta:DObject):Option[Option[T]] =
    _incorrectTypeBehaviour.matcher(delta.value, _path, _codec)
}

final case class MaybeDelta[T](_path:Path, _codec:DCodec[T], _incorrectTypeBehaviour:IncorrectTypeBehaviour) {
  private val _deltaCodec = PessimisticCodecs.dNullableCodec[T](_codec)

  def $get(delta:DObject):ValidResult[Option[DNullable[T]]] =
    _incorrectTypeBehaviour.traverse(delta.value, _path, _deltaCodec)

  def $set(value:T):PathSetter[DObject] =
    ValueSetter[DObject](_path, _codec(value).value)

  def $drop:PathSetter[DObject] =
    ValueDrop[DObject](_path)

  def $setOrDrop(value:Option[T]):PathSetter[DObject] =
    value.fold[PathSetter[DObject]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v).value))

  def $setNull:PathSetter[DObject] =
    ValueSetter[DObject](_path, DNull)

  def $setOrNull(value:Option[DNullable[T]]):PathSetter[DObject] =
    value.fold[PathSetter[DObject]](ValueDrop(_path))(v => ValueSetter(_path, _deltaCodec(v).value))

  def $modify(f:Option[T] => T):ValidPathSetter[DObject] =
    MaybeModifySetter[DObject, T](obj => _incorrectTypeBehaviour.traverse(obj.value, _path, _codec), f, (d, t) => $set(t)(d))

  def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[DObject] =
    ModifyOrDropSetter[DObject, T](obj => _incorrectTypeBehaviour.traverse(obj.value, _path, _codec), f, (d, mt) => $setOrDrop(mt)(d))

  def $modifyOrNull(f:Option[DNullable[T]] => Option[DNullable[T]]):ValidPathSetter[DObject] =
    ModifyOrDropSetter[DObject, DNullable[T]](obj => _incorrectTypeBehaviour.traverse(obj.value, _path, _deltaCodec), f, (d, mt) => $setOrNull(mt)(d))

  def unapply(delta:DObject):Option[Option[DNullable[T]]] =
    _incorrectTypeBehaviour.matcher(delta.value, _path, _deltaCodec)
}


