package dsentric.contracts

import dsentric._
import dsentric.failure._

private[dsentric] trait PropertyLens[D <: DObject, T] {

  def _path:Path
  def _codec: DCodec[T]
  def _root:ContractFor[D]
  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour

  def $verify(obj:D):List[StructuralFailure]

  /**
   * Return the property value or failure
   * Can return None on failure if typeBehaviour is to empty
   * Can return Default value if provided value is empty or invalid and type behaviour is empty.
   * @param obj
   * @return
   */
  private[contracts] def __get(obj:D):ValidResult[Option[T]]

  /**
   * Applies the property value to the object.
   * Can remove if value on failure if typeBehaviour is to empty.
   * Will set default if value is not found or value is empty or invalid and type behaviour is empty.
   * @param obj
   * @return
   */
  private[contracts] def __apply(obj:D):ValidResult[D]

  private[contracts] def __set(obj:D, value:T):D =
    obj.internalWrap(PathLensOps.set(obj.value, _path, _codec(value).value)).asInstanceOf[D]

  private[contracts] def __expected(obj:D):ValidResult[T] =
    __get(obj)
      .flatMap{
        case None =>
          ValidResult.failure[T](ExpectedFailure(this))
        case Some(t) =>
          Right(t)
      }
}

private[dsentric] trait ExpectedLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T] {

  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour

  private[contracts] def __get(data:D):ValidResult[Option[T]] =
    expectedResult(__incorrectTypeBehaviour.traverse(data.value, this), this)

  private[contracts] def __apply(obj: D): ValidResult[D] =
    expectedResult(__incorrectTypeBehaviour.traverse(obj.value, this), this)
      .map(_ => obj)

  private def expectedResult(v:ValidResult[Option[T]], p:PropertyLens[D, T]): ValidResult[Option[T]] =
    v.flatMap{
      case None => ValidResult.failure(ExpectedFailure(p))
      case _ => v
    }

  final def $verify(obj:D):List[StructuralFailure] =
    FailOnIncorrectTypeBehaviour.traverseVerify(obj.value, this, true)

  final def $get(obj:D):ValidResult[T] =
    __expected(obj)

  final def $set(value:T):PathSetter[D] =
    ValueSetter(_path, _codec(value).value)

  final def $maybeSet(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](IdentitySetter[D]()) { v =>
      ValueSetter(_path, _codec(v).value)
    }

  final def $modify(f:T => T):ValidPathSetter[D] =
    ModifySetter(__get, f, __set)

  //When copying a maybe property with no value, then the copy does nothing
  final def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    ModifySetter(d => expectedResult(p.__get(d), p), identity[T], __set)

  final lazy val $delta:ExpectedDelta[D, T] =
    new ExpectedDelta(this)


  final def unapply(obj:D):Option[T] =
    PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply)
}

private[dsentric] trait MaybeLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, Option[T]] {

  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour

  private[contracts] def __get(obj:D):ValidResult[Option[T]] =
    __incorrectTypeBehaviour.traverse(obj.value, this)

  private[contracts] def __apply(obj: D): ValidResult[D] =
    PathLensOps.transform(obj.value, _path){
      case None =>
        ValidResult.none
      case Some(a) =>
        __incorrectTypeBehaviour.apply(a, this).map(_.map(_ => a))
    }.map(_.fold(obj)(v => obj.internalWrap(v).asInstanceOf[D]))


  final def $get(obj:D):ValidResult[Option[T]] =
    __get(obj)

  final def $set(value:T):PathSetter[D] =
    ValueSetter(_path, _codec(value).value)

  final def $maybeSet(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](IdentitySetter[D]()) { v =>
      ValueSetter(_path, _codec(v).value)
    }

  final def $getOrElse(obj:D, default: => T):ValidResult[T] =
    __get(obj).map(_.getOrElse(default))

  final def $verify(obj:D):List[StructuralFailure] =
    FailOnIncorrectTypeBehaviour.traverseVerify(obj.value, this, false)

  final def $drop: PathSetter[D] =
    ValueDrop(_path)

  final def $setOrDrop(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v).value))

  final def $modify(f:Option[T] => T):ValidPathSetter[D] =
    MaybeModifySetter(__get, f, __set)

  final def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[D] =
    ModifyOrDropSetter[D, T](__get, f, (d, mt) => $setOrDrop(mt)(d))

  final def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    ModifyOrDropSetter(p.__get, identity[Option[T]], (d:D, mt:Option[T]) => $setOrDrop(mt)(d))

  final lazy val $delta:MaybeDelta[D, T] =
    new MaybeDelta[D, T](this)

  final def unapply(obj:D):Option[Option[T]] =
    __incorrectTypeBehaviour.traverseMatcher(obj.value, this)
}

private[dsentric] trait DefaultLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T]{

  def _default:T

  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour

  private[contracts] def __get(obj:D):ValidResult[Option[T]] =
    __incorrectTypeBehaviour.traverse(obj.value,this)
      .map(_.orElse(Some(_default)))

  private[contracts] def __apply(obj: D): ValidResult[D] =
    PathLensOps.transform(obj.value, _path){
      case None =>
        ValidResult.none
      case Some(a) =>
        __incorrectTypeBehaviour.apply(a, this).map {
          case None => Some(_codec(_default))
          case Some(_) => Some(a)
        }
    }.map(_.fold(obj)(v => obj.internalWrap(v).asInstanceOf[D]))

  //Behaviour is default on incorrect type
  final def $get(obj:D):ValidResult[T] =
    __incorrectTypeBehaviour.traverse(obj.value, this)
      .map(_.getOrElse(_default))

  final def $set(value:T):PathSetter[D] =
    ValueSetter(_path, _codec(value).value)

  final def $maybeSet(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](IdentitySetter[D]()) { v =>
      ValueSetter(_path, _codec(v).value)
    }

  final def $verify(obj:D):List[StructuralFailure] =
    FailOnIncorrectTypeBehaviour.traverseVerify(obj.value, this, false)

  final def $restore: PathSetter[D] =
    ValueDrop(_path)

  final def $modify(f:T => T):ValidPathSetter[D] =
    ModifySetter(__get, f, __set)

  final def $setOrRestore(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v).value))

  final def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    ModifyOrDropSetter[D, T](p.__get, identity[Option[T]], (d, mt) => $setOrRestore(mt)(d))

  final lazy val $delta:MaybeDelta[D, T] =
    new MaybeDelta[D, T](this)

  final def unapply(obj:D):Option[T] =
    __incorrectTypeBehaviour.traverseMatcher(obj.value, this)
    .map(_.getOrElse(_default))
}



final class ExpectedDelta[D <: DObject, T] private[dsentric](property:ExpectedLens[D, T]) {
  def $get(delta:DObject):ValidResult[Option[T]] =
    property.__incorrectTypeBehaviour.traverse(delta.value, property)

  def $set(value:T):PathSetter[DObject] =
    ValueSetter[DObject](property._path, property._codec(value).value)

  def $setOrDrop(value:Option[T]):PathSetter[DObject] =
    value.fold[PathSetter[DObject]](ValueDrop(property._path))(v => ValueSetter(property._path, property._codec(v).value))

  def $modify(f:Option[T] => T):ValidPathSetter[DObject] =
    MaybeModifySetter[DObject, T](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property), f, (d, t) =>  $set(t)(d))

  def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[DObject] =
    ModifyOrDropSetter[DObject, T](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property), f, (d, mt) => $setOrDrop(mt)(d))

  def $drop:PathSetter[DObject] =
    ValueDrop[DObject](property._path)

  def unapply(delta:DObject):Option[Option[T]] =
    property.__incorrectTypeBehaviour.matcher(delta.value, property)
}

final class MaybeDelta[D <: DObject, T] private[dsentric](property:PropertyLens[D, T]) {
  private val _deltaCodec = PessimisticCodecs.dNullableCodec[T](property._codec)

  def $get(delta:DObject):ValidResult[Option[DNullable[T]]] =
    property.__incorrectTypeBehaviour.traverse(delta.value, property._root, property._path, _deltaCodec)

  def $set(value:T):PathSetter[DObject] =
    ValueSetter[DObject](property._path, property._codec(value).value)

  def $drop:PathSetter[DObject] =
    ValueDrop[DObject](property._path)

  def $setOrDrop(value:Option[T]):PathSetter[DObject] =
    value.fold[PathSetter[DObject]](ValueDrop(property._path))(v => ValueSetter(property._path, property._codec(v).value))

  def $setNull:PathSetter[DObject] =
    ValueSetter[DObject](property._path, DNull)

  def $setOrNull(value:Option[DNullable[T]]):PathSetter[DObject] =
    value.fold[PathSetter[DObject]](ValueDrop(property._path))(v => ValueSetter(property._path, _deltaCodec(v).value))

  def $modify(f:Option[T] => T):ValidPathSetter[DObject] =
    MaybeModifySetter[DObject, T](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property), f, (d, t) => $set(t)(d))

  def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[DObject] =
    ModifyOrDropSetter[DObject, T](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property), f, (d, mt) => $setOrDrop(mt)(d))

  def $modifyOrNull(f:Option[DNullable[T]] => Option[DNullable[T]]):ValidPathSetter[DObject] =
    ModifyOrDropSetter[DObject, DNullable[T]](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property._root, property._path, _deltaCodec), f, (d, mt) => $setOrNull(mt)(d))

  def unapply(delta:DObject):Option[Option[DNullable[T]]] =
    property.__incorrectTypeBehaviour.matcher(delta.value, property._root, property._path, _deltaCodec)
}


