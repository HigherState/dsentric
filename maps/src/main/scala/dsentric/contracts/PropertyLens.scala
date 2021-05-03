package dsentric.contracts

import dsentric.codecs.DCodec
import dsentric.{RawObject, _}
import dsentric.failure._

private[dsentric] trait PropertyLens[D <: DObject, T] extends BaseAux with ParameterisedAux[D] {
  def _key:String
  def _path:Path
  def _codec: DCodec[T]
  def _parent: BaseContract[D]
  def _root: ContractFor[D] = _parent._root
  /**
   * Does the object satisfy all the type and expectation constraints.
   * Returns list of possible failures.
   * @param obj
   * @return
   */
  def $verify(obj:D):List[StructuralFailure]

  /**
   * Return the property value or failure
   * Can return Default value if provided value is empty or invalid and type behaviour is empty.
   * @param obj
   * @return
   */
  private[contracts] def __get(obj:D):Traversed[T]

  /**
   * Verifies the direct property against the object.
   * @param obj
   * @return
   */
  private[contracts] def __verifyTraversal(obj:RawObject):List[StructuralFailure]

  private[contracts] def __set(obj:D, value:T):D =
    obj.internalWrap(PathLensOps.set(obj.value, _path, _codec(value))).asInstanceOf[D]

}

private[dsentric] trait ExpectedLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T] {

  private[contracts] def __get(data:D):Traversed[T] =
    TraversalOps.traverse(data.value, this) match {
      case NotFound => Failed(ExpectedFailure(this))
      case t => t
    }

  private[contracts] def __verifyTraversal(obj:RawObject):List[StructuralFailure] =
    TraversalOps.propertyValue(obj, this) match {
      case NotFound => List(ExpectedFailure(this))
      case Found(_) => Nil
      case Failed(f, tail) => f :: tail
    }


  /**
   * Verifies the Type of the value for the Property and its existence.
   * Returns failure if object not found, unless property has MaybeObject Ancestor
   * @param obj
   * @return
   */
  final def $verify(obj:D):List[StructuralFailure] =
    TraversalOps.traverse(obj.value, this) match {
      case NotFound => List(ExpectedFailure(this))
      case Failed(f, tail) => f :: tail
      case _ => Nil
    }

  /**
   * Returns object if found and of correct type
   * Returns ExpectedFailure if object not found, unless property has MaybeObject Property Ancestor
   * Returns None if there is a path with Maybes Object Properties that have no values
   * @param obj
   * @return
   */
  final def $get(obj:D):ValidResult[Option[T]] =
    __get(obj).toValidOption

  /**
   * Sets or Replaces the value for the Property.
   * Will create object path to Property if objects dont exist.
   * @param value
   * @return
   */
  final def $set(value:T):PathSetter[D] =
    ValueSetter(_path, _codec(value))

  /**
   * Sets or Replaces value for the Property.
   * Does nothing if None is provided.
   * Will create object path to Property if objects dont exist.
   * @param value
   * @return
   */
  final def $maybeSet(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](IdentitySetter[D]()) { v =>
      ValueSetter(_path, _codec(v))
    }

  /**
   * Modifies value for the property.
   * Returns Failure if existing value is Empty or of wrong type.
   * @param f
   * @return
   */
  final def $modify(f:T => T):ValidPathSetter[D] =
    ModifySetter(d => __get(d).toValidOption, f, __set)

  /**
   * Copying from an existing property, if that property is
   * an Empty value on a Maybe Property, it will ignore the copy operation.
   * If its an Expected property it will fail if empty
   * If the copied property is Empty but is a Default Property, it will
   * copy the default value.
   * @param p
   * @return
   */
  final def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    ModifySetter(d => p.__get(d).toValidOption, identity[T], __set)

//  final lazy val $delta:ExpectedDelta[D, T] =
//    new ExpectedDelta(this)

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[T] =
    PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply)
}

private[dsentric] trait MaybeLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, Option[T]] {

  private[contracts] def __get(obj:D):Traversed[T] =
    TraversalOps.traverse(obj.value, this)

  private[contracts] def __verifyTraversal(obj:RawObject):List[StructuralFailure] =
    TraversalOps.propertyValue(obj, this) match {
      case Failed(f, tail) => f :: tail
      case _ => Nil
    }

  /**
   * Verifies the Type of the value for the Property if it exists in the object.
   * @param obj
   * @return
   */
  final def $verify(obj:D):List[StructuralFailure] =
    TraversalOps.traverse(obj.value, this) match {
      case Failed(f, tail) => f :: tail
      case _ => Nil
    }

  /**
   * Gets value for the property if found in passed object.
   * Returns failure if value is of unexpected type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType
   * @param obj
   * @return
   */
  final def $get(obj:D):ValidResult[Option[T]] =
    __get(obj).toValidOption

  /**
   * Gets value for the property if found in passed object, otherwise returns default.
   * Returns failure if value is of unexpected type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType
   * @param obj
   * @return
   */
  final def $getOrElse(obj:D, default: => T):ValidResult[T] =
    __get(obj).toValidOption.map(_.getOrElse(default))

  /**
   * Sets or Replaces value for the Property.
   * Will create object path to Property if objects dont exist.
   * @param value
   * @return
   */
  final def $set(value:T):PathSetter[D] =
    ValueSetter(_path, _codec(value))

  /**
   * Sets or Replaces value for the Property.
   * Does nothing if None is provided.
   * Will create object path to Property if objects dont exist.
   * @param value
   * @return
   */
  final def $maybeSet(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](IdentitySetter[D]()) { v =>
      ValueSetter(_path, _codec(v))
    }

  /**
   * Removes the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   * @return
   */
  final def $drop: PathSetter[D] =
    ValueDrop(_path)

  /**
   * Sets or Replaces vale for the Property if provided
   * Will create object path to Property if objects dont exist.
   * If None is provided it will remove the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   *
   * @param value
   * @return
   */
  final def $setOrDrop(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v)))

  /**
   * Modifies or sets the value if it doesnt exist.
   * Will create object path to Property if objects dont exist only if path is expected.
   * Returns failure if value is of the wrong type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType, in which case
   * None will be passed as the argument into the function.
   * @param f
   * @return
   */
  final def $modify(f:Option[T] => T):ValidPathSetter[D] =
    TraversedModifySetter(__get, f, __set)

  /**
   * Modifies or sets the value if it doesnt exist.
   * Will create object path to Property if objects dont exist only if path is expected.
   * If None is returned it will remove the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   * Returns failure if value is of the wrong type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType, in which case
   * None will be passed as the argument into the function.
   *
   * @param f
   * @return
   */
  final def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[D] =
    TraversedModifyOrDropSetter[D, T](
      __get,
      f,
      (d, mt) => $setOrDrop(mt)(d)
    )

  /**
   * Copying from an existing property, if that property is
   * an Empty value on a Maybe Property, it will drop the value for this property.
   * If the MaybeProperty is in a nonexistent maybe object, it will ignore the copy.
   * If its an Expected property it will fail if empty
   * If the copied property is Empty but is a Default Property, it will
   * copy the default value.
   * If will fail if the source property is of the wrong type, unless the
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType, in which case
   * The target property will be dropped.
   * @param p
   * @return
   */
  final def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    TraversedModifyOrDropSetter(
      p.__get,
      identity[Option[T]],
      (d:D, mt:Option[T]) => $setOrDrop(mt)(d))

//  final lazy val $delta:MaybeDelta[D, T] =
//    new MaybeDelta[D, T](this)

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * Will return None on incorrect type
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[Option[T]] =
    TraversalOps.traverse(obj.value, this) match {
      case PathEmptyMaybe => Some(None)
      case NotFound => Some(None)
      case Found(t) => Some(Some(t))
      case Failed(_, _) => None
    }
}

private[dsentric] trait DefaultLens[D <: DObject, T] extends PropertyLens[D, T] with ApplicativeLens[D, T]{

  def _default:T

  private[contracts] def __get(obj:D):Traversed[T] =
    TraversalOps.traverse(obj.value,this) match {
      case NotFound => Found(_default)
      case t => t
    }

  private[contracts] def __verifyTraversal(obj:RawObject):List[StructuralFailure] =
    TraversalOps.propertyValue(obj, this) match {
      case Failed(f, tail) => f :: tail
      case _ => Nil
    }

  /**
   * Gets value for the property if found in passed object.
   * Otherwise returns the default value.
   *
   * Will return None if there is a default Maybe ancestor which is empty
   *
   * Returns fail if value found to be incorrect type unless
   * incorrectTypeBehaviour is set to EmptyOnIncorrectType, in which case
   * the default value is returned
   * @param obj
   * @return
   */
  final def $get(obj:D):ValidResult[Option[T]] =
    __get(obj).toValidOption

  /**
   * Sets or Replaces the value for the Property.
   * Will create object path to Property if objects dont exist.
   * If you set a value which is the same as the default value,
   * The value will get set in the object
   * @param value
   * @return
   */
  final def $set(value:T):PathSetter[D] =
    ValueSetter(_path, _codec(value))

  /**
   * Sets or Replaces value for the Property.
   * Does nothing if None is provided.
   * Will create object path to Property if objects dont exist.
   * @param value
   * @return
   */
  final def $maybeSet(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](IdentitySetter[D]()) { v =>
      ValueSetter(_path, _codec(v))
    }

  /**
   * When verifying an incorrect type always returns a failure, even if
   * incorrect type behaviour is set to ignore
   * @param obj
   * @return
   */
  final def $verify(obj:D):List[StructuralFailure] =
    TraversalOps.traverse(obj.value, this) match {
      case Failed(f, tail) => f :: tail
      case _ => Nil
    }

  /**
   * Removes the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   * @return
   */
  final def $restore: PathSetter[D] =
    ValueDrop(_path)

  /**
   * Modifies or sets the value if it doesnt exist.
   * Will create object path to Property if objects dont exist.
   * Returns failure if value is of the wrong type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType, in which case
   * None will be passed as the argument into the function.
   * @param f
   * @return
   */
  final def $modify(f:T => T):ValidPathSetter[D] =
    ModifySetter($get, f, __set)

  /**
   * Sets or Replaces vale for the Property if provided
   * Will create object path to Property if objects dont exist.
   * If None is provided it will remove the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   *
   * @param value
   * @return
   */
  final def $setOrRestore(value:Option[T]):PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v)))

  /**
   * Copying from an existing property, if that property is
   * an Empty value on a Maybe Property, it will drop the value for this property.
   * If its an Expected property it will fail if empty
   * If the copied property is Empty but is a Default Property, it will
   * copy the default value.
   * If will fail if the source property is of the wrong type, unless the
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType, in which case
   * The target property will be dropped.
   * @param p
   * @return
   */
  final def $copy(p:PropertyLens[D, T]):ValidPathSetter[D] =
    TraversedModifyOrDropSetter[D, T](p.__get, identity[Option[T]], (d, mt) => $setOrRestore(mt)(d))

//  final lazy val $delta:MaybeDelta[D, T] =
//    new MaybeDelta[D, T](this)

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * Returns Some(default) if not found or type is incorrect and incorrectTypeBehaviour is Empty
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[T] =
    TraversalOps.traverse(obj.value, this) match {
      case PathEmptyMaybe => Some(_default)
      case NotFound => Some(_default)
      case Found(t) => Some(t)
      case Failed(_, _) => None
    }
}



//final class ExpectedDelta[D <: DObject, T] private[dsentric](property:ExpectedLens[D, T]) {
//  def $get(delta:DObject):ValidResult[Option[T]] =
//    property.__incorrectTypeBehaviour.traverse(delta.value, property)
//
//  def $set(value:T):PathSetter[DObject] =
//    ValueSetter[DObject](property._path, property._codec(value).value)
//
//  def $setOrDrop(value:Option[T]):PathSetter[DObject] =
//    value.fold[PathSetter[DObject]](ValueDrop(property._path))(v => ValueSetter(property._path, property._codec(v).value))
//
//  def $modify(f:Option[T] => T):ValidPathSetter[DObject] =
//    MaybeModifySetter[DObject, T](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property), f, (d, t) =>  $set(t)(d))
//
//  def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[DObject] =
//    ModifyOrDropSetter[DObject, T](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property), f, (d, mt) => $setOrDrop(mt)(d))
//
//  def $drop:PathSetter[DObject] =
//    ValueDrop[DObject](property._path)
//
//  def unapply(delta:DObject):Option[Option[T]] =
//    property.__incorrectTypeBehaviour.matcher(delta.value, property)
//}
//
//final class MaybeDelta[D <: DObject, T] private[dsentric](property:PropertyLens[D, T]) {
//  private val _deltaCodec = PessimisticCodecs.dNullableCodec[T](property._codec)
//
//  def $get(delta:DObject):ValidResult[Option[DNullable[T]]] =
//    property.__incorrectTypeBehaviour.traverse(delta.value, property._root, property._path, _deltaCodec)
//
//  def $set(value:T):PathSetter[DObject] =
//    ValueSetter[DObject](property._path, property._codec(value).value)
//
//  def $drop:PathSetter[DObject] =
//    ValueDrop[DObject](property._path)
//
//  def $setOrDrop(value:Option[T]):PathSetter[DObject] =
//    value.fold[PathSetter[DObject]](ValueDrop(property._path))(v => ValueSetter(property._path, property._codec(v).value))
//
//  def $setNull:PathSetter[DObject] =
//    ValueSetter[DObject](property._path, DNull)
//
//  def $setOrNull(value:Option[DNullable[T]]):PathSetter[DObject] =
//    value.fold[PathSetter[DObject]](ValueDrop(property._path))(v => ValueSetter(property._path, _deltaCodec(v).value))
//
//  def $modify(f:Option[T] => T):ValidPathSetter[DObject] =
//    MaybeModifySetter[DObject, T](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property), f, (d, t) => $set(t)(d))
//
//  def $modifyOrDrop(f:Option[T] => Option[T]):ValidPathSetter[DObject] =
//    ModifyOrDropSetter[DObject, T](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property), f, (d, mt) => $setOrDrop(mt)(d))
//
//  def $modifyOrNull(f:Option[DNullable[T]] => Option[DNullable[T]]):ValidPathSetter[DObject] =
//    ModifyOrDropSetter[DObject, DNullable[T]](obj => property.__incorrectTypeBehaviour.traverse(obj.value, property._root, property._path, _deltaCodec), f, (d, mt) => $setOrNull(mt)(d))
//
//  def unapply(delta:DObject):Option[Option[DNullable[T]]] =
//    property.__incorrectTypeBehaviour.matcher(delta.value, _deltaCodec)
//}


