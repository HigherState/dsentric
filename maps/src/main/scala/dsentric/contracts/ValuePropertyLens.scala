package dsentric.contracts

import dsentric._
import dsentric.failure._

sealed trait ValuePropertyLens[D <: DObject, T] extends PropertyLens[D, T] {

  /**
   * Sets or Replaces value for the Property.
   * Does nothing if None is provided.
   * Will create object path to Property if objects dont exist.
   * @param value
   * @return
   */
  final def $maybeSet(value: Option[T]): PathSetter[D] =
    value.fold[PathSetter[D]](IdentitySetter[D]()) { v =>
      ValueSetter(_path, _codec(v))
    }

  final def $validSet(value: ValidResult[T]): ValidPathSetter[D] =
    ValidValueSetter(_path, value.map(_codec.apply))

  final def $validMaybeSet(value: ValidResult[Option[T]]): ValidPathSetter[D] =
    value match {
      case Right(None)    =>
        IdentityValidSetter[D]()
      case Right(Some(v)) =>
        ValidValueSetter(_path, Right(_codec.apply(v)))
      case Left(failed)   =>
        ValidValueSetter(_path, Left(failed))
    }
}

sealed trait ExpectedLensLike[D <: DObject, T] extends ValuePropertyLens[D, T] {
  private[contracts] def isIgnore2BadTypes(dropBadTypes: Boolean): BadTypes =
    if (dropBadTypes) ToDropBadTypes
    else FailOnBadTypes

  private[contracts] def __apply(rawObject: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] =
    rawObject.get(_key) match {
      case None      =>
        ValidResult.failure(ExpectedFailure(this))
      case Some(raw) =>
        GetOps.get(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case NotFound  =>
            ValidResult.failure(ExpectedFailure(this))
          case Found(t)  =>
            ValidResult.success(rawObject + (_key -> _codec(t)))
          case f: Failed =>
            f.toValid
        }
    }

  private[contracts] def __reduce(obj: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] =
    obj.get(_key) match {
      case None      =>
        ValidResult.failure(ExpectedFailure(this))
      case Some(raw) =>
        ReduceOps.reduce(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case NotFound                               =>
            ValidResult.failure(ExpectedFailure(this))
          case Found(reducedRaw) if reducedRaw != raw =>
            ValidResult.success(obj + (_key -> reducedRaw))
          case Found(_)                               =>
            ValidResult.success(obj)
          case Failed(head, tail)                     =>
            ValidResult.failure(head, tail)
        }
    }

  private[contracts] def __verify(obj: RawObject): List[Failure] =
    obj.get(_key) match {
      case None      =>
        List(ExpectedFailure(this))
      case Some(raw) =>
        VerifyOps.verify(this, raw)
    }

  private[contracts] def __reduceDelta(
    deltaObject: RawObject,
    currentObject: RawObject,
    dropBadTypes: Boolean
  ): ValidResult[RawObject] = {
    //TODO Tidyup dropbadTypes in context of delta, dunno if we want to make it boolean as reduce functions do get called
    val drop = if (dropBadTypes) DropBadTypes else FailOnBadTypes
    deltaObject.get(_key) -> currentObject.get(_key) match {
      case (None, _)                          =>
        // We dont return failure here, even if currentValue doesnt have a representation for the property
        // Deltas dont need to repair, they just cant make things worse.
        ValidResult.success(deltaObject)
      case (Some(DNull), None)                =>
        ValidResult.success(deltaObject - _key)
      case (Some(raw), None)                  =>
        ReduceOps.reduce(this, raw, drop) match {
          //Delta not responsible for setting an expected value
          case NotFound                               =>
            ValidResult.success(deltaObject - _key)
          case Found(reducedRaw) if reducedRaw != raw =>
            ValidResult.success(deltaObject + (_key -> reducedRaw))
          case Found(_)                               =>
            ValidResult.success(deltaObject)
          case Failed(head, tail)                     =>
            ValidResult.failure(head, tail)
        }
      case (Some(deltaRaw), Some(currentRaw)) =>
        DeltaReduceOps.deltaReduce(this, deltaRaw, currentRaw, drop) match {
          case DeltaReduced(reducedRaw)       =>
            ValidResult.success(deltaObject + (_key -> reducedRaw))
          case DeltaEmpty                     =>
            ValidResult.success(deltaObject - _key)
          case DeltaRemoving(_) | DeltaRemove =>
            ValidResult.failure(ExpectedFailure(this))
          case DeltaFailed(head, tail)        =>
            ValidResult.failure(head, tail)
        }
    }
  }

  /**
   * Copying from an existing property, if that property is
   * an Empty value on a Maybe Property, it will ignore the copy operation.
   * If its an Expected property it will fail if empty
   * If the copied property is Empty but is a Default Property, it will
   * copy the default value.
   * @param p
   * @return
   */
  final def $copy(p: PropertyLens[D, T], dropBadTypes: Boolean = false): ValidPathSetter[D] =
    ModifySetter(d => p.__get(d, dropBadTypes).toValidOption, identity[T], _codec, _path)

  /**
   * Modifies value for the property.
   * Returns Failure if existing value is Empty or of wrong type.
   * @param f
   * @return
   */
  final def $modify(f: T => T, dropBadTypes: Boolean = false): ValidPathSetter[D] =
    ModifySetter(d => __get(d, dropBadTypes).toValidOption, f, _codec, _path)

  final def $modifyWith(f: T => ValidResult[T], dropBadTypes: Boolean = false): ValidPathSetter[D] =
    ModifyValidSetter(d => __get(d, dropBadTypes).toValidOption, f, _codec, _path)

  final def unapply(d: Delta): Option[Option[T]] =
    PathLensOps.traverse(d.value, _path) match {
      case None    => Some(None)
      case Some(r) =>
        _codec.unapply(r).map(Some(_))
    }
}

sealed trait UnexpectedLensLike[D <: DObject, T] extends ValuePropertyLens[D, T] {
  private[contracts] def isIgnore2BadTypes(dropBadTypes: Boolean): BadTypes =
    if (dropBadTypes) DropBadTypes
    else FailOnBadTypes

  private[contracts] def __reduce(obj: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] =
    obj.get(_key) match {
      case None      =>
        ValidResult.success(obj)
      case Some(raw) =>
        ReduceOps.reduce(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case Found(reducedRaw) if reducedRaw != raw =>
            ValidResult.success(obj + (_key -> reducedRaw))
          case Found(_)                               =>
            ValidResult.success(obj)
          case NotFound                               =>
            ValidResult.success(obj - _key)
          case Failed(head, tail)                     =>
            ValidResult.failure(head, tail)
        }
    }

  private[contracts] def __verify(obj: RawObject): List[Failure] =
    obj.get(_key) match {
      case None      =>
        Nil
      case Some(raw) =>
        VerifyOps.verify(this, raw)
    }

  //Same as Modify, can tidy
  private[contracts] def __reduceDelta(
    deltaObject: RawObject,
    currentObject: RawObject,
    dropBadTypes: Boolean
  ): ValidResult[RawObject] =
    deltaObject.get(_key) -> currentObject.get(_key) match {
      case (None, _)                          =>
        // We dont return failure here, even if currentValue doesnt have a representation for the property
        // Deltas dont need to repair, they just cant make things worse.
        ValidResult.success(deltaObject)
      case (Some(DNull), None)                =>
        ValidResult.success(deltaObject - _key)
      case (Some(raw), None)                  =>
        ReduceOps.reduce(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case NotFound                               =>
            ValidResult.success(deltaObject - _key)
          case Found(reducedRaw) if reducedRaw != raw =>
            ValidResult.success(deltaObject + (_key -> reducedRaw))
          case Found(_)                               =>
            ValidResult.success(deltaObject)
          case Failed(head, tail)                     =>
            ValidResult.failure(head, tail)
        }
      case (Some(deltaRaw), Some(currentRaw)) =>
        DeltaReduceOps.deltaReduce(this, deltaRaw, currentRaw, isIgnore2BadTypes(dropBadTypes)) match {
          case DeltaReduced(reducedRaw)         =>
            ValidResult.success(deltaObject + (_key -> reducedRaw))
          case DeltaEmpty                       =>
            ValidResult.success(deltaObject - _key)
          case DeltaRemove if deltaRaw != DNull =>
            ValidResult.success(deltaObject + (_key -> DNull))
          case DeltaRemove                      =>
            ValidResult.success(deltaObject)
          case DeltaRemoving(reducedRaw)        =>
            ValidResult.success(deltaObject + (_key -> reducedRaw))
          case DeltaFailed(head, tail)          =>
            ValidResult.failure(head, tail)
        }
    }

  @inline
  private[contracts] def __setOrDrop(value: Option[T]): PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v)))

  /**
   * Copying from an existing property, if that property is
   * an Empty value on a Maybe Property, it will drop the value for this property.
   * If the MaybeProperty is in a nonexistent maybe object, it will ignore the copy.
   * If its an Expected property it will fail if empty
   * If the copied property is Empty but is a Default Property, it will
   * copy the default value.
   * If will fail if the source property is of the wrong type
   * @param p
   * @return
   */
  final def $copy(p: PropertyLens[D, T], dropBadTypes: Boolean = false): ValidPathSetter[D] =
    TraversedModifyOrDropSetter(p.__get(_, dropBadTypes), identity[Option[T]], _codec, _path)

}

private[dsentric] trait ExpectedLens[D <: DObject, T] extends ExpectedLensLike[D, T] with ApplicativeLens[D, T] {

  private[contracts] def __get(base: RawObject, dropBadTypes: Boolean): Valid[T] =
    TraversalOps.traverse(base, this, dropBadTypes) match {
      case NotFound   =>
        Failed(ExpectedFailure(this))
      case Found(raw) =>
        GetOps.get(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case NotFound  =>
            Failed(ExpectedFailure(this))
          case Found(t)  =>
            Found(t)
          case f: Failed =>
            f
        }
      case f: Failed  =>
        f
    }

  /**
   * Returns object if found and of correct type
   * Returns ExpectedFailure if object not found, unless property has MaybeObject Property Ancestor
   * Returns None if there is a path with Maybes Object Properties that have no values
   * @param obj
   * @return
   */
  final def $get(obj: D, dropBadTypes: Boolean = false): ValidResult[T] =
    __get(obj.value, dropBadTypes).toValid

  final def $get(validated: Validated[D]): T =
    PathLensOps
      .traverse(validated.validObject.value, _path)
      .flatMap(_codec.unapply)
      .get

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * @param obj
   * @return
   */
  final def unapply(obj: D): Option[T] =
    PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply)

  final def unapply(validated: Validated[D]): Some[T] =
    Some {
      PathLensOps
        .traverse(validated.validObject.value, _path)
        .flatMap(_codec.unapply)
        .get
    }
}

/**
 * This is an Expected Property nested in a Maybe Object
 * @tparam D
 * @tparam T
 */
private[dsentric] trait MaybeExpectedLens[D <: DObject, T]
    extends ExpectedLensLike[D, T]
    with ApplicativeLens[D, Option[T]] {

  private[contracts] def __get(base: RawObject, dropBadTypes: Boolean): MaybeAvailable[T] =
    TraversalOps.maybeTraverse(base, this, dropBadTypes) match {
      case NotFound       =>
        Failed(ExpectedFailure(this))
      case Found(raw)     =>
        GetOps.get(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case NotFound  =>
            Failed(ExpectedFailure(this))
          case Found(t)  =>
            Found(t)
          case f: Failed =>
            f
        }
      case f: Failed      =>
        f
      case PathEmptyMaybe =>
        PathEmptyMaybe
    }

  /**
   * Returns object if found and of correct type
   * Returns ExpectedFailure if object not found, unless property has MaybeObject Property Ancestor
   * Returns None if there is a path with Maybes Object Properties that have no values
   * @param obj
   * @return
   */
  final def $get(obj: D, dropBadTypes: Boolean = false): ValidResult[Option[T]] =
    __get(obj.value, dropBadTypes).toValidOption

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * Matches success None when the maybe path is empty
   * Returns None if type is wrong
   * @param obj
   * @return
   */
  final def unapply(obj: D): Option[Option[T]] =
    TraversalOps.maybeTraverse(obj.value, this, false) match {
      case PathEmptyMaybe =>
        Some(None)
      case Found(r)       =>
        _codec.unapply(r).map(Some.apply)
      case _              =>
        None
    }

  final def unapply(obj: Validated[D]): Option[T] =
    TraversalOps.maybeTraverse(obj.validObject.value, this, false) match {
      case PathEmptyMaybe =>
        None
      case Found(r)       =>
        Some(_codec.unapply(r).get)
      case _              => //Should not occur, maybe need a throw case?
        None
    }
}

private[dsentric] trait MaybeLens[D <: DObject, T] extends UnexpectedLensLike[D, T] with ApplicativeLens[D, Option[T]] {

  private[contracts] def __get(base: RawObject, dropBadTypes: Boolean): MaybeAvailable[T] =
    TraversalOps.maybeTraverse(base, this, dropBadTypes).flatMap { raw =>
      GetOps.get(this, raw, isIgnore2BadTypes(dropBadTypes))
    }

  private[contracts] def __apply(rawObject: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] =
    rawObject.get(_key) match {
      case None      =>
        ValidResult.success(rawObject)
      case Some(raw) =>
        GetOps.get(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case NotFound  =>
            ValidResult.success(rawObject - _key)
          case Found(t)  =>
            ValidResult.success(rawObject + (_key -> _codec(t)))
          case f: Failed =>
            f.toValid
        }
    }

  /**
   * Gets value for the property if found in passed object.
   * Returns failure if value is of unexpected type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType
   * @param obj
   * @return
   */
  final def $get(obj: D, dropBadTypes: Boolean = false): ValidResult[Option[T]] =
    __get(obj.value, dropBadTypes).toValidOption

  /**
   * Gets value for the property if found in passed object, otherwise returns default.
   * Returns failure if value is of unexpected type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType
   * @param obj
   * @return
   */
  final def $getOrElse(obj: D, default: => T, dropBadTypes: Boolean = false): ValidResult[T] =
    __get(obj.value, dropBadTypes).toValidOption.map(_.getOrElse(default))

  /**
   * Removes the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   * @return
   */
  final def $drop: PathSetter[D] =
    ValueDrop(_path)

  /**
   * Sets or Replaces value for the Property if provided
   * Will create object path to Property if objects dont exist.
   * If None is provided it will remove the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   *
   * @param value
   * @return
   */
  final def $setOrDrop(value: Option[T]): PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v)))

  /**
   * Sets value for the Property if provided
   * Will create object path to Property if objects dont exist.
   * Is not concerned with correct type of value.
   * @param value
   * @param dropBadTypes
   * @return
   */
  final def $setIfEmpty(value: T): PathSetter[D] =
    ValueIfEmptySetter(_path, value)

  /**
   * Modifies value for the property.
   * Returns Failure if wrong type.
   * Does nothing if value not found
   * @param f
   * @return
   */
  final def $modify(f: T => T, dropBadTypes: Boolean = false): ValidPathSetter[D] =
    ModifySetter(d => __get(d, dropBadTypes).toValidOption, f, _codec, _path)

  final def $modifyWith(f: T => ValidResult[T], dropBadTypes: Boolean = false): ValidPathSetter[D] =
    ModifyValidSetter(d => __get(d, dropBadTypes).toValidOption, f, _codec, _path)

  /**
   * Modifies or sets the value if it doesnt exist.
   * Will create object path to Property if objects dont exist only if path is expected.
   * Returns failure if value is of the wrong type
   * IMPORTANT set will create the path to the object if not found, this will not (may change)
   *   --perhaps need ExpectedMaybeLens and it would create path
   * @param f
   * @return
   */
  final def $modifyOrSet(f: Option[T] => T, dropBadTypes: Boolean = false): ValidPathSetter[D] =
    TraversedModifySetter(__get(_, dropBadTypes), f, _codec, _path)

  /**
   * Modifies or sets the value if it doesnt exist.
   * Will create object path to Property if objects dont exist only if path is expected.
   * Returns failure if value is of the wrong type
   * IMPORTANT set will create the path to the object if not found, this will not (may change)
   * @param f
   * @return
   */
  final def $modifyOrSetWith(f: Option[T] => ValidResult[T], dropBadTypes: Boolean = false): ValidPathSetter[D] =
    TraversedModifyValidSetter(__get(_, dropBadTypes), f, _codec, _path)

  /**
   * Modifies or sets the value if it doesnt exist.
   * Will create object path to Property if objects dont exist only if path is expected.
   * If None is returned it will remove the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   * Returns failure if value is of the wrong type
   *
   * @param f
   * @return
   */
  final def $modifyOrDrop(f: Option[T] => Option[T], dropBadTypes: Boolean = false): ValidPathSetter[D] =
    TraversedModifyOrDropSetter[D, T](__get(_, dropBadTypes), f, _codec, _path)

  /**
   * Modifies or sets the value if it doesnt exist.
   * Will create object path to Property if objects dont exist only if path is expected.
   * If None is returned it will remove the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   * Returns failure if value is of the wrong type
   *
   * @param f
   * @return
   */
  final def $modifyOrDropWith(f: Option[T] => Option[ValidResult[T]], dropBadTypes: Boolean = false): ValidPathSetter[D] =
    TraversedModifyOrDropValidSetter[D, T](__get(_, dropBadTypes), f, _codec, _path)

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * Like Expected Properties, will return None on incorrect type
   * @param obj
   * @return
   */
  final def unapply(obj: D): Option[Option[T]] =
    PathLensOps.traverse(obj.value, _path) match {
      case None    =>
        Some(None)
      case Some(v) =>
        _codec.unapply(v).map(Some.apply)
    }

  final def unapply(validated: Validated[D]): Some[Option[T]] =
    PathLensOps.traverse(validated.validObject.value, _path) match {
      case None    =>
        Some(None)
      case Some(v) =>
        Some(_codec.unapply(v))
    }

  final def unapply(d: Delta): Option[Option[DNullable[T]]] =
    PathLensOps.traverse(d.value, _path) match {
      case None        => Some(None)
      case Some(DNull) =>
        Some(Some(DNull))
      case Some(r)     =>
        _codec.unapply(r).map(t => Some(DSome(t)))
    }
}

sealed trait DefaultLensLike[D <: DObject, T] extends UnexpectedLensLike[D, T] {

  def _default: T

  private[contracts] def __apply(rawObject: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] =
    rawObject.get(_key) match {
      case None      =>
        ValidResult.success(rawObject + (_key -> _codec(_default)))
      case Some(raw) =>
        GetOps.get(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case NotFound  =>
            ValidResult.success(rawObject + (_key -> _codec(_default)))
          case Found(t)  =>
            ValidResult.success(rawObject + (_key -> _codec(t)))
          case f: Failed =>
            f.toValid
        }
    }

  /**
   * Modifies value for the property.
   * Uses Default value if value not found
   * Returns Failure if existing value is Empty or of wrong type.
   * @param f
   * @return
   */
  final def $modify(f: T => T, dropBadTypes: Boolean = false): ValidPathSetter[D] =
    ModifySetter(d => __get(d, dropBadTypes).toValidOption, f, _codec, _path)

  final def $modifyWith(f: T => ValidResult[T], dropBadTypes: Boolean = false): ValidPathSetter[D] =
    ModifyValidSetter(d => __get(d, dropBadTypes).toValidOption, f, _codec, _path)

  /**
   * Removes the property value from the object if it exists.
   * If the removed property is contained in an otherwise empty nested object, it
   * will remove the entire object, this is recursive.
   * @return
   */
  final def $restore: PathSetter[D] =
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
  final def $setOrRestore(value: Option[T]): PathSetter[D] =
    value.fold[PathSetter[D]](ValueDrop(_path))(v => ValueSetter(_path, _codec(v)))

  final def unapply(d: Delta): Option[Option[DNullable[T]]] =
    PathLensOps.traverse(d.value, _path) match {
      case None        => Some(None)
      case Some(DNull) =>
        Some(Some(DNull))
      case Some(r)     =>
        _codec.unapply(r).map(t => Some(DSome(t)))
    }
}

private[dsentric] trait DefaultLens[D <: DObject, T] extends DefaultLensLike[D, T] with ApplicativeLens[D, T] {

  private[dsentric] def __rawDefault: Raw                                        = _codec(_default)
  private[contracts] def __get(base: RawObject, dropBadTypes: Boolean): Valid[T] =
    TraversalOps.traverse(base, this, dropBadTypes) match {
      case NotFound   =>
        Found(_default)
      case f: Failed  =>
        f
      case Found(raw) =>
        GetOps.get(this, raw, isIgnore2BadTypes(dropBadTypes)) match {
          case NotFound    =>
            Found(_default)
          case f: Failed   =>
            f
          case f: Found[T] =>
            f
        }
    }

  /**
   * Gets value for the property if found in passed object.
   * Otherwise returns the default value.
   * Returns fail if value found to be incorrect type unless
   * incorrectTypeBehaviour is set to EmptyOnIncorrectType, in which case
   * the default value is returned
   * @param obj
   * @return
   */
  final def $get(obj: D, dropBadTypes: Boolean = false): ValidResult[T] =
    __get(obj.value, dropBadTypes).toValid

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * Returns Some(default) if not found
   * @param obj
   * @return
   */
  final def unapply(obj: D): Option[T] =
    PathLensOps
      .traverse(obj.value, _path)
      .fold[Option[T]](Some(_default))(_codec.unapply)

  final def unapply(obj: Validated[D]): Some[T] =
    Some(
      PathLensOps
        .traverse(obj.validObject.value, _path)
        .map(_codec.unapply(_).get)
        .getOrElse(_default)
    )
}

/**
 * This is an Expected Property nested in a Maybe Object
 * @tparam D
 * @tparam T
 */
private[dsentric] trait MaybeDefaultLens[D <: DObject, T]
    extends DefaultLensLike[D, T]
    with ApplicativeLens[D, Option[T]] {

  private[contracts] def __get(base: RawObject, dropBadTypes: Boolean): MaybeAvailable[T] =
    TraversalOps.maybeTraverse(base, this, dropBadTypes).flatMap { raw =>
      GetOps.get(this, raw, isIgnore2BadTypes(dropBadTypes))
    } match {
      case NotFound =>
        Found(_default)
      case t        =>
        t
    }

  /**
   * Returns object if found and of correct type
   * Returns ExpectedFailure if object not found, unless property has MaybeObject Property Ancestor
   * Returns None if there is a path with Maybes Object Properties that have no values
   * @param obj
   * @return
   */
  final def $get(obj: D, dropBadTypes: Boolean = false): ValidResult[Option[T]] =
    __get(obj.value, dropBadTypes).toValidOption

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * Returns Some(Some(default)) if not found or type is incorrect, Some(None) if path empty
   * @param obj
   * @return
   */
  final def unapply(obj: D): Option[Option[T]] =
    TraversalOps.maybeTraverse(obj.value, this, false) match {
      case PathEmptyMaybe =>
        Some(None)
      case Found(r)       =>
        _codec.unapply(r).map { v =>
          Some(v)
        }
      case _              =>
        Some(Some(_default))
    }

  final def unapply(obj: Validated[D]): Some[Option[T]] =
    TraversalOps.maybeTraverse(obj.validObject.value, this, false) match {
      case PathEmptyMaybe =>
        Some(None)
      case Found(r)       =>
        Some(Some(_codec.unapply(r).get))
      case _              =>
        Some(Some(_default))
    }
}
