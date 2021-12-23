package dsentric.contracts

import dsentric._
import dsentric.codecs.DCodec
import dsentric.failure.{ExpectedFailure, Failure, IncorrectTypeFailure, ValidResult}

sealed private[dsentric] trait ObjectPropertyLens[D <: DObject] extends BaseContract[D] with PropertyLens[D, DObject] {

  def _codec: DCodec[DObject]

  private[contracts] def isIgnore2BadTypes(dropBadTypes: Boolean): BadTypes =
    if (dropBadTypes) DropBadTypes
    else FailOnBadTypes

  private[contracts] def __reduceDelta(
    deltaObject: RawObject,
    currentObject: RawObject,
    dropBadTypes: Boolean
  ): ValidResult[RawObject] = {
    val badTypes = isIgnore2BadTypes(dropBadTypes)
    deltaObject.get(_key) -> currentObject.get(_key) match {
      case (None, _)           =>
        ValidResult.success(deltaObject)
      case (Some(DNull), None) =>
        //checking constraint to avoid fishing
        __applyConstraints(NotFound, badTypes) match {
          case Failed(head, tail) =>
            ValidResult.failure(head, tail)
          case _                  =>
            ValidResult.success(deltaObject - _key)
        }

      case (Some(c: RawObject @unchecked), None) if RawObjectOps.reducesEmpty(c) =>
        ValidResult.success(deltaObject - _key)
      case (Some(d), Some(c: RawObject @unchecked))                              =>
        val deltaReduce = DeltaReduceOps.deltaReduce(this, d, c, badTypes)
        __applyConstraints(c, deltaReduce, badTypes) match {
          // We shouldn't be able to outright remove an Expected object
          // But its possible it can be reduced to nothing if all properties are Maybes, so we dont fail on DeltaRemoving
          case DeltaRemove if this.isInstanceOf[ExpectedObjectPropertyLens[D]] =>
            ValidResult.failure(ExpectedFailure(this))
          case DeltaRemove                                                     =>
            ValidResult.success(deltaObject + (_key -> DNull))
          case DeltaEmpty                                                      =>
            ValidResult.success(deltaObject - _key)
          case DeltaRemoving(delta)                                            =>
            ValidResult.success(deltaObject + (_key -> delta))
          case DeltaReduced(delta)                                             =>
            ValidResult.success(deltaObject + (_key -> delta))
          case DeltaFailed(head, tail)                                         =>
            ValidResult.failure(head, tail)
        }
      case (Some(d: RawObject @unchecked), _)                                    =>
        val reduce = ReduceOps.reduce(this, d, isIgnore2BadTypes(dropBadTypes))
        __applyConstraints(reduce, badTypes) match {
          case NotFound           =>
            ValidResult.success(deltaObject - _key)
          case Found(rawObject)   =>
            ValidResult.success(deltaObject + (_key -> rawObject))
          case Failed(head, tail) =>
            ValidResult.failure(head, tail)
        }

      case (Some(_), _) if dropBadTypes =>
        ValidResult.success(deltaObject - _key)

      case (Some(d), _) =>
        ValidResult.failure(IncorrectTypeFailure(this, d))
    }
  }

  /**
   * Apply object contract to modify the object
   * @param f
   * @return
   */
  final def $modify(f: this.type => D => D): D => D =
    f(this)

  /**
   * Apply object contract to modify the object where there are
   * changes requiring verification
   * @param f
   * @return
   */
  final def $verifyModify(f: this.type => D => ValidResult[D]): D => ValidResult[D] =
    f(this)

  /**
   * Verifies the structure of the object against its properties
   * and additional property definition returning a list of possibly failures.
   * @param obj
   * @return
   */
  def $verify(obj: D): List[Failure]

  /**
   * Sets the object content to the passed value.
   * Does nothing if None is passed.
   * @param obj
   * @return
   */
  final def $maybeSet(obj: Option[DObject]): PathSetter[D] =
    obj.fold[PathSetter[D]](IdentitySetter[D]())($set)
}

sealed trait ExpectedObjectPropertyLensLike[D <: DObject] extends ObjectPropertyLens[D] {
  private[contracts] def __reduce(obj: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] = {
    def reduce(rawObject: Map[String, Any]): Available[RawObject] = {
      val reduce = ReduceOps.reduce(this, rawObject, isIgnore2BadTypes(dropBadTypes))
      __applyConstraints(reduce, isIgnore2BadTypes(dropBadTypes))
    }

    (obj.get(_key) match {
      case None | Some(DNull)                    =>
        reduce(RawObject.empty)
      case Some(rawObject: RawObject @unchecked) =>
        reduce(rawObject)
      case Some(_) if dropBadTypes               =>
        reduce(RawObject.empty)
      case Some(r)                               =>
        Failed(IncorrectTypeFailure(this, r))
    }) match {
      case NotFound           =>
        ValidResult.success(obj - _key)
      case Found(r)           =>
        ValidResult.success(obj + (_key -> r))
      case Failed(head, tail) =>
        ValidResult.failure(head, tail)
    }
  }

  private[contracts] def __verify(obj: RawObject): List[Failure] =
    obj.get(_key) match {
      case None                                  =>
        VerifyOps.verify(this, RawObject.empty)
      case Some(rawObject: RawObject @unchecked) =>
        VerifyOps.verify(this, rawObject)
      case Some(r)                               =>
        List(IncorrectTypeFailure(this, r))
    }

  /**
   * Operates like a get on the object, setting the new value, including defaults.
   *
   * @param rawObject
   * @param dropBadTypes
   * @return
   */
  private[contracts] def __apply(
    rawObject: RawObject,
    dropBadTypes: Boolean,
    setDefaultValues: Boolean
  ): ValidResult[RawObject] = {
    def get(propertyObject: RawObject): ValidResult[RawObject] =
      GetOps.get(this, propertyObject, isIgnore2BadTypes(dropBadTypes), setDefaultValues) match {
        case Found(r)  =>
          ValidResult.success(rawObject + (_key -> r))
        case f: Failed =>
          f.toValid
      }

    rawObject.get(_key) match {
      case None                                  =>
        get(RawObject.empty)
      case Some(rawObject: RawObject @unchecked) =>
        get(rawObject)
      case Some(_) if dropBadTypes               =>
        get(RawObject.empty)
      case Some(r)                               =>
        ValidResult.failure(IncorrectTypeFailure(this, r))
    }
  }

}

/**
 * An Expected object doesnt necessarily have to be present if none of its properties
 * are expected.
 * @tparam D
 */
private[dsentric] trait ExpectedObjectPropertyLens[D <: DObject]
    extends ExpectedObjectPropertyLensLike[D]
    with ApplicativeLens[D, DObject] {

  private[contracts] def __get(base: RawObject, dropBadTypes: Boolean, setDefaultValues: Boolean): Valid[DObject] = {
    def get(rawObject: RawObject): Valid[DObject] =
      GetOps.get(this, rawObject, isIgnore2BadTypes(dropBadTypes), setDefaultValues) match {
        case Found(rawObject) if rawObject.isEmpty =>
          Found(DObject.empty)
        case Found(rawObject)                      =>
          //its possible this will return an empty object.
          Found(new DObjectInst(rawObject))

        case f: Failed =>
          f
      }

    TraversalOps.traverse(base, this, dropBadTypes) match {
      case NotFound                               =>
        get(RawObject.empty)
      case Found(rawObject: RawObject @unchecked) =>
        get(rawObject)
      case Found(_) if dropBadTypes               =>
        get(RawObject.empty)
      case Found(r)                               =>
        Failed(IncorrectTypeFailure(this, r))
      case f: Failed                              =>
        f
    }
  }

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * TODO: handle path failure
   * @param obj
   * @return
   */
  final def unapply(obj: D): Option[DObject] =
    PathLensOps.traverse(obj.value, _path) match {
      case None    => Some(DObject.empty)
      case Some(v) => _codec.unapply(v)
    }

  final def unapply(obj: Validated[D]): Some[DObject] =
    PathLensOps
      .traverse(obj.validObject.value, _path)
      .flatMap(_codec.unapply) match {
      case s: Some[DObject] =>
        s
      case None             =>
        Some(DObject.empty)
    }

  /**
   * Returns object for this property.
   * Will return failure if any of the properties would fail
   * or the additional properties are invalid
   * Default properties that havent been defined will be provided in the object
   *
   * @param obj
   * @return
   */
  final def $get(obj: D, dropBadTypes: Boolean = false, setDefaultValues: Boolean = true): ValidResult[DObject] =
    __get(obj.value, dropBadTypes, setDefaultValues).toValid

  final def $get[D2 <: D](validated: Validated[D2]): DObject                            =
    $get(validated, true)
  final def $get[D2 <: D](validated: Validated[D2], setDefaultValues: Boolean): DObject =
    __get(validated.validObject.value, false, setDefaultValues).toOption.getOrElse(DObject.empty)
}

private[dsentric] trait MaybeExpectedObjectPropertyLens[D <: DObject]
    extends ExpectedObjectPropertyLensLike[D]
    with ApplicativeLens[D, Option[DObject]] {

  private[contracts] def __get(
    base: RawObject,
    dropBadTypes: Boolean,
    setDefaultValues: Boolean
  ): MaybeAvailable[DObject] = {
    def get(rawObject: Map[String, Any]): MaybeAvailable[DObject] =
      GetOps
        .get(this, rawObject, isIgnore2BadTypes(dropBadTypes), setDefaultValues)
        .flatMap(a => Found(new DObjectInst(a)))

    TraversalOps.maybeTraverse(base, this, dropBadTypes) match {
      case NotFound                               =>
        get(RawObject.empty)
      case Found(rawObject: RawObject @unchecked) =>
        get(rawObject)
      case Found(_) if dropBadTypes               =>
        get(RawObject.empty)
      case Found(r)                               =>
        Failed(IncorrectTypeFailure(this, r))
      case f: Failed                              =>
        f
      case PathEmptyMaybe                         =>
        PathEmptyMaybe
    }
  }

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * @param obj
   * @return
   */
  final def unapply(obj: D): Option[Option[DObject]] =
    TraversalOps.maybeTraverse(obj.value, this, false) match {
      case PathEmptyMaybe =>
        Some(None)
      case Found(r)       =>
        _codec.unapply(r).map(Some(_))
      case _: Failed      =>
        None
      case _              =>
        Some(Some(DObject.empty))
    }

  final def unapply(obj: Validated[D]): Some[Option[DObject]] =
    TraversalOps.maybeTraverse(obj.validObject.value, this, false) match {
      case PathEmptyMaybe =>
        Some(None)
      case Found(r)       =>
        Some(Some(_codec.unapply(r).get))
      case _              =>
        Some(Some(DObject.empty))
    }

  final def $get[D2 <: D](validated: Validated[D2]): Option[DObject]                            =
    $get(validated, true)
  final def $get[D2 <: D](validated: Validated[D2], setDefaultValues: Boolean): Option[DObject] =
    __get(validated.validObject.value, false, setDefaultValues) match {
      case PathEmptyMaybe => None
      case Found(d)       => Some(d)
      case _              => Some(DObject.empty)
    }

  /**
   * Returns object for this property.
   * Will return failure if any of the properties would fail
   * or the additional properties are invalid
   * Returns None if there is a path with Maybes Object Properties that have no values
   * Default properties that havent been defined will be provided in the object
   *
   * @param obj
   * @return
   */
  final def $get(
    obj: D,
    dropBadTypes: Boolean = false,
    setDefaultValues: Boolean = true
  ): ValidResult[Option[DObject]] =
    __get(obj.value, dropBadTypes, setDefaultValues).toValidOption
}

/**
 * Object lens for a Property which contains an object or could be empty.
 * @tparam D
 */
private[dsentric] trait MaybeObjectPropertyLens[D <: DObject]
    extends ObjectPropertyLens[D]
    with ApplicativeLens[D, Option[DObject]] {

  private[contracts] def __get(
    base: RawObject,
    dropBadTypes: Boolean,
    setDefaultValues: Boolean
  ): MaybeAvailable[DObject] = {
    def reduce(rawObject: Map[String, Any]): MaybeAvailable[DObject] =
      GetOps
        .get(this, rawObject, isIgnore2BadTypes(dropBadTypes), setDefaultValues)
        .flatMap(a => Found(new DObjectInst(a)))

    TraversalOps.maybeTraverse(base, this, dropBadTypes).flatMap {
      case rawObject: RawObject @unchecked =>
        reduce(rawObject)
      case _ if dropBadTypes               =>
        NotFound
      case raw                             =>
        Failed(IncorrectTypeFailure(this, raw))
    }
  }

  /**
   * Operates like a get on the object, setting the new value, including defaults.
   *
   * @param rawObject
   * @param dropBadTypes
   * @return
   */
  private[contracts] def __apply(
    rawObject: RawObject,
    dropBadTypes: Boolean,
    setDefaultValues: Boolean
  ): ValidResult[RawObject] = {
    def get(propertyObject: RawObject): ValidResult[RawObject] =
      GetOps.get(this, propertyObject, isIgnore2BadTypes(dropBadTypes), setDefaultValues) match {
        case Found(r) if r.isEmpty =>
          ValidResult.success(rawObject - _key)
        case Found(r)              =>
          ValidResult.success(rawObject + (_key -> r))
        case f: Failed             =>
          f.toValid
      }

    rawObject.get(_key) match {
      case None                                  =>
        ValidResult.success(rawObject)
      case Some(rawObject: RawObject @unchecked) =>
        get(rawObject)
      case Some(_) if dropBadTypes               =>
        ValidResult.success(rawObject - _key)
      case Some(r)                               =>
        ValidResult.failure(IncorrectTypeFailure(this, r))
    }
  }

  private[contracts] def __reduce(obj: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] = {
    def reduce(rawObject: Map[String, Any]): Available[RawObject] = {
      val reduce = ReduceOps.reduce(this, rawObject, isIgnore2BadTypes(dropBadTypes))
      __applyConstraints(reduce, isIgnore2BadTypes(dropBadTypes))
    }

    (obj.get(_key) match {
      case None | Some(DNull)                                         =>
        NotFound
      case Some(rawObject: RawObject @unchecked) if rawObject.isEmpty =>
        NotFound
      case Some(rawObject: RawObject @unchecked)                      =>
        reduce(rawObject)
      case Some(_) if dropBadTypes                                    =>
        NotFound
      case Some(r)                                                    =>
        Failed(IncorrectTypeFailure(this, r))
    }) match {
      case NotFound                  =>
        ValidResult.success(obj - _key)
      case Found(r)                  =>
        ValidResult.success(obj + (_key -> r))
      case _: Failed if dropBadTypes =>
        ValidResult.success(obj - _key)
      case Failed(head, tail)        =>
        ValidResult.failure(head, tail)
    }
  }

  private[contracts] def __verify(obj: RawObject): List[Failure] =
    obj.get(_key) match {
      case None                                  =>
        Nil
      case Some(rawObject: RawObject @unchecked) =>
        VerifyOps.verify(this, rawObject)
      case Some(r)                               =>
        List(IncorrectTypeFailure(this, r))
    }

  final def unapply(obj: D): Option[Option[DObject]] =
    TraversalOps.maybeTraverse(obj.value, this, false) match {
      case PathEmptyMaybe                 =>
        Some(None)
      case NotFound                       =>
        Some(None)
      case Found(t: RawObject @unchecked) =>
        Some(Some(new DObjectInst(t)))
      case Found(_)                       =>
        None
      case Failed(_, _)                   => None
    }

  final def unapply(obj: Validated[D]): Some[Option[DObject]] =
    PathLensOps.traverse(obj.validObject.value, this._path) match {
      case None    =>
        Some(None)
      case Some(r) =>
        Some(Some(_codec.unapply(r).get))
    }

  final def $drop: PathSetter[D] =
    ValueDrop(_path)

  /**
   * Returns object for this property.
   * Will return failure if any of the properties would fail
   * or the additional properties are invalid
   * Returns None if there is a path with Maybes Object Properties that have no values
   * Default properties that havent been defined will be provided in the object
   *
   * @param obj
   * @return
   */
  final def $get(
    obj: D,
    dropBadTypes: Boolean = false,
    setDefaultValues: Boolean = true
  ): ValidResult[Option[DObject]] =
    __get(obj.value, dropBadTypes, setDefaultValues).toValidOption

  final def $get[D2 <: D](validated: Validated[D2]): Option[DObject]                            =
    $get(validated, true)
  final def $get[D2 <: D](validated: Validated[D2], setDefaultValues: Boolean): Option[DObject] =
    __get(validated.validObject.value, false, setDefaultValues) match {
      case PathEmptyMaybe => None
      case Found(d)       => Some(d)
      case _              => None
    }

  final def $getOrElse(
    default: DObject
  )(obj: D, dropBadTypes: Boolean = false, setDefaultValues: Boolean = true): ValidResult[DObject] =
    __get(obj.value, dropBadTypes, setDefaultValues).toValidOption.map(_.getOrElse(default))
}
