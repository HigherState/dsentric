package dsentric.contracts

import dsentric._
import cats.data._
import dsentric.codecs.DCodec
import dsentric.codecs.std.DCodecs
import dsentric.failure.{ClosedContractFailure, ExpectedFailure, Failure, IncorrectTypeFailure, ValidResult}

private[dsentric] sealed trait ObjectPropertyLens[D <: DObject]
  extends BaseContract[D] with PropertyLens[D, DObject]{

  def _codec: DCodec[DObject]

  private[contracts] def isIgnore2BadTypes(dropBadTypes:Boolean):BadTypes =
    if (dropBadTypes) DropBadTypes
    else FailOnBadTypes

  private[contracts] def __reduceDelta(deltaObject:RawObject, currentObject:RawObject, dropBadTypes:Boolean):ValidResult[RawObject] = {
    val badTypes = isIgnore2BadTypes(dropBadTypes)
    deltaObject.get(_key) -> currentObject.get(_key) match {
      case (None, _) =>
        ValidResult.success(deltaObject)
      case (Some(DNull), None) =>
        //checking constraint to avoid fishing
        __applyConstraints(NotFound, badTypes) match {
          case Failed(head, tail) =>
            ValidResult.failure(head, tail)
          case _ =>
            ValidResult.success(deltaObject - _key)
        }

      case (Some(c:RawObject@unchecked), None) if RawObjectOps.reducesEmpty(c) =>
        ValidResult.success(deltaObject - _key)
      case (Some(d), Some(c:RawObject@unchecked)) =>
        val deltaReduce = ObjectPropertyLensOps.deltaReduce(this, d, c, badTypes)
        __applyConstraints(c, deltaReduce, badTypes) match {
          // We shouldn't be able to outright remove an Expected object
          // But its possible it can be reduced to nothing if all properties are Maybes, so we dont fail on DeltaRemoving
          case DeltaRemove if this.isInstanceOf[ExpectedObjectPropertyLens[D]] =>
            ValidResult.failure(ExpectedFailure(this))
          case DeltaRemove =>
            ValidResult.success(deltaObject + (_key -> DNull))
          case DeltaEmpty =>
            ValidResult.success(deltaObject - _key)
          case DeltaRemoving(delta) =>
            ValidResult.success(deltaObject + (_key -> delta))
          case DeltaReduced(delta) =>
            ValidResult.success(deltaObject + (_key -> delta))
          case DeltaFailed(head, tail) =>
            ValidResult.failure(head, tail)
        }
      case (Some(d:RawObject@unchecked), _) =>
        val reduce = ObjectPropertyLensOps.reduce(this, d, isIgnore2BadTypes(dropBadTypes))
        __applyConstraints(reduce, badTypes) match {
          case NotFound =>
            ValidResult.success(deltaObject - _key)
          case Found(rawObject) =>
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
  final def $modify(f:this.type => D => D):D => D =
    f(this)

  /**
   * Apply object contract to modify the object where there are
   * changes requiring verification
   * @param f
   * @return
   */
  final def $verifyModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  /**
   * Verifies the structure of the object against its properties
   * and additional property definition returning a list of possibly failures.
   * @param obj
   * @return
   */
  def $verify(obj:D):List[Failure]

  /**
   * Returns object for this property.
   * Will return failure if any of the properties would fail
   * or the additional properties are invalid
   * Returns None if there is a path with Maybes Object Properties that have no values
   * Default properties that havent been defined will be provided in the object
   *
   * Tricky expected missing on maybe when no object vs expected missing when there is an object....
   *
   *
   * @param obj
   * @return
   */
  final def $get(obj:D, dropBadTypes:Boolean = false):ValidResult[Option[DObject]] =
    __get(obj.value, dropBadTypes).toValidOption
  /**
   * Sets the object content to the passed value.
   * Does nothing if None is passed.
   * @param obj
   * @return
   */
  final def $maybeSet(obj:Option[DObject]):PathSetter[D] =
    obj.fold[PathSetter[D]](IdentitySetter[D]())($set)
}

sealed trait ExpectedObjectPropertyLensLike[D <: DObject] extends ObjectPropertyLens[D] {
  private[contracts] def __reduce(obj: RawObject, dropBadTypes:Boolean):ValidResult[RawObject] = {
    def reduce(rawObject:Map[String, Any]):Available[RawObject] = {
      val reduce = ObjectPropertyLensOps.reduce(this, rawObject, isIgnore2BadTypes(dropBadTypes))
      __applyConstraints(reduce, isIgnore2BadTypes(dropBadTypes))
    }

    (obj.get(_key) match {
      case None | Some(DNull) =>
        reduce(RawObject.empty)
      case Some(rawObject:RawObject@unchecked) =>
        reduce(rawObject)
      case Some(_) if dropBadTypes =>
        reduce(RawObject.empty)
      case Some(r) =>
        Failed(IncorrectTypeFailure(this, r))
    }) match {
      case NotFound =>
        ValidResult.success(obj - _key)
      case Found(r) =>
        ValidResult.success(obj + (_key -> r))
      case Failed(head, tail) =>
        ValidResult.failure(head, tail)
    }
  }

  /**
   * Operates like a get on the object, setting the new value, including defaults.
   *
   * @param rawObject
   * @param dropBadTypes
   * @return
   */
  private[contracts] def __apply(rawObject: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] = {
    def get(rawObject:RawObject):ValidResult[RawObject] =
      ObjectPropertyLensOps.get(this, rawObject, isIgnore2BadTypes(dropBadTypes)) match {
        case Found(r) =>
          ValidResult.success(rawObject + (_key -> r))
        case f:Failed =>
          f.toValid
      }

    rawObject.get(_key) match {
      case None =>
        get(RawObject.empty)
      case Some(rawObject:RawObject@unchecked) =>
        get(rawObject)
      case Some(_) if dropBadTypes =>
        get(RawObject.empty)
      case Some(r) =>
        ValidResult.failure(IncorrectTypeFailure(this, r))
    }
  }
}

/**
 * An Expected object doesnt necessarily have to be present if none of its properties
 * are expected.
 * @tparam D
 */
private[dsentric] trait ExpectedObjectPropertyLens[D <: DObject] extends ExpectedObjectPropertyLensLike[D] with ApplicativeLens[D, DObject]{

  private[contracts] def __get(base:RawObject, dropBadTypes:Boolean):Valid[DObject] = {
    def get(rawObject:RawObject):Valid[DObject] =
      ObjectPropertyLensOps.get(this, rawObject, isIgnore2BadTypes(dropBadTypes)) match {
        case Found(rawObject) if rawObject.isEmpty =>
          Found(DObject.empty)
        case Found(rawObject) =>
          //its possible this will return an empty object.
          Found(new DObjectInst(rawObject))

        case f:Failed =>
          f
      }

    TraversalOps.traverse(base, this, dropBadTypes) match {
      case NotFound  =>
        get(RawObject.empty)
      case Found(rawObject:RawObject@unchecked) =>
        get(rawObject)
      case Found(_) if dropBadTypes =>
        get(RawObject.empty)
      case Found(r) =>
        Failed(IncorrectTypeFailure(this, r))
      case f:Failed =>
        f
    }
  }

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[DObject] =
    PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply)
}

private[dsentric] trait MaybeExpectedObjectPropertyLens[D <: DObject] extends ExpectedObjectPropertyLensLike[D] with ApplicativeLens[D, Option[DObject]]{

  private[contracts] def __get(base:RawObject, dropBadTypes:Boolean):MaybeAvailable[DObject] = {
    def get(rawObject:Map[String, Any]):MaybeAvailable[DObject] =
      ObjectPropertyLensOps.get(this, rawObject, isIgnore2BadTypes(dropBadTypes))
        .flatMap(a => Found(new DObjectInst(a)))

    TraversalOps.maybeTraverse(base, this, dropBadTypes) match {
      case NotFound  =>
        get(RawObject.empty)
      case Found(rawObject:RawObject@unchecked) =>
        get(rawObject)
      case Found(_) if dropBadTypes =>
        get(RawObject.empty)
      case Found(r) =>
        Failed(IncorrectTypeFailure(this, r))
      case f:Failed =>
        f
      case PathEmptyMaybe =>
        PathEmptyMaybe
    }
  }


  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[Option[DObject]] =
    TraversalOps.maybeTraverse(obj.value, this, false) match {
      case PathEmptyMaybe =>
        Some(None)
      case Found(r) =>
        _codec.unapply(r).map(Some(_))
      case _ =>
        None
    }
}

/**
 * Object lens for a Property which contains an object or could be empty.
 * @tparam D
 */
private[dsentric] trait MaybeObjectPropertyLens[D <: DObject] extends ObjectPropertyLens[D] {

  private[contracts] def __get(base:RawObject, dropBadTypes:Boolean):MaybeAvailable[DObject] = {
    def reduce(rawObject:Map[String, Any]):MaybeAvailable[DObject] =
      ObjectPropertyLensOps.reduce(this, rawObject, isIgnore2BadTypes(dropBadTypes))
        .flatMap(a => Found(new DObjectInst(a)))

    TraversalOps.maybeTraverse(base, this, dropBadTypes).flatMap {
      case rawObject:RawObject@unchecked =>
        reduce(rawObject)
      case _ if dropBadTypes =>
        NotFound
      case raw =>
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
  private[contracts] def __apply(rawObject: RawObject, dropBadTypes: Boolean): ValidResult[RawObject] = {
    def get(rawObject:RawObject):ValidResult[RawObject] =
      ObjectPropertyLensOps.get(this, rawObject, isIgnore2BadTypes(dropBadTypes)) match {
        case Found(r) if r.isEmpty =>
          ValidResult.success(rawObject - _key)
        case Found(r) =>
          ValidResult.success(rawObject + (_key -> r))
        case f:Failed =>
          f.toValid
      }

    rawObject.get(_key) match {
      case None =>
        ValidResult.success(rawObject)
      case Some(rawObject:RawObject@unchecked) =>
        get(rawObject)
      case Some(_) if dropBadTypes =>
        ValidResult.success(rawObject - _key)
      case Some(r) =>
        ValidResult.failure(IncorrectTypeFailure(this, r))
    }
  }

  private[contracts] def __reduce(obj: RawObject, dropBadTypes:Boolean):ValidResult[RawObject] = {
    def reduce(rawObject:Map[String, Any]):Available[RawObject] = {
      val reduce = ObjectPropertyLensOps.reduce(this, rawObject, isIgnore2BadTypes(dropBadTypes))
      __applyConstraints(reduce, isIgnore2BadTypes(dropBadTypes))
    }

    (obj.get(_key) match {
      case None | Some(DNull) =>
        NotFound
      case Some(rawObject:RawObject@unchecked) if rawObject.isEmpty =>
        NotFound
      case Some(rawObject:RawObject@unchecked) =>
        reduce(rawObject)
      case Some(_) if dropBadTypes =>
        NotFound
      case Some(r) =>
        Failed(IncorrectTypeFailure(this, r))
    }) match {
      case NotFound =>
        ValidResult.success(obj - _key)
      case Found(r) =>
        ValidResult.success(obj + (_key -> r))
      case f:Failed if dropBadTypes =>
        ValidResult.success(obj - _key)
      case Failed(head, tail) =>
        ValidResult.failure(head, tail)
    }
  }


  final def unapply(obj:D):Option[Option[DObject]] =
    TraversalOps.maybeTraverse(obj.value, this, false) match {
      case PathEmptyMaybe =>
        Some(None)
      case NotFound =>
        Some(None)
      case Found(t:RawObject@unchecked) =>
        Some(Some(new DObjectInst(t)))
      case Found(_) =>
        None
      case Failed(_, _) => None
    }

}

private[dsentric] object ObjectPropertyLensOps extends DeltaReduceOps with GetOps {

  /**
   * Validates Types and structure, applies any defaults if empty
   * */
  def get[D <: DObject](baseContract:BaseContract[D], obj:RawObject, badTypes:BadTypes):Valid[RawObject] = {
    def getAdditionalProperties:ValidResult[RawObject] = {
      val exclude = baseContract._fields.keySet
      baseContract match {
        case a:AdditionalProperties[Any, Any]@unchecked =>
          val (baseObject, additionalObject) = obj.partition(p => exclude(p._1))
          val codec = DCodecs.keyValueMapCodec(a._additionalKeyCodec, a._additionalValueCodec)
          getMap(a._root, a._path, badTypes, codec, additionalObject)
            .flatMap(t => Found(codec.apply(t))) match {
            case Found(rawObject) if rawObject == additionalObject =>
              ValidResult.success(obj)
            case Found(rawObject) =>
              ValidResult.success(baseObject ++ rawObject)
            case NotFound =>
              ValidResult.success(baseObject)
            case Failed(head, tail) =>
              ValidResult.failure(head, tail)
          }
        case _ if badTypes == DropBadTypes =>
          ValidResult.success(obj.view.filterKeys(exclude).toMap)
        case _ =>
          obj.keys
            .filterNot(exclude)
            .map(k => ClosedContractFailure(baseContract._root, baseContract._path, k))
            .toList match {
            case head :: tail =>
              ValidResult.failure(head, tail)
            case Nil =>
              ValidResult.success(obj)
          }
      }
    }

    val drop = badTypes.nest == DropBadTypes
    baseContract._fields.foldLeft(getAdditionalProperties){
      case (Right(d), (_, p)) =>
        p.__apply(d, drop)
      case (l@Left(nel), (_, p)) =>
        p.__apply(obj, drop) match {
          case Right(_) =>
            l
          case Left(nel2) =>
            Left(nel ::: nel2)
        }
    } match {
      case Right(obj) =>
        Found(obj)
      case Left(NonEmptyList(head, tail)) =>
        Failed(head, tail)
    }
  }
  /**
   * Reduces empty property fields, removing DCodec values that return NotFound
   *
   * Ultimately clearing out empty Objects as well
   * Will also remove any nulls
   * */
  def reduce[D <: DObject](baseContract:BaseContract[D], obj:RawObject, badTypes:BadTypes):Available[RawObject] = {

    def reduceAdditionalProperties:ValidResult[RawObject] = {
      val exclude = baseContract._fields.keySet
      baseContract match {
        case a:AdditionalProperties[Any, Any]@unchecked =>
          val (baseObject, additionalObject) = obj.partition(p => exclude(p._1))
          if (additionalObject.isEmpty) ValidResult.success(baseObject)
          else {
            reduceMap(a._root, a._path, badTypes, DCodecs.keyValueMapCodec(a._additionalKeyCodec, a._additionalValueCodec), additionalObject) match {
              case Found(rawObject) if rawObject == additionalObject =>
                ValidResult.success(obj)
              case Found(rawObject) =>
                ValidResult.success(baseObject ++ rawObject)
              case NotFound =>
                ValidResult.success(baseObject)
              case Failed(head, tail) =>
                ValidResult.failure(head, tail)
            }
          }
        case _ if badTypes == DropBadTypes =>
          ValidResult.success(obj.view.filterKeys(exclude).toMap)
        case _ =>
          val closed = obj.view.filterKeys(key => !exclude.contains(key))

          if (closed.isEmpty) ValidResult.success(obj)
          else RawObjectOps.reduceMap(closed.toMap)
            .toList
            .flatMap(_.keys.map(key => ClosedContractFailure(baseContract._root, baseContract._path, key))) match {
              case head :: tail =>
                ValidResult.failure(head, tail)
              case Nil =>
                ValidResult.success(obj -- closed.keys)
            }
      }
    }

    val drop = badTypes.nest == DropBadTypes
    baseContract._fields.foldLeft(reduceAdditionalProperties){
      case (Right(d), (_, p)) =>
        p.__reduce(d, drop)
      case (l@Left(nel), (_, p)) =>
        p.__reduce(obj, drop) match {
          case Right(_) =>
            l
          case Left(nel2) =>
            Left(nel ::: nel2)
        }
    } match {
      case Right(obj) if obj.isEmpty =>
        NotFound
      case Right(obj) =>
        Found(obj)
      case Left(NonEmptyList(head, tail)) =>
        Failed(head, tail)
    }
  }

  /**
   * Delta on the object only needs to check the properties that it is changing to valid.
   * It is not the responsibility of the delta to be validated against incorrect types or expected requirements
   * in the object that it is not changing.
   * @param baseContract
   * @param delta
   * @param currentObject
   * @param badTypes
   * @tparam D
   * @return
   */
  def deltaReduce[D <: DObject](baseContract:BaseContract[D], delta:Raw, currentObject:RawObject, badTypes:BadTypes):DeltaReduce[RawObject] = {
    def deltaReduceAdditionalProperties(deltaObject:RawObject):ValidResult[RawObject] = {
      val exclude = baseContract._fields.keySet
      baseContract match {
        case a:AdditionalProperties[Any, Any]@unchecked =>
          val (baseDelta, additionalDelta) = deltaObject.partition(p => exclude(p._1))
          if (additionalDelta.nonEmpty) {
            val additionalCurrent = currentObject.filter(p => !exclude(p._1))
            deltaReduceMap(a._root, a._path, badTypes, DCodecs.keyValueMapCodec(a._additionalKeyCodec, a._additionalValueCodec), additionalDelta, additionalCurrent) match {
              case DeltaEmpty | DeltaRemove => //Delta Remove isnt possible in this instance
                ValidResult.success(baseDelta)
              case DeltaRemoving(delta) =>
                ValidResult.success(baseDelta ++ delta)
              case DeltaReduced(delta) =>
                ValidResult.success(baseDelta ++ delta)
              case DeltaFailed(head, tail) =>
                ValidResult.failure(head, tail)
            }
          }
          else
            ValidResult.success(baseDelta)
        case _ =>
          deltaObject.foldLeft[ValidResult[RawObject]](ValidResult.success(deltaObject)) {
            case (r, (key, _)) if exclude(key) =>
              r
            //Allow removing of existing additional properties that shouldn't be there
            case (Right(d), (key, DNull)) if currentObject.contains(key) =>
              ValidResult.success(d)
            case (Right(d), (key, DNull | RawObject.empty)) =>
              ValidResult.success(d - key)
            case (failed, (_, DNull | RawObject.empty)) =>
              failed
            case (Right(d), (key, __)) if badTypes == DropBadTypes =>
              ValidResult.success(d - key)
            case (Right(_), (key, __)) =>
              ValidResult.failure(ClosedContractFailure(baseContract._root, baseContract._path, key))
            case (f, _) if badTypes == DropBadTypes =>
              f
            case (Left(NonEmptyList(head, tail)), (key, _)) =>
              ValidResult.failure(ClosedContractFailure(baseContract._root, baseContract._path, key), head :: tail)
          }
      }
    }

    delta match {
      case DNull =>
        DeltaRemove
      case deltaObject:RawObject@unchecked =>
        val drop = badTypes.nest == DropBadTypes //not sure if this means anything in Delta, need to review
        val init = deltaReduceAdditionalProperties(deltaObject)
        baseContract._fields
          .view.filterKeys(deltaObject.contains)
          .foldLeft(init){
            case (Right(d), (_, p)) =>
              p.__reduceDelta(d, currentObject, drop)
            case (l@Left(nel), (_, p)) =>
              p.__reduceDelta(deltaObject, currentObject, drop) match {
                case Right(_) =>
                  l
                case Left(nel2) =>
                  Left(nel ::: nel2)
              }
          } match {
          case Left(nel) =>
            DeltaFailed(nel.head, nel.tail)
          case Right(delta) =>
            if (delta.isEmpty) DeltaEmpty
            else if (RawObjectOps.rightReduceConcatMap(currentObject, currentObject).isEmpty) //TODO improve performance
              DeltaRemoving(delta)
            else
              DeltaReduced(delta)
        }

      case _ if badTypes == DropBadTypes =>
        DeltaEmpty
    }
  }
}


