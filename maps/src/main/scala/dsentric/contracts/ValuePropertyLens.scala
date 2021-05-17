package dsentric.contracts

import cats.data.NonEmptyList
import dsentric._
import dsentric.codecs.{DCodec, DCollectionCodec, DContractCodec, DMapCodec, DValueCodec}
import dsentric.failure.{ExpectedFailure, Failure, IncorrectKeyTypeFailure, IncorrectTypeFailure, MissingElementFailure, StructuralFailure, ValidResult}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait ValuePropertyLens[D <: DObject, T] extends PropertyLens[D, T]

private[dsentric] trait ExpectedLens[D <: DObject, T] extends ValuePropertyLens[D, T] with ApplicativeLens[D, T] {

  private def isIgnore2BadTypes(ignoreBadTypes:Boolean):BadTypes =
    if (ignoreBadTypes) ToDropBadTypes
    else FailOnBadTypes

  private[contracts] def __get(data:D, ignoreBadTypes:Boolean):Traversed[T] =
    TraversalOps.traverse(data.value, this, ignoreBadTypes) match {
      case NotFound =>
        Failed(ExpectedFailure(this))
      case Found(raw) =>
        ValuePropertyLensOps.get(this, raw, isIgnore2BadTypes(ignoreBadTypes))
          .failNotFound(ExpectedFailure(this))
    }

  private[contracts] def __reduce(obj: RawObject, ignoreBadTypes:Boolean):Available[RawObject] =
    obj.get(_key) match {
      case None =>
        Failed(ExpectedFailure(this))
      case Some(raw) =>
        ValuePropertyLensOps.reduce(this, raw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case NotFound =>
            Failed(ExpectedFailure(this))
          case Found(reducedRaw) if reducedRaw != raw =>
            Found(obj + (_key -> reducedRaw))
          case Found(_) =>
            Found(obj)
          case f:Failed =>
            f
        }
    }

  private[contracts] def __reduceDelta(deltaObject:RawObject, currentValue:RawObject, ignoreBadTypes:Boolean):ValidResult[RawObject] =
    deltaObject.get(_key) -> currentValue.get(_key) match {
      case (None, _) =>
        // We dont return failure here, even if currentValue doesnt have a representation for the property
        // Deltas dont need to repair, they just cant make things worse.
        Right(deltaObject)
      case (Some(raw), None) =>
        ValuePropertyLensOps.reduce(this, raw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case NotFound =>
            Left(NonEmptyList(ExpectedFailure(this), Nil))
          case Found(reducedRaw) if reducedRaw != raw =>
            Right(deltaObject + (_key -> reducedRaw))
          case Found(_) =>
            Right(deltaObject)
          case Failed(head, tail) =>
            Left(NonEmptyList(head, tail))
        }
      case (Some(deltaRaw), Some(currentRaw)) =>
        ValuePropertyLensOps.deltaReduce(this, deltaRaw, currentRaw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case DeltaReduced(reducedRaw) =>
            Right(deltaObject + (_key -> reducedRaw))
          case DeltaEmpty =>
            Right(deltaObject - _key)
          case DeltaRemoving(_) | DeltaRemove =>
            Left(NonEmptyList(ExpectedFailure(this), Nil))
          case DeltaFailed(head, tail) =>
            Left(NonEmptyList(head, tail))
        }
    }

  /**
   * Verifies the Type of the value for the Property and its existence.
   * Returns failure if object not found, unless property has MaybeObject Ancestor
   * TODO make performant
   * @param obj
   * @return
   */
  final def $verify(obj:D):List[StructuralFailure] =
    __get(obj, false) match {
      case Failed(head, tail) =>
        head :: tail
      case _ =>
        Nil
    }

  /**
   * Returns object if found and of correct type
   * Returns ExpectedFailure if object not found, unless property has MaybeObject Property Ancestor
   * Returns None if there is a path with Maybes Object Properties that have no values
   * @param obj
   * @return
   */
  final def $get(obj:D, ignoreBadTypes:Boolean = false):ValidResult[Option[T]] =
    __get(obj, ignoreBadTypes).toValidOption

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
  final def $modify(f:T => T, ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    ModifySetter(d => __get(d, ignoreBadTypes).toValidOption, f, __set)

  final def $modifyWith(f:T => ValidResult[T], ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    ModifyValidSetter(d => __get(d, ignoreBadTypes).toValidOption, f, __set)

  /**
   * Copying from an existing property, if that property is
   * an Empty value on a Maybe Property, it will ignore the copy operation.
   * If its an Expected property it will fail if empty
   * If the copied property is Empty but is a Default Property, it will
   * copy the default value.
   * @param p
   * @return
   */
  final def $copy(p:PropertyLens[D, T], ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    ModifySetter(d => p.__get(d, ignoreBadTypes).toValidOption, identity[T], __set)

  //  final lazy val $delta:ExpectedDelta[D, T] =
  //    new ExpectedDelta(this)

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * TODO Would be cool if we could take into account pathing, IE has a maybe path, so should return Option of T
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[T] =
    PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply)
}

private[dsentric] trait MaybeLens[D <: DObject, T] extends ValuePropertyLens[D, T] with ApplicativeLens[D, Option[T]] {

  private def isIgnore2BadTypes(ignoreBadTypes:Boolean):BadTypes =
    if (ignoreBadTypes) DropBadTypes
    else FailOnBadTypes

  private[contracts] def __get(data:D, ignoreBadTypes:Boolean):Traversed[T] =
    TraversalOps.traverse(data.value, this, ignoreBadTypes).flatMap{raw =>
      ValuePropertyLensOps.get(this, raw, isIgnore2BadTypes(ignoreBadTypes))
    }

  private[contracts] def __reduce(obj: RawObject, ignoreBadTypes:Boolean):Available[RawObject] =
    obj.get(_key) match {
      case None =>
        NotFound
      case Some(raw) =>
        ValuePropertyLensOps.reduce(this, raw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case Found(reducedRaw) if reducedRaw != raw =>
            Found(obj + (_key -> reducedRaw))
          case Found(_) =>
            Found(obj)
          case NotFound =>
            NotFound
          case f:Failed =>
            f
        }
    }

  private[contracts] def __reduceDelta(deltaObject:RawObject, currentValue:RawObject, ignoreBadTypes:Boolean):ValidResult[RawObject] =
    deltaObject.get(_key) -> currentValue.get(_key) match {
      case (None, _) =>
        // We dont return failure here, even if currentValue doesnt have a representation for the property
        // Deltas dont need to repair, they just cant make things worse.
        Right(deltaObject)
      case (Some(raw), None) =>
        ValuePropertyLensOps.reduce(this, raw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case NotFound =>
            Right(deltaObject - _key)
          case Found(reducedRaw) if reducedRaw != raw =>
            Right(deltaObject + (_key -> reducedRaw))
          case Found(_) =>
            Right(deltaObject)
          case Failed(head, tail) =>
            Left(NonEmptyList(head, tail))
        }
      case (Some(deltaRaw), Some(currentRaw)) =>
        ValuePropertyLensOps.deltaReduce(this, deltaRaw, currentRaw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case DeltaReduced(reducedRaw) =>
            Right(deltaObject + (_key -> reducedRaw))
          case DeltaEmpty =>
            Right(deltaObject - _key)
          case DeltaRemove if deltaRaw != DNull =>
            Right(deltaObject + (_key -> DNull))
          case DeltaRemove =>
            Right(deltaObject)
          case DeltaRemoving(reducedRaw) =>
            Right(deltaObject + (_key -> reducedRaw))
          case DeltaFailed(head, tail) =>
            Left(NonEmptyList(head, tail))
        }
    }

  /**
   * Verifies the Type of the value for the Property if it exists in the object.
   * @param obj
   * @return
   */
  final def $verify(obj:D):List[StructuralFailure] =
    __get(obj, false) match {
      case Failed(head, tail) =>
        head :: tail
      case _ =>
        Nil
    }

  /**
   * Gets value for the property if found in passed object.
   * Returns failure if value is of unexpected type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType
   * @param obj
   * @return
   */
  final def $get(obj:D, ignoreBadTypes:Boolean = false):ValidResult[Option[T]] =
    __get(obj, ignoreBadTypes).toValidOption

  /**
   * Gets value for the property if found in passed object, otherwise returns default.
   * Returns failure if value is of unexpected type,
   * unless incorrectTypeBehaviour is set to EmptyOnIncorrectType
   * @param obj
   * @return
   */
  final def $getOrElse(obj:D, default: => T, ignoreBadTypes:Boolean = false):ValidResult[T] =
    __get(obj, ignoreBadTypes).toValidOption.map(_.getOrElse(default))

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
   * Returns failure if value is of the wrong type
   * @param f
   * @return
   */
  final def $modify(f:Option[T] => T, ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    TraversedModifySetter(__get(_, ignoreBadTypes), f, __set)

  /**
   * Modifies or sets the value if it doesnt exist.
   * Will create object path to Property if objects dont exist only if path is expected.
   * Returns failure if value is of the wrong type
   * @param f
   * @return
   */
  final def $modifyWith(f:Option[T] => ValidResult[T], ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    TraversedModifyValidSetter(__get(_, ignoreBadTypes), f, __set)

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
  final def $modifyOrDrop(f:Option[T] => Option[T], ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    TraversedModifyOrDropSetter[D, T](
      __get(_, ignoreBadTypes),
      f,
      (d, mt) => $setOrDrop(mt)(d)
    )

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
  final def $modifyOrDropWith(f:Option[T] => Option[ValidResult[T]], ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    TraversedModifyOrDropValidSetter[D, T](
      __get(_, ignoreBadTypes),
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
   * If will fail if the source property is of the wrong type
   * @param p
   * @return
   */
  final def $copy(p:PropertyLens[D, T], ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    TraversedModifyOrDropSetter(
      p.__get(_, ignoreBadTypes),
      identity[Option[T]],
      (d:D, mt:Option[T]) => $setOrDrop(mt)(d))

  //  final lazy val $delta:MaybeDelta[D, T] =
  //    new MaybeDelta[D, T](this)

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * Will return Some(None) on incorrect type
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[Option[T]] =
    Some(PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply))
}

private[dsentric] trait DefaultLens[D <: DObject, T] extends ValuePropertyLens[D, T] with ApplicativeLens[D, T]{

  def _default:T

  private def isIgnore2BadTypes(ignoreBadTypes:Boolean):BadTypes =
    if (ignoreBadTypes) DropBadTypes
    else FailOnBadTypes


  private[contracts] def __get(data:D, ignoreBadTypes:Boolean):Traversed[T] =
    TraversalOps.traverse(data.value, this, ignoreBadTypes).flatMap{raw =>
      ValuePropertyLensOps.get(this, raw, isIgnore2BadTypes(ignoreBadTypes)) match {
        case NotFound =>
          Found(_default)
        case t =>
          t
      }
    }

  //Same as Modify, cana tidy
  private[contracts] def __reduce(obj: RawObject, ignoreBadTypes:Boolean):Available[RawObject] =
    obj.get(_key) match {
      case None =>
        NotFound
      case Some(raw) =>
        ValuePropertyLensOps.reduce(this, raw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case Found(reducedRaw) if reducedRaw != raw =>
            Found(obj + (_key -> reducedRaw))
          case Found(_) =>
            Found(obj)
          case NotFound =>
            NotFound
          case f:Failed =>
            f
        }
    }

  //Same as Modify, cana tidy
  private[contracts] def __reduceDelta(deltaObject:RawObject, currentValue:RawObject, ignoreBadTypes:Boolean):ValidResult[RawObject] =
    deltaObject.get(_key) -> currentValue.get(_key) match {
      case (None, _) =>
        // We dont return failure here, even if currentValue doesnt have a representation for the property
        // Deltas dont need to repair, they just cant make things worse.
        Right(deltaObject)
      case (Some(raw), None) =>
        ValuePropertyLensOps.reduce(this, raw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case NotFound =>
            Right(deltaObject - _key)
          case Found(reducedRaw) if reducedRaw != raw =>
            Right(deltaObject + (_key -> reducedRaw))
          case Found(_) =>
            Right(deltaObject)
          case Failed(head, tail) =>
            Left(NonEmptyList(head, tail))
        }
      case (Some(deltaRaw), Some(currentRaw)) =>
        ValuePropertyLensOps.deltaReduce(this, deltaRaw, currentRaw, isIgnore2BadTypes(ignoreBadTypes)) match {
          case DeltaReduced(reducedRaw) =>
            Right(deltaObject + (_key -> reducedRaw))
          case DeltaEmpty =>
            Right(deltaObject - _key)
          case DeltaRemove if deltaRaw != DNull =>
            Right(deltaObject + (_key -> DNull))
          case DeltaRemove =>
            Right(deltaObject)
          case DeltaRemoving(reducedRaw) =>
            Right(deltaObject + (_key -> reducedRaw))
          case DeltaFailed(head, tail) =>
            Left(NonEmptyList(head, tail))
        }
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
  final def $get(obj:D, ignoreBadTypes:Boolean = false):ValidResult[Option[T]] =
    __get(obj, ignoreBadTypes).toValidOption

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
    __get(obj, false) match {
      case Failed(head, tail) =>
        head :: tail
      case _ =>
        Nil
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
  final def $modify(f:T => T, ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    ModifySetter($get(_, ignoreBadTypes), f, __set)

  final def $modifyWith(f:T => ValidResult[T], ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    ModifyValidSetter($get(_, ignoreBadTypes), f, __set)

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
  final def $copy(p:PropertyLens[D, T], ignoreBadTypes:Boolean = false):ValidPathSetter[D] =
    TraversedModifyOrDropSetter[D, T](p.__get(_, ignoreBadTypes), identity[Option[T]], (d, mt) => $setOrRestore(mt)(d))

  //  final lazy val $delta:MaybeDelta[D, T] =
  //    new MaybeDelta[D, T](this)

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * Returns Some(default) if not found or type is incorrect
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[T] =
    Some(PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply).getOrElse(_default))
}


object ValuePropertyLensOps extends GetOps with ReduceOps with DeltaReduceOps {
  def get[D <: DObject, T](propertyLens: PropertyLens[D, T], raw:Raw, badTypes: BadTypes):Available[T] =
    getCodec(propertyLens._root, propertyLens._path, badTypes)(propertyLens._codec -> raw)

  def reduce[D <: DObject, T](propertyLens: PropertyLens[D, T], raw:Raw, badTypes: BadTypes):Available[Raw] =
    reduceCodec(propertyLens._root, propertyLens._path, badTypes)(propertyLens._codec -> raw)

  def deltaReduce[D <: DObject, T](propertyLens: PropertyLens[D, T], delta:Raw, current:Raw, badTypes: BadTypes):DeltaReduce[Raw] = {

    deltaReduceCodec(propertyLens._root, propertyLens._path, badTypes)((propertyLens._codec, delta, current))
  }
}

/**
 * When ignoring bad types we only want it to ignore on nested entities.
 * The reason being we want to return immediate bad type information to Expected properties so that they return the
 * type failure as opposed to the Not Found failure
 */
private sealed trait BadTypes {
  def nest:BadTypes
}
private case object FailOnBadTypes extends BadTypes {
  def nest: BadTypes = this
}
private case object ToDropBadTypes extends BadTypes {
  def nest: BadTypes = DropBadTypes
}
private case object DropBadTypes extends BadTypes {
  def nest: BadTypes = this
}

/**
 * Validates and returns the element, bad values can be removed or generate failures.
 * Empty objects or Null values are left
 */
trait GetOps {
  protected def getCodec[D <: DObject, C](contract:ContractFor[D], path:Path, badTypes:BadTypes):Function[(DCodec[C], Raw), Available[C]] = {
    case (d:DValueCodec[C], raw) =>
      getValue(contract, path, badTypes, d, raw)
    case (d:DMapCodec[C, _, _], rawObject:RawObject) =>
      getMap(contract, path, badTypes, d, rawObject)
    case (d:DCollectionCodec[C, _], rawArray: RawArray) =>
      getCollection(contract, path, badTypes, d, rawArray)
    case (DContractCodec(codecContract), rawObject:RawObject) =>
      getContract(contract, path, badTypes, codecContract, rawObject)
        .asInstanceOf[Available[C]]
    case _ if badTypes == DropBadTypes =>
      NotFound
    case (d, raw) =>
      Failed(IncorrectTypeFailure(contract, path, d, raw))
  }

  protected def getValue[D <: DObject, V](contract:ContractFor[D], path:Path, badTypes:BadTypes, codec:DValueCodec[V], raw:Raw):Available[V] =
    codec.unapply(raw) match {
      case None if badTypes == DropBadTypes =>
        NotFound
      case None =>
        Failed(IncorrectTypeFailure(contract, path, codec, raw))
      case Some(v) =>
        Found(v)
    }

  protected def getMap[D <: DObject, C, K, V](contract:ContractFor[D], path:Path, badTypes:BadTypes, codec:DMapCodec[C, K, V], raw:RawObject):Available[C] = {
    val nested = badTypes.nest
    raw.view.map { p =>
      val key =
        codec.keyCodec.unapply(p._1) match {
          case None if nested == DropBadTypes =>
            NotFound
          case None =>
            Failed(IncorrectKeyTypeFailure(contract, path, codec.keyCodec, p._1))
          case Some(key) =>
            Found(key)
        }
      val value = getCodec(contract, path \ p._1, nested)(codec.valueCodec -> p._2)
      Available.sequence2(key, value)
    }.foldLeft[Either[ListBuffer[StructuralFailure], mutable.Builder[(K, V), Map[K, V]]]](Right(Map.newBuilder[K, V])) {
      case (Right(mb), Found(pair)) =>
        Right(mb.addOne(pair))
      case (Right(_), Failed(head, tail)) =>
        Left(new ListBuffer[StructuralFailure].addAll(head :: tail))
      case (Left(lb), Failed(head, tail)) =>
        Left(lb.addAll(head :: tail))
      case (result, _) =>
        result
    } match {
      case Right(mb) =>
        Found(codec.build(mb.result()))
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(lb) =>
        val head :: tail = lb.result()
        Failed(head, tail)
    }
  }

  protected def getCollection[D <: DObject, S, T](contract:ContractFor[D], path:Path, badTypes:BadTypes, codec:DCollectionCodec[S, T], raw:RawArray):Available[S] = {
    val nested = badTypes.nest
    raw.zipWithIndex.map { p =>
      getCodec(contract, path \ p._2, nested)(codec.valueCodec -> p._1) -> p._1
    }.foldLeft[Either[ListBuffer[StructuralFailure], mutable.Builder[T, Vector[T]]]](Right(Vector.newBuilder[T])){
      case (Right(vb), (Found(t), _)) =>
        Right(vb.addOne(t))
      case (Right(_), (Failed(head, tail), _)) =>
        Left(new ListBuffer[StructuralFailure].addAll(head :: tail))
      case (Left(lb), (Failed(head, tail), _)) =>
        Left(lb.addAll(head :: tail))
      case (Right(_), (NotFound, index)) =>
        Left(new ListBuffer[StructuralFailure].addOne(MissingElementFailure(contract, codec, path \ index)))
      case (Left(lb), (NotFound, index)) =>
        Left(lb.addOne(MissingElementFailure(contract, codec, path \ index)))
      case (result, _) =>
        result
    } match {
      case Right(vb) =>
        Found(codec.build(vb.result()))
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(lb) =>
        val head :: tail = lb.result()
        Failed(head, tail)
    }
  }

  protected def getContract[D <: DObject](contract:ContractFor[D], path:Path, badTypes:BadTypes, codecContract:Contract, raw:RawObject):Available[DObject] =
    codecContract.$get(new DObjectInst(raw), badTypes.nest == DropBadTypes) match {
      case Right(d) =>
        Found(d)
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(NonEmptyList(head, tail)) =>
        Failed(head, tail).rebase(contract, path)
    }

}

/**
 * Verifies the structure of the document and removes any Empty objects or Null values.
 * Bad types can be dropped or return failures
 */
trait ReduceOps {

  protected def reduceCodec[D <: DObject, C](contract:ContractFor[D], path:Path, badTypes:BadTypes):Function[(DCodec[C], Raw), Available[Raw]] = {
    case (_, DNull) =>
      NotFound
    case (_, rawObject:RawObject)  if rawObject.isEmpty =>
      NotFound
    case (d:DValueCodec[C], raw) =>
      reduceValue(contract, path, badTypes, d, raw)
    case (d:DMapCodec[C, _, _], rawObject:RawObject) =>
      reduceMap(contract, path, badTypes, d, rawObject)
    case (d:DCollectionCodec[C, _], rawArray: RawArray) =>
      reduceCollection(contract, path, badTypes, d, rawArray)
    case (DContractCodec(codecContract), rawObject:RawObject) =>
      reduceContract(contract, path, badTypes, codecContract, rawObject)
    case _ if badTypes == DropBadTypes =>
      NotFound
    case (d, raw) =>
      Failed(IncorrectTypeFailure(contract, path, d, raw))
  }

  protected def reduceValue[D <: DObject, V](contract:ContractFor[D], path:Path, badTypes:BadTypes, codec:DValueCodec[V], raw:Raw):Available[Raw] =
    raw match {
      case rawObject: RawObject =>
        RawObjectOps.reduceMap(rawObject).fold[Available[Raw]](NotFound) { r =>
          codec.unapply(r) match {
            case None if badTypes == DropBadTypes =>
              NotFound
            case None =>
              Failed(IncorrectTypeFailure(contract, path, codec, raw))
            case Some(_) =>
              Found(raw)
          }
        }
      case r =>
        codec.unapply(r) match {
        case None if badTypes == DropBadTypes =>
          NotFound
        case None =>
          Failed(IncorrectTypeFailure(contract, path, codec, raw))
        case Some(_) =>
          Found(raw)
      }
    }

  protected def reduceMap[D <: DObject, C, K, V](contract:ContractFor[D], path:Path, badTypes:BadTypes, codec:DMapCodec[C, K, V], raw:RawObject):Available[RawObject] = {
    val nest = badTypes.nest
    raw.view.map { p =>
      val key =
        codec.keyCodec.unapply(p._1) match {
          case None if nest == DropBadTypes =>
            NotFound
          case None =>
            Failed(IncorrectKeyTypeFailure(contract, path, codec.keyCodec, p._1))
          case Some(_) =>
            Found(p._1)
        }
      val value = reduceCodec(contract, path \ p._1, nest)(codec.valueCodec -> p._2)
      Available.sequence2(key, value)
    }.foldLeft[Either[ListBuffer[StructuralFailure], mutable.Builder[(String, Raw), Map[String, Raw]]]](Right(Map.newBuilder[String, Raw])) {
      case (Right(mb), Found(pair)) =>
        Right(mb.addOne(pair))
      case (Right(_), Failed(head, tail)) =>
        Left(new ListBuffer[StructuralFailure].addAll(head :: tail))
      case (Left(lb), Failed(head, tail)) =>
        Left(lb.addAll(head :: tail))
      case (result, _) =>
        result
    } match {
      case Right(mb) =>
        val map = mb.result()
        if (map.isEmpty)
          NotFound
        else
          Found(mb.result())
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(lb) =>
        val head :: tail = lb.result()
        Failed(head, tail)
    }
  }

  protected def reduceCollection[D <: DObject, S, T](contract:ContractFor[D], path:Path, badTypes:BadTypes, codec:DCollectionCodec[S, T], raw:RawArray):Available[RawArray] = {
    val nest = badTypes.nest
    raw.zipWithIndex.map { p =>
      reduceCodec(contract, path \ p._2, nest)(codec.valueCodec -> p._1) -> p._1
    }.foldLeft[Either[ListBuffer[StructuralFailure], mutable.Builder[Raw, Vector[Raw]]]](Right(Vector.newBuilder[Raw])){
      case (Right(vb), (Found(t), _)) =>
        Right(vb.addOne(t))
      case (Right(_), (Failed(head, tail), _)) =>
        Left(new ListBuffer[StructuralFailure].addAll(head :: tail))
      case (Left(lb), (Failed(head, tail), _)) =>
        Left(lb.addAll(head :: tail))
      case (Right(_), (NotFound, index)) =>
        Left(new ListBuffer[StructuralFailure].addOne(MissingElementFailure(contract, codec, path \ index)))
      case (Left(lb), (NotFound, index)) =>
        Left(lb.addOne(MissingElementFailure(contract, codec, path \ index)))
      case (result, _) =>
        result
    } match {
      case Right(vb) =>
        Found(vb.result())
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(lb) =>
        val head :: tail = lb.result()
        Failed(head, tail)
    }
  }

  protected def reduceContract[D <: DObject](contract:ContractFor[D], path:Path, badTypes:BadTypes, codecContract:Contract, raw:RawObject):Available[RawObject] =
    codecContract.$reduce(new DObjectInst(raw), badTypes.nest == DropBadTypes) match {
      case Right(d) =>
        Found(d.value)
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(NonEmptyList(head, tail)) =>
        Failed(head, tail).rebase(contract, path)
    }
}

/**
 * Reduces deltas that do no change, or are effectively empty
 * (An empty object will not replace or remove a Value)
 */
trait DeltaReduceOps extends ReduceOps {

  private def available2DeltaReduce[T]:Function[Available[T],DeltaReduce[T]] = {
    case Found(t) => DeltaReduced(t)
    case NotFound => DeltaEmpty
    case Failed(head, tail) => DeltaFailed(head, tail)
  }


  protected def deltaReduceCodec[D <: DObject, C](contract:ContractFor[D], path:Path, badTypes:BadTypes):Function[(DCodec[C], Raw, Raw), DeltaReduce[Raw]] = {
    case (_, DNull, _) =>
      DeltaRemove
    case (d:DValueCodec[C], delta, current) =>
      deltaReduceValue(contract, path, badTypes, d, delta, current)
    case (d:DMapCodec[C, _, _], deltaObject:RawObject, current:Raw) =>
      deltaReduceMap(contract, path, badTypes, d, deltaObject, current)
    case (d:DCollectionCodec[C, _], deltaArray: RawArray, _) =>
      available2DeltaReduce(reduceCollection(contract, path, badTypes, d, deltaArray))
    case (DContractCodec(codecContract), rawObject:RawObject, current) =>
      deltaReduceContract(contract, path, badTypes, codecContract, rawObject, current)
    case _ if badTypes == DropBadTypes =>
      DeltaEmpty
    case (d, raw, _) =>
      DeltaFailed(IncorrectTypeFailure(contract, path, d, raw))
  }


  protected def deltaReduceValue[D <: DObject, V](contract:ContractFor[D], path:Path, badTypes:BadTypes, codec:DValueCodec[V], delta:Raw, current:Raw):DeltaReduce[Raw] =
    (delta, current) match {
      case (deltaObject:RawObject, currentObject:RawObject) =>
        RawObjectOps.rightDifferenceReduceMap(currentObject -> deltaObject) match {
          case None =>
            DeltaEmpty
          case Some(reducedDelta) =>
            val newState = RawObjectOps.rightReduceConcatMap(currentObject, reducedDelta)
            if (newState.isEmpty)
              DeltaRemoving(reducedDelta)
            else
              codec.unapply(newState) match {
                case None if badTypes == DropBadTypes =>
                  DeltaEmpty
                case None =>
                  DeltaFailed(IncorrectTypeFailure(contract, path, codec, reducedDelta))
                case Some(_) =>
                  DeltaReduced(reducedDelta)
              }
        }
      case (deltaRaw:Raw, _) =>
        available2DeltaReduce(reduceValue(contract, path, badTypes, codec, deltaRaw))
    }

  protected def deltaReduceMap[D <: DObject, C, K, V](contract:ContractFor[D], path:Path, badTypes:BadTypes, codec:DMapCodec[C, K, V], deltaObject:RawObject, current:Raw):DeltaReduce[RawObject] =
    current match {
      case currentObject: RawObject =>
        val nest = badTypes.nest
        deltaObject.map{
          case (key, DNull) if (currentObject.contains(key)) =>
            key -> DeltaRemove
          case (key, DNull)  =>
            key -> DeltaEmpty
          case (key, deltaValue) =>
            codec.keyCodec.unapply(key) -> currentObject.get(key) match {
              case (None, _) if nest == DropBadTypes =>
                key -> NotFound
              case (None, None) =>
                val failure = IncorrectKeyTypeFailure(contract, path, codec.keyCodec, key)
                reduceCodec(contract, path \ key, nest)(codec.valueCodec -> deltaValue) match {
                  case Failed(head, tail) =>
                    key -> DeltaFailed(failure, head :: tail)
                  case _ =>
                    key -> DeltaFailed(failure)
                }
              case (None, Some(currentValue)) =>
                val failure = IncorrectKeyTypeFailure(contract, path, codec.keyCodec, key)
                deltaReduceCodec(contract, path \ key, nest)((codec.valueCodec, deltaValue, currentValue)) match {
                  case DeltaFailed(head, tail) =>
                    key -> DeltaFailed(failure, head :: tail)
                  case DeltaEmpty =>
                    key -> DeltaEmpty
                  case _ =>
                    key -> DeltaFailed(failure)
                }
              case (Some(_), None) =>
                key -> available2DeltaReduce(reduceCodec(contract, path \ key, nest)(codec.valueCodec -> deltaValue))
              case (Some(_), Some(currentValue)) =>
                key -> deltaReduceCodec(contract, path \ key, nest)((codec.valueCodec, deltaValue, currentValue))
            }
          }.foldLeft[Either[ListBuffer[Failure], mutable.Builder[(String, Raw), Map[String, Raw]]]](Right(Map.newBuilder[String, Raw])){
            case (Right(mb), (key, DeltaReduced(d))) =>
              Right(mb.addOne(key -> d))
            case (Right(mb), (key, DeltaRemoving(d))) =>
              Right(mb.addOne(key -> d))
            case (Right(mb), (key, DeltaRemove)) =>
              Right(mb.addOne(key -> DNull))
            case (Right(_), (_, DeltaFailed(head, tail))) =>
              Left(new ListBuffer[Failure].addAll(head :: tail))
            case (Left(lb), (_, DeltaFailed(head, tail))) =>
              Left(lb.addAll(head :: tail))
            case (result, _) =>
              result
          } match {
          case Left(_) if badTypes == DropBadTypes =>
            DeltaEmpty
          case Left(lb) =>
            DeltaFailed(lb.head, lb.tail.result())
          case Right(mb) =>
            val map = mb.result()
            if (map.isEmpty) DeltaEmpty
            else if (RawObjectOps.rightReduceConcatMap(currentObject, map).isEmpty) //TODO improve performance
              DeltaRemoving(map)
            else
              DeltaReduced(map)
        }
      case _ =>
        available2DeltaReduce(reduceMap(contract, path, badTypes, codec, deltaObject))
    }

  protected def deltaReduceContract[D <: DObject](contract:ContractFor[D], path:Path, badTypes:BadTypes, codecContract:Contract, deltaObject:RawObject, current:Raw):DeltaReduce[RawObject] =
    current match {
      case currentObject: RawObject =>
        codecContract.$reduceDelta(new DObjectInst(currentObject), new DeltaInst(deltaObject), badTypes.nest == DropBadTypes) match {
          case Right(d) if d.isEmpty =>
            DeltaEmpty
          case Right(d) =>
            if (RawObjectOps.rightReduceConcatMap(currentObject, d.value).isEmpty)
              DeltaRemoving(d.value)
            else
              DeltaReduced(d.value)
          case Left(_) if badTypes == DropBadTypes =>
            DeltaEmpty
          case Left(NonEmptyList(head, tail)) =>
            DeltaFailed(head, tail)
        }
      case _ =>
        available2DeltaReduce(reduceContract(contract, path, badTypes, codecContract, deltaObject))
    }
}