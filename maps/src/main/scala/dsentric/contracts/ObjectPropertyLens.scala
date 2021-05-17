package dsentric.contracts

import dsentric._
import cats.data._
import dsentric.codecs.DCodec
import dsentric.codecs.std.DCodecs
import dsentric.failure.{ClosedContractFailure, StructuralFailure, ValidResult, ValidStructural}

private[dsentric] sealed trait ObjectPropertyLens[D <: DObject]
  extends BaseContract[D] with PropertyLens[D, DObject]{

  def _codec: DCodec[DObject]

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
  def $verify(obj:D):List[StructuralFailure]

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
    __get(obj, dropBadTypes).toValidOption
  /**
   * Sets the object content to the passed value.
   * Does nothing if None is passed.
   * Verifies the object satisfies the property requirements
   * and additional properties definition.
   * @param obj
   * @return
   */
  final def $maybeSet(obj:Option[DObject]):ValidPathSetter[D] =
    obj.fold[ValidPathSetter[D]](IdentityValidSetter[D]())($set)
}

/**
 * An Expected object doesnt necessarily have to be present if none of its properties
 * are expected.
 * @tparam D
 */
private[dsentric] trait ExpectedObjectPropertyLens[D <: DObject] extends ObjectPropertyLens[D]{

  private def isIgnore2BadTypes(dropBadTypes:Boolean):BadTypes =
    if (dropBadTypes) DropBadTypes
    else FailOnBadTypes

  private[contracts] def __get(data:D, dropBadTypes:Boolean):Traversed[DObject] = {
    def reduce(rawObject:Map[String, Any]):Traversed[DObject] =
      ObjectPropertyLensOps.reduce(this, rawObject, isIgnore2BadTypes(dropBadTypes)) match {
        case Right(rawObject) =>
          //its possible this will return an empty object.
          Found(new DObjectInst(rawObject))
        case Left(NonEmptyList(head, tail)) =>
          Failed(head, tail)
      }

    TraversalOps.traverse(data.value, this, dropBadTypes) match {
      case NotFound =>
        reduce(RawObject.empty)
      case Found(rawObject:RawObject@unchecked) =>
        reduce(rawObject)
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
  final def unapply(obj:D):Option[DObject] =
    PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply)


  private[contracts] def __reduce(obj: RawObject, dropBadTypes:Boolean):ValidStructural[RawObject] =
    TraversalOps.propertyValue(obj, this) match {
      case NotFound =>
        ObjectLens.propertyVerifier(this, Map.empty) match {
          case head :: tail => Left(NonEmptyList(head, tail))
          case _ => Right(obj)
        }
      case Failed(f, tail) =>
        Left(NonEmptyList(f, tail))
      case Found(v) =>
        ObjectLens.verifyReduce(this, v.value)
          .map{propertyObject =>
            if (propertyObject == v) obj
            else if (propertyObject.isEmpty) obj - _key
            else obj + (_key -> propertyObject)
          }
    }


  /**
   * Verifies the property against the delta property value.
   * Is not responsible for traversal.
   *
   * @param deltaValue
   * @param currentValue
   * @return
   */
  private[contracts] def __verifyAndReduceDelta(deltaValue: Raw, currentValue: Option[Raw]): DeltaReduce[RawObject] =
    _codec.verifyAndReduceDelta(deltaValue, currentValue) -> currentValue match {
      case (DeltaRemove, _) =>
        ObjectLens.propertyVerifier(this, Map.empty) match {
          case Nil => DeltaRemove
          case head :: tail => DeltaFailed(head, tail)
        }
      case (DeltaReduced(r), Some(c:RawObject)) =>

      case (DeltaReduced(r), _) =>
        ObjectLens.propertyVerifier(this, r) match {
          case head :: tail =>
            DeltaFailed(head, tail)
          case Nil =>
            DeltaReduced(r)
        }
      case (d, _) =>
        d
    }

  /**
   * Verifies the structure of the object against its properties
   * and additional property definition returning a list of possibly failures.
   *
   * @param obj
   * @return
   */
  final def $verify(obj:  D): List[StructuralFailure] =
    TraversalOps.traverse(obj.value, this) match {
      case PathEmptyMaybe =>
        Nil
      case NotFound =>
        ObjectLens.propertyVerifier(this, Map.empty)
      case Failed(f, tail) =>
        f :: tail
      case Found(v) =>
        ObjectLens.propertyVerifier(this, v.value)
    }
}

/**
 * Object lens for a Property which contains an object or could be empty.
 * @tparam D
 */
private[dsentric] trait MaybeObjectPropertyLens[D <: DObject] extends ObjectPropertyLens[D] {

  final def unapply(obj:D):Option[Option[DObject]] =
    TraversalOps.traverse(obj.value, this, false) match {
      case PathEmptyMaybe => Some(None)
      case NotFound => Some(None)
      case Found(t) => Some(Some(t))
      case Failed(_, _) => None
    }

  /**
   * Verifies the direct property against the object.
   *
   * @param obj
   * @return
   */
  private[contracts] def __verifyTraversal(obj: RawObject): List[StructuralFailure] =
    TraversalOps.propertyValue(obj, this) match {
      case NotFound => Nil
      case Failed(f, tail) => f :: tail
      case Found(v) => ObjectLens.propertyVerifier(this, v.value)
    }

  private[contracts] def __verifyReduce(obj: RawObject): Either[NonEmptyList[StructuralFailure], RawObject] =
    TraversalOps.propertyValue(obj, this) match {
      case NotFound =>
        Right(obj)
      case Failed(f, tail) =>
        Left(NonEmptyList(f, tail))
      case Found(v) =>
        ObjectLens.verifyReduce(this, v.value)
          .map{propertyObject =>
            if (propertyObject == v) obj
            else if (propertyObject.isEmpty) obj - _key
            else obj + (_key -> propertyObject)
          }
    }

  /**
   * Verifies the property against the delta property value.
   * Is not responsible for traversal.
   *
   * @param deltaValue
   * @param currentValue
   * @return
   */
  private[contracts] def __verifyAndReduceDelta(deltaValue: Raw, currentValue: Option[Raw]): DeltaReduce[RawObject] =
    _codec.verifyAndReduceDelta(deltaValue, currentValue) -> currentValue match {

      case (DeltaReduced(r), Some(c:RawObject)) =>

      case (DeltaReduced(r), _) =>
        ObjectLens.propertyVerifier(this, r) match {
          case head :: tail =>
            DeltaFailed(head, tail)
          case Nil =>
            DeltaReduced(r)
        }
      case (d, _) =>
        d
    }

  private[dsentric] def __get(obj: D): Traversed[DObject] =
    TraversalOps.traverse(obj.value, this)
      .flatMap { v =>
        ObjectLens.propertyVerifier(this, v.value) match {
          case head :: tail => Failed(head, tail)
          case Nil => Found(v)
        }
      }

  /**
   * Verifies the structure of the object against its properties
   * and additional property definition returning a list of possibly failures.
   *
   * @param obj
   * @return
   */
  final def $verify(obj: D): List[StructuralFailure] =
    TraversalOps.traverse(obj.value, this) match {
      case PathEmptyMaybe =>
        Nil
      case NotFound =>
        Nil
      case Failed(f, tail) =>
        f :: tail
      case Found(v) =>
        ObjectLens.propertyVerifier(this, v.value)
    }

}

private[dsentric] object ObjectPropertyLensOps extends ReduceOps {


  /**
   * Reduces empty property fields, removing DCodec values that return NotFound
   * Ultimately clearing out empty Objects as well
   * Will also remove any nulls
   * */
  def reduce[D <: DObject](baseContract:BaseContract[D], obj:RawObject, badTypes:BadTypes):ValidStructural[RawObject] = {

    val init = reduceAdditionalProperties(baseContract, obj, badTypes)
    val drop = badTypes.nest == DropBadTypes
    baseContract._fields.foldLeft(init){
      case (Right(d), (_, p)) =>
        p.__reduce(d, drop)
      case (l@Left(nel), (_, p)) =>
        p.__reduce(obj, drop) match {
          case Right(_) =>
            l
          case Left(nel2) =>
            Left(nel ::: nel2)
        }
    }
  }

  private def reduceAdditionalProperties[D <: DObject](
                                                      baseContract:BaseContract[D],
                                                      obj:RawObject,
                                                      badTypes:BadTypes
                                                    ):ValidStructural[RawObject] = {
    val exclude = baseContract._fields.keySet
    baseContract match {
      case a:AdditionalProperties[Any, Any]@unchecked =>
        val excludedObject = obj -- exclude
        reduceMap(a._root, a._path, badTypes, DCodecs.keyValueMapCodec(a._additionalKeyCodec, a._additionalValueCodec), excludedObject) match {
          case Found(rawObject) if rawObject == excludedObject =>
            Right(obj)
          case Found(rawObject) =>
            Right(obj -- exclude ++ rawObject)
          case NotFound =>
            Right(excludedObject)
          case Failed(head, tail) =>
            Left(NonEmptyList(head, tail))
        }
      case _ =>
        obj.keys
          .filterNot(exclude)
          .map(k => ClosedContractFailure(baseContract._root, baseContract._path, k))
          .toList match {
          case head :: tail =>
            Left(NonEmptyList(head, tail))
          case Nil =>
            Right(obj)
        }

    }
  }
}


