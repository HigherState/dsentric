package dsentric.contracts

import dsentric._
import dsentric.failure.{NotFound, _}
import cats.data._

private[dsentric] sealed trait ObjectLens[D <: DObject]
  extends BaseContract[D] with PropertyLens[D, DObject]{

  /**
   * Applies the direct property value to the object.
   * Will traverse all object properties and apply values
   * @param obj
   * @return
   */
  private[contracts] def __applyTraversal(obj: RawObject):Either[NonEmptyList[StructuralFailure], RawObject]
  /**
   * Verifies the direct property against the object.
   * @param obj
   * @return
   */
  private[contracts] def __verifyTraversal(obj: RawObject): List[StructuralFailure]

  /**
   * Verifies the direct property against the object.
   * Will remove from object if empty
   * @param obj
   * @return
   */
  private[contracts] def __verifyReduce(obj: RawObject):Either[NonEmptyList[StructuralFailure], RawObject]


  private[dsentric] def __get(obj:D):Traversed[DObject]

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
   * Sets the object content to the passed value.
   * Verifies the object satisfies the property requirements
   * and additional properties definition.
   * Will drop value if resulting object is empty
   * TypeBehaviour is always to fail, cannot set bad data.
   * @param obj
   * @return
   */
  final def $set(obj:DObject):ValidPathSetter[D] =
    ValidObjectSetter(this._path, ObjectLens.verifyReduce(this, obj.value))

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
  final def $get(obj:D):ValidResult[Option[DObject]] =
    __get(obj).toValidOption
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
private[dsentric] trait ExpectedObjectLens[D <: DObject] extends ObjectLens[D]{

  /**
   * Unapply is only ever a simple prism to the value and its decoding
   * @param obj
   * @return
   */
  final def unapply(obj:D):Option[DObject] =
    PathLensOps.traverse(obj.value, _path).flatMap(_codec.unapply)

  /**
   * Applies the direct property value to the object.
   * Will traverse all object properties and apply values
   *
   * @param obj
   * @return
   */
  private[contracts] def __applyTraversal(obj: RawObject):Either[NonEmptyList[StructuralFailure], RawObject] =
    __incorrectTypeBehaviour.property(obj, this) match {
      case NotFound => ObjectLens.propertyApplicator(this, Map.empty)
      case Failed(f, tail) => Left(NonEmptyList(f, tail))
      case Found(v) => ObjectLens.propertyApplicator(this, v.value)
    }

  /**
   * Verifies the direct property against the object.
   *
   * @param obj
   * @return
   */
  private[contracts] def __verifyTraversal(obj:  RawObject): List[StructuralFailure] =
    FailOnIncorrectTypeBehaviour.property(obj, this) match {
      case NotFound => ObjectLens.propertyVerifier(this, Map.empty)
      case Failed(f, tail) => f :: tail
      case Found(v) => ObjectLens.propertyVerifier(this, v.value)
    }

  private[contracts] def __verifyReduce(obj: RawObject): Either[NonEmptyList[StructuralFailure], RawObject] =
    FailOnIncorrectTypeBehaviour.property(obj, this) match {
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

  private[dsentric] def __get(obj: D): Traversed[DObject] =
    __incorrectTypeBehaviour.traverse(obj.value, this) match {
      case NotFound =>
        ObjectLens.propertyApplicator(this, Map.empty) match {
          case Left(NonEmptyList(head, tail)) => Failed(head, tail)
          case Right(rawObject) => Found(new DObjectInst(rawObject))
        }
      case Found(v) =>
        ObjectLens.propertyApplicator(this, v.value) match {
          case Left(NonEmptyList(head, tail)) => Failed(head, tail)
          case Right(rawObject) => Found(v.internalWrap(rawObject))
        }
      case t => t
    }

  /**
   * Verifies the structure of the object against its properties
   * and additional property definition returning a list of possibly failures.
   *
   * @param obj
   * @return
   */
  final def $verify(obj:  D): List[StructuralFailure] =
    FailOnIncorrectTypeBehaviour.traverse(obj.value, this) match {
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
private[dsentric] trait MaybeObjectLens[D <: DObject] extends ObjectLens[D] {

  final def unapply(obj:D):Option[Option[DObject]] =
    __incorrectTypeBehaviour.traverse(obj.value, this) match {
      case PathEmptyMaybe => Some(None)
      case NotFound => Some(None)
      case Found(t) => Some(Some(t))
      case Failed(_, _) => None
    }

  /**
   * Applies the direct property value to the object.
   * Will traverse all object properties and apply values
   *
   * @param obj
   * @return
   */
  private[contracts] def __applyTraversal(obj: RawObject):Either[NonEmptyList[StructuralFailure], RawObject] =
    __incorrectTypeBehaviour.property(obj, this) match {
      case NotFound => Right(obj)
      case Failed(f, tail) => Left(NonEmptyList(f, tail))
      case Found(v) => ObjectLens.propertyApplicator(this, v.value)
    }

  /**
   * Verifies the direct property against the object.
   *
   * @param obj
   * @return
   */
  private[contracts] def __verifyTraversal(obj: RawObject): List[StructuralFailure] =
    FailOnIncorrectTypeBehaviour.property(obj, this) match {
      case NotFound => Nil
      case Failed(f, tail) => f :: tail
      case Found(v) => ObjectLens.propertyVerifier(this, v.value)
    }

  private[contracts] def __verifyReduce(obj: RawObject): Either[NonEmptyList[StructuralFailure], RawObject] =
    FailOnIncorrectTypeBehaviour.property(obj, this) match {
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

  private[dsentric] def __get(obj: D): Traversed[DObject] =
    __incorrectTypeBehaviour.traverse(obj.value, this) match {
      case Found(v) =>
        ObjectLens.propertyApplicator(this, v.value) match {
          case Left(NonEmptyList(head, tail)) => Failed(head, tail)
          case Right(rawObject) => Found(v.internalWrap(rawObject))
        }
      case t => t
    }

  /**
   * Verifies the structure of the object against its properties
   * and additional property definition returning a list of possibly failures.
   *
   * @param obj
   * @return
   */
  final def $verify(obj: D): List[StructuralFailure] =
    FailOnIncorrectTypeBehaviour.traverse(obj.value, this) match {
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

//Codec implies it doesnt actually have to be an array as raw type
//Any modifications setting empty vector should clear the field
private[dsentric] trait ObjectsLens[D <: DObject, T <: DObject] extends PropertyLens[D, Vector[T]] {
  def _contract:ContractFor[T]
  def _valueCodec:DObjectCodec[T]
  val _codec:DArrayCodec[T, Vector[T]] =
    PessimisticCodecs.vectorCodec(_valueCodec)

  private def __getRaw(obj:D):ValidResult[RawArray] =
    PathLensOps.traverse(obj.value, _path).fold[ValidResult[RawArray]](Right(RawArray.empty)){
      case r:RawArray@unchecked => Right(r)
      case _ if __incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour =>
        Right(RawArray.empty)
      case v =>
        ValidResult.failure(IncorrectTypeFailure(this, v))
    }

  private[contracts] def __get(obj: D): Traversed[Vector[T]] = ???
//    __getRaw(obj)
//    .flatMap{ array =>
//      if (__incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour)
//        ValidResult.success{
//          Some {
//            array.flatMap{e =>
//              val a = _valueCodec.unapply(e)
//              a.flatMap(t => _contract.$get(t).toOption)
//
//            }
//          }
//        }
//      else {
//        val validResults: Vector[ValidResult[T]] =
//          array.zipWithIndex
//            .map { p =>
//              _valueCodec.unapply(p._1)
//                .fold[ValidResult[T]](ValidResult.failure(IncorrectTypeFailure(_contract, Path.empty, _valueCodec, p._1))) { t =>
//                  _contract.$get(t)
//                }
//                .left
//                .map(_.map(_.rebase(_root, _path \ p._2)))
//            }
//        ValidResult.parSequence(validResults).map(Some.apply)
//      }
//    }

  private[contracts] def __applyTraversal(obj: RawObject):Either[NonEmptyList[StructuralFailure], RawObject] = ???

  private[contracts] def __verifyTraversal(obj:  RawObject): List[StructuralFailure] = ???

  private[contracts] def __map(obj: D, f:T => T): ValidResult[Option[RawArray]] = {
    __getRaw(obj)
      .flatMap{ array =>
        if (array.isEmpty) ValidResult.success(None)
        else if (__incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour)
          ValidResult.success{
            Some {
              array.flatMap{e =>
                _valueCodec.unapply(e)
                  .flatMap(t => _contract.$get(t).toOption)
                  .map(t => _valueCodec(f(t)).value)
              }
            }
          }
        else {
          val validResults: Vector[ValidResult[Raw]] =
            array.zipWithIndex
              .map { p =>
                _valueCodec.unapply(p._1)
                  .fold[ValidResult[Raw]](ValidResult.failure(IncorrectTypeFailure(_contract, Path.empty, _valueCodec, p._1))) { t =>
                    _contract.$get(t).map(t => _valueCodec(f(t)).value)
                  }
                  .left
                  .map(_.map(_.rebase(_root, _path \ p._2)))
              }
          ValidResult.parSequence(validResults).map(Some.apply)
        }
      }
  }

  private[contracts] def __map(obj: D, f:ValidPathSetter[T]): ValidResult[Option[RawArray]] = {
    val isIgnore = __incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour
    __getRaw(obj)
      .flatMap{ array =>
        if (array.isEmpty) ValidResult.success(None)
        else {
          val validResults: Vector[ValidResult[Raw]] =
            array.zipWithIndex
              .map { p =>
                val validResult =
                  if (isIgnore)
                    _valueCodec.unapply(p._1)
                      .flatMap(_contract.$get(_).toOption)
                      .fold(ValidResult.success(p._1))(t => f(t).map(_valueCodec(_).value))
                  else
                    _valueCodec.unapply(p._1)
                      .fold[ValidResult[Raw]](ValidResult.failure(IncorrectTypeFailure(_contract, Path.empty, _valueCodec, p._1))) { t =>
                        _contract.$get(t).flatMap(t => f(t).map(_valueCodec(_).value))
                      }
                validResult.left
                  .map(_.map(_.rebase(_root, _path \ p._2)))
              }
          ValidResult.parSequence(validResults).map(Some.apply)
        }
      }
  }

  final def $get(obj:D):ValidResult[Vector[T]] =
    __get(obj).toValidOption.map(_.getOrElse(Vector.empty))

  final def $verify(obj:D):List[StructuralFailure] =
    __incorrectTypeBehaviour.traverse(obj.value, this) match {
      case PathEmptyMaybe => Nil
      case Found(v) => v.zipWithIndex.flatMap { e =>
        _contract.$verify(e._1).map(_.rebase(_root, _path \ e._2))
      }.toList
      case NotFound => Nil
      case Failed(f, tail) => f :: tail
    }

  final def $set(objs:Vector[T]):ValidPathSetter[D] = {
    val validRaw =
      objs.toList.zipWithIndex.flatMap{e =>
        _contract.$verify(e._1).map(_.rebase(_contract, Path(e._2)))
      } match {
        case head :: tail =>
          Left(NonEmptyList(head, tail))
        case Nil =>
          Right(_codec.apply(objs).value)
      }
    if (validRaw.exists(_.isEmpty))
      LiftedSetter(ValueDrop(_path))
    else
      ValidValueSetter(_path, validRaw)
  }

  final def $clear:PathSetter[D] =
    ValueDrop(_path)

  final def $append(element:T):ValidPathSetter[D] = {
    def modifier(d:D): ValidResult[RawArray] =
      _contract.$verify(element) -> __getRaw(d) match {
        case (Nil, Right(d)) => Right(d :+ _valueCodec(element).value)
        case (l, Left(f)) => Left(f ++ l)
        case (head :: tail, _) => Left(NonEmptyList(head, tail))
      }

    RawModifySetter(modifier, _path)
  }

  final def $map(f:T => T):ValidPathSetter[D] =
    RawModifyOrIgnoreSetter(d => __map(d, f), _path)

  final def $map(f:ValidPathSetter[T]):ValidPathSetter[D] =
    RawModifyOrIgnoreSetter(d => __map(d, f), _path)
}

//Any modifications setting empty map should clear the field
private[dsentric] trait MapObjectsLens[D <: DObject, K, T <: DObject] extends PropertyLens[D, Map[K, T]] {
  def _path:Path
  def _contract:ContractFor[T]
  def _keyCodec:StringCodec[K]
  def _valueCodec:DObjectCodec[T]
  val _codec:DMapCodec[K, T] = PessimisticCodecs.fullMapCodec( _valueCodec, _keyCodec)

  private def __getRaw(obj:D):ValidResult[RawObject] =
    PathLensOps.traverse(obj.value, _path).fold[ValidResult[RawObject]](Right(RawObject.empty)){
      case r:RawObject@unchecked => Right(r)
      case _ if __incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour =>
        Right(RawObject.empty)
      case v =>
        ValidResult.failure(IncorrectTypeFailure(this, v))
    }

  private[contracts] def __get(obj: D): Traversed[Map[K, T]] = ???
//    __getRaw(obj)
//      .flatMap{ rawObj =>
//        if (__incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour)
//          ValidResult.success{
//            Some{
//              rawObj.flatMap{p =>
//                _valueCodec.unapply(p._2)
//                  .flatMap(t => _contract.$get(t).toOption)
//                  .map(tt => _keyCodec.fromString(p._1) -> tt)
//              }
//            }
//          }
//        else {
//          val validResults = rawObj.iterator.map{p =>
//            _valueCodec.unapply(p._2)
//              .fold[ValidResult[(K, T)]](ValidResult.failure(IncorrectTypeFailure(_contract, Path.empty, _valueCodec, p._2))){t =>
//                _contract.$get(t)
//                  .map(tt => _keyCodec.fromString(p._1) -> tt)
//              }
//              .left
//              .map(_.map(_.rebase(_root, _path \ p._1)))
//          }.toVector
//          ValidResult.parSequence(validResults).map(i => Some(i.toMap))
//        }
//      }

  private[contracts] def __map(obj: D, f:T => T): ValidResult[Option[RawObject]] = {
    __getRaw(obj)
      .flatMap{ map =>
        if (map.isEmpty) ValidResult.success(None)
        else if (__incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour)
          ValidResult.success{
            Some {
              map.flatMap{p =>
                _valueCodec.unapply(p._2)
                  .flatMap(t => _contract.$get(t).toOption)
                  .map(t => p._1 -> _valueCodec(f(t)).value)
              }
            }
          }
        else {
          val validResults: Vector[ValidResult[(String, Raw)]] =
            map
              .map { p =>
                _valueCodec.unapply(p._2)
                  .fold[ValidResult[(String, Raw)]](ValidResult.failure(IncorrectTypeFailure(_contract, Path.empty, _valueCodec, p._2))) { t =>
                    _contract.$get(t).map(t => p._1 -> _valueCodec(f(t)).value)
                  }
                  .left
                  .map(_.map(_.rebase(_root, _path \ p._1)))
              }.toVector
          ValidResult.parSequence(validResults).map(v => Some(v.toMap))
        }
      }
  }

  private[contracts] def __map(obj: D, f:ValidPathSetter[T]): ValidResult[Option[RawObject]] = {
    val isIgnore = __incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour
    __getRaw(obj)
      .flatMap{ map =>
        if (map.isEmpty) ValidResult.success(None)
        else {
          val validResults: Vector[ValidResult[(String, Raw)]] =
            map
              .map { p =>
                val validResult =
                  if (isIgnore)
                    _valueCodec.unapply(p._2)
                      .flatMap(_contract.$get(_).toOption)
                      .fold(ValidResult.success(p))(t => f(t).map(v => p._1 -> _valueCodec(v).value))
                  else
                    _valueCodec.unapply(p._2)
                      .fold[ValidResult[(String, Raw)]](ValidResult.failure(IncorrectTypeFailure(_contract, Path.empty, _valueCodec, p._2))) { t =>
                        _contract.$get(t).flatMap(t => f(t).map(v => p._1 -> _valueCodec(v).value))
                      }
                validResult.left
                  .map(_.map(_.rebase(_root, _path \ p._1)))
              }.toVector
          ValidResult.parSequence(validResults).map(v => Some(v.toMap))
        }
      }
  }

  private[contracts] def __applyTraversal(obj: RawObject):Either[NonEmptyList[StructuralFailure], RawObject] = ???

  private[contracts] def __verifyTraversal(obj:  RawObject): List[StructuralFailure] = ???

  final def $verify(obj: D): List[StructuralFailure] = ???
//    __incorrectTypeBehaviour.traverse(obj.value, this)
//      .fold(_.toList, mv => mv.getOrElse(Vector.empty).toList.flatMap { e =>
//        _contract.$verify(e._2).map(_.rebase(_root, _path \ _keyCodec.toString(e._1)))
//      })

  final def $get(obj:D):ValidResult[Map[K, T]] =
    __get(obj).toValidOption.map(_.getOrElse(Map.empty))

  final def $get(k:K)(obj:D):ValidResult[Option[T]] = ???
//  {
//    val key = _keyCodec.toString(k)
//    __incorrectTypeBehaviour.traverse(obj.value, _root, _path \ key, _valueCodec)
//      .flatMap {
//        case None =>
//          Right(None)
//        case Some(t) =>
//          _contract.$get(t) match {
//            case Left(f) =>
//              if (__incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour)
//                Right(None)
//              else
//                Left(f.map(_.rebase(_root, _path \ key)))
//            case Right(v) =>
//              Right(Some(v))
//          }
//      }
//  }

  final def $exists(k:K)(obj:D):ValidResult[Boolean] =
    __getRaw(obj).map(_.contains(_codec.keyCodec.toString(k)))

  final def $set(objs:Map[K, T]):ValidPathSetter[D] = {
    val validRaw =
      objs.flatMap{e =>
        _contract.$verify(e._2).map(_.rebase(_contract, Path(_keyCodec.toString(e._1))))
      } match {
        case head :: tail =>
          Left(NonEmptyList(head, tail))
        case Nil =>
          Right(_codec.apply(objs).value)
      }
    if (validRaw.exists(_.isEmpty))
      LiftedSetter(ValueDrop(_path))
    else
      ValidValueSetter(_path, validRaw)
  }

  final def $clear:PathSetter[D] =
    ValueDrop(_path)

  final def $remove(key:K):ValidPathSetter[D] =
    RawModifyDropOrIgnoreSetter(d => __getRaw(d).map{ obj =>
      if (obj.isEmpty) None
      else Some {
        val newObj = obj - _keyCodec.toString(key)
        if (newObj.isEmpty) None
        else Some(newObj)
      }
    }, _path)

  final def $add(keyValue:(K, T)):ValidPathSetter[D] = {
    def modifier(d:D): ValidResult[RawObject] =
      (_contract.$verify(keyValue._2) -> __getRaw(d)) match {
        case (Nil, Right(d)) =>
          Right(d + (_keyCodec.toString(keyValue._1) ->  _valueCodec(keyValue._2).value))
        case (l, Left(f)) =>
          Left(f ++ l)
        case (head :: tail, _) =>
          Left(NonEmptyList(head, tail))
      }

    RawModifySetter(modifier, _path)
  }

  final def $map(f:T => T):ValidPathSetter[D] =
    RawModifyOrIgnoreSetter(d => __map(d, f), _path)

  final def $map(f:ValidPathSetter[T]):ValidPathSetter[D] =
    RawModifyOrIgnoreSetter(d => __map(d, f), _path)
}


private[dsentric] object ObjectLens {

  def propertyApplicator[D <: DObject](
                                        baseContract:BaseContract[D],
                                        obj:RawObject):Either[NonEmptyList[StructuralFailure], RawObject] = {
    val propertyObject =
      baseContract._fields.foldLeft[Either[NonEmptyList[StructuralFailure], RawObject]](Right(obj)){
        case (Right(d), (_, p:Property[D, Any]@unchecked)) =>
          p.__applyTraversal(d)
        case (l:Left[NonEmptyList[StructuralFailure], RawObject], (_, p:Property[D, Any]@unchecked)) =>
          p.__applyTraversal(obj) match {
            case Left(failures) =>
              Left(failures ::: l.value)
            case _ =>
              l

          }
      }
    propertyObject match {
      case Left(failures) =>
        Left(failures ++ additionPropertyVerifier(baseContract, obj))
      case Right(newObj) =>
        additionalPropertyApplicator(baseContract, newObj)
    }

  }

  def propertyVerifier[D <: DObject](
                                      baseContract:BaseContract[D],
                                      obj:RawObject):List[StructuralFailure] =
    baseContract._fields.flatMap{
      case (_, p:Property[D, Any]@unchecked) =>
        p.__verifyTraversal(obj)
    }.toList ++ additionPropertyVerifier(baseContract, obj)

  /**
   * Reduces empty property fields
   * */
  def verifyReduce[D <: DObject](baseContract:BaseContract[D],
                                    obj:RawObject):Either[NonEmptyList[StructuralFailure], RawObject] = {

    val init = additionPropertyVerifier(baseContract, obj) match {
      case head :: tail => Left(NonEmptyList(head, tail))
      case _ => Right(obj)
    }

    baseContract._fields.foldLeft(init){
      case (Right(d), (_, p:ObjectLens[D]@unchecked)) =>
        p.__verifyReduce(d)
      case (Right(d), (_, p:Property[D, Any]@unchecked)) =>
        p.__verifyTraversal(d) match {
          case Nil =>
            Right(d)
          case head :: tail =>
            Left(NonEmptyList(head, tail))
        }
      case (l:Left[NonEmptyList[StructuralFailure], RawObject], (_, p:Property[D, Any]@unchecked)) =>
        p.__verifyTraversal(obj) match {
          case Nil =>
            l
          case head :: tail =>
            Left(NonEmptyList(head, tail) ::: l.value)
        }
    }
  }


  private def additionalPropertyApplicator[D <: DObject](baseContract:BaseContract[D],
                                                         obj:RawObject):Either[NonEmptyList[StructuralFailure], RawObject] = {
    val exclude = baseContract._fields.keySet
    baseContract match {
      case _:AdditionalProperties =>
        Right(obj)

      case a:AdditionalPropertyValues[Any, Any]@unchecked =>
        obj.filter(p => !exclude.contains(p._1))
          .toList
          .flatMap { kv =>
            baseContract.__incorrectTypeBehaviour.verifyKey(kv._1, baseContract._root, baseContract._path, a._additionalKeyCodec) ++
            baseContract.__incorrectTypeBehaviour.verify(kv._2, baseContract._root, baseContract._path, a._additionalValueCodec)
          } match {
          case head :: tail =>
            Left(NonEmptyList(head, tail))
          case _ =>
            Right(obj)
        }

      case a:AdditionalPropertyObjects[Any, DObject]@unchecked =>
        ValidResult.parSequence[StructuralFailure, (String, Raw)] {
          obj.filter(p => !exclude.contains(p._1))
            .toVector
            .flatMap { kv =>
              val keyFailures = baseContract.__incorrectTypeBehaviour.verifyKey(kv._1, baseContract._root, baseContract._path, a._additionalKeyCodec)
              val validValue = baseContract.__incorrectTypeBehaviour(kv._2, baseContract._root, baseContract._path \ kv._1, a._additionalValueCodec)
              validValue -> keyFailures match {
                case (Failed(typeFailure, tail), failures) =>
                  Some(Left(NonEmptyList(typeFailure, tail ++ failures.toList)))
                case (NotFound, None) =>
                  None
                case (NotFound, Some(failure)) =>
                  Some(Left(NonEmptyList(failure, Nil)))
                case (Found(value), None) =>
                  //Important to unapply the codec as there may be more structure involved in the type.
                  val traversed =
                    propertyApplicator(a._additionalContract, value.value)
                      .map(d2 => kv._1 -> a._additionalValueCodec.apply(value.internalWrap(d2)).value)
                  Some(traversed.left.map(_.map(_.rebase(baseContract._root, baseContract._path))))
                case (Found(value), Some(failure)) =>
                  val valueFailures =
                    propertyVerifier(a._additionalContract, value.value)
                      .map(_.rebase(baseContract._root, baseContract._path))
                  Some(Left(NonEmptyList(failure, valueFailures)))
              }
            }
        }.map(pairs => obj ++ pairs)

      case _ =>
        obj
          .keys
          .filterNot(exclude)
          .map(k => ClosedContractFailure(baseContract._root, baseContract._path, k))
          .toList match {
            case Nil =>
              Right(obj)
            case head :: tail =>
              Left(NonEmptyList(head, tail))
          }
    }
  }

  private def additionPropertyVerifier[D <: DObject](
                                                      baseContract:BaseContract[D],
                                                      obj:RawObject
                                                    ):List[StructuralFailure] = {
    val exclude = baseContract._fields.keySet
    baseContract match {
      case _:AdditionalProperties =>
        Nil
      case a:AdditionalPropertyValues[Any, Any]@unchecked =>
        obj.filter(p => !exclude.contains(p._1))
          .toList
          .flatMap { kv =>
            baseContract.__incorrectTypeBehaviour.verifyKey(kv._1, baseContract._root, baseContract._path, a._additionalKeyCodec) ++
              baseContract.__incorrectTypeBehaviour.verify(kv._2, baseContract._root, baseContract._path, a._additionalValueCodec)
          }

      case a:AdditionalPropertyObjects[Any, DObject]@unchecked =>
        obj.filter(p => !exclude.contains(p._1))
          .toList
          .flatMap { kv =>
            FailOnIncorrectTypeBehaviour.verifyKey(kv._1, baseContract._root, baseContract._path, a._additionalKeyCodec) ++
            (FailOnIncorrectTypeBehaviour.apply(kv._2, baseContract._root, baseContract._path \ kv._1, a._additionalValueCodec) match {
              case Failed(f, tail) => f :: tail
              case NotFound => Nil
              case Found(v) =>
                propertyVerifier(a._additionalContract, v.value)
                  .map(_.rebase(baseContract._root, baseContract._path))
            })
          }
      case _ =>
        obj.keys
          .filterNot(exclude)
          .map(k => ClosedContractFailure(baseContract._root, baseContract._path, k))
          .toList
    }
  }
}


