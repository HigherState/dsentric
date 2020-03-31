package dsentric.contracts

import dsentric._
import dsentric.failure._
import cats.data._

private[dsentric] sealed trait ObjectLens[D <: DObject] extends PropertyLens[D, DObject]{

  def _fields: Map[String, Property[D, _]]

  def $additionalProperties:AdditionalProperties

  final def $modify(f:this.type => D => D):D => D =
    f(this)

  final def $verifyModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  final def $verify(obj:D):List[StructuralFailure] = {
    //Verifying ignores EmptyOnIncorrectTypeBehaviour
    val failures =
      FailOnIncorrectTypeBehaviour.traverse(obj.value, this) match {
        case Right(Some(t)) =>
          $additionalProperties.verify(_root, _path, t.value -- _fields.keySet)
        case Left(NonEmptyList(head, tail)) =>
          head :: tail
        case _ =>
          Nil
      }
    failures ++ ObjectLens.propertyVerifier(_fields, obj)
  }

  final def $add(d:(String, Data)):ValidPathSetter[D] = ???

  final def $addMany(d:Iterable[(String, Data)]):ValidPathSetter[D] = ???

  private def verifyResult(objectBeingSet:DObject): D => List[StructuralFailure] = (obj:D) =>
    ObjectLens.propertyVerifier(_fields, obj) ++
    $additionalProperties.verify(_root, _path, objectBeingSet.value -- _fields.keySet)

  //Dynamic object so validation is against root contract
  final def $set(d:DObject):ValidPathSetter[D] =
    VerifyValueSetter(_path, d.value, verifyResult(d))

  final def $maybeSet(d:Option[DObject]):ValidPathSetter[D] =
    d.fold[ValidPathSetter[D]](IdentityValidSetter[D]()) { v =>
      VerifyValueSetter(_path, v.value, verifyResult(v))
    }

  private[dsentric] def __get(obj:D):ValidResult[Option[DObject]] =
    for {
      d <- ObjectLens.propertyApplicator(_fields, obj)
      l <- __incorrectTypeBehaviour.traverse(d.value, this)
      r <- l.map { v =>
        $additionalProperties.applicator(_root, _path, v.value -- _fields.keys)
          .map(ap => Some(new DObjectInst(v.value ++ ap)))
        }.getOrElse(Right(None))
    } yield r

}

private[dsentric] trait ExpectedObjectLens[D <: DObject] extends ObjectLens[D]{

  def _fields: Map[String, Property[D, _]]

  def $get(obj:D):ValidResult[DObject] =
    __get(obj).map(_.getOrElse(DObject.empty))

  def unapply(obj:D):Option[DObject] =
    $get(obj).toOption //TODO Optimise
}

private[dsentric] trait MaybeObjectLens[D <: DObject] extends ObjectLens[D] {

  def _fields: Map[String, Property[D, _]]

  def unapply(obj:D):Option[Option[DObject]] =
    $get(obj).toOption //TODO Optimise

  def $get(obj:D):ValidResult[Option[DObject]] =
    obj.get(_path) match {
      case None =>
        Right(None)
      case _ =>
        __get(obj)
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

  private[contracts] def __get(obj: D): ValidResult[Option[Vector[T]]] =
    __getRaw(obj)
    .flatMap{ array =>
      if (__incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour)
        ValidResult.success{
          Some {
            array.flatMap{e =>
              val a = _valueCodec.unapply(e)
              a.flatMap(t => _contract.$get(t).toOption)

            }
          }
        }
      else {
        val validResults: Vector[ValidResult[T]] =
          array.zipWithIndex
            .map { p =>
              _valueCodec.unapply(p._1)
                .fold[ValidResult[T]](ValidResult.failure(IncorrectTypeFailure(_contract, Path.empty, _valueCodec, p._1))) { t =>
                  _contract.$get(t)
                }
                .left
                .map(_.map(_.rebase(_root, _path \ p._2)))
            }
        ValidResult.parSequence(validResults).map(Some.apply)
      }
    }

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
    __get(obj).map(_.getOrElse(Vector.empty))

  final def $verify(obj:D):List[StructuralFailure] =
    __incorrectTypeBehaviour.traverse(obj.value, this)
    .fold(_.toList, mv => mv.getOrElse(Vector.empty).toList.zipWithIndex.flatMap { e =>
      _contract.$verify(e._1).map(_.rebase(_root, _path \ e._2))
    })

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

  private[contracts] def __get(obj: D): ValidResult[Option[Map[K, T]]] =
    __getRaw(obj)
      .flatMap{ rawObj =>
        if (__incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour)
          ValidResult.success{
            Some{
              rawObj.flatMap{p =>
                _valueCodec.unapply(p._2)
                  .flatMap(t => _contract.$get(t).toOption)
                  .map(tt => _keyCodec.fromString(p._1) -> tt)
              }
            }
          }
        else {
          val validResults = rawObj.iterator.map{p =>
            _valueCodec.unapply(p._2)
              .fold[ValidResult[(K, T)]](ValidResult.failure(IncorrectTypeFailure(_contract, Path.empty, _valueCodec, p._2))){t =>
                _contract.$get(t)
                  .map(tt => _keyCodec.fromString(p._1) -> tt)
              }
              .left
              .map(_.map(_.rebase(_root, _path \ p._1)))
          }.toVector
          ValidResult.parSequence(validResults).map(i => Some(i.toMap))
        }
      }

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

  final def $verify(obj: D): List[StructuralFailure] =
    __incorrectTypeBehaviour.traverse(obj.value, this)
      .fold(_.toList, mv => mv.getOrElse(Vector.empty).toList.flatMap { e =>
        _contract.$verify(e._2).map(_.rebase(_root, _path \ _keyCodec.toString(e._1)))
      })

  final def $get(obj:D):ValidResult[Map[K, T]] =
    __get(obj).map(_.getOrElse(Map.empty))

  final def $get(k:K)(obj:D):ValidResult[Option[T]] = {
    val key = _keyCodec.toString(k)
    __incorrectTypeBehaviour.traverse(obj.value, _root, _path \ key, _valueCodec)
      .flatMap {
        case None =>
          Right(None)
        case Some(t) =>
          _contract.$get(t) match {
            case Left(f) =>
              if (__incorrectTypeBehaviour == EmptyOnIncorrectTypeBehaviour)
                Right(None)
              else
                Left(f.map(_.rebase(_root, _path \ key)))
            case Right(v) =>
              Right(Some(v))
          }
      }
  }

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
                                        fields: Map[String, Property[D, _]],
                                        obj:D):ValidResult[D] =
    fields.foldLeft[ValidResult[D]](Right(obj)){
      //TODO Optimise
      case (Right(d), (_, p:Property[D, Any]@unchecked)) =>
        p.__get(obj).map{
          case None =>
            d.-\(p._path).asInstanceOf[D]
          case Some(r) =>
            p.__set(d, r)
        }
      case (l:Left[NonEmptyList[Failure], D], (_, p:Property[D, Any]@unchecked)) =>
        p.__get(obj)
          .swap
          .toOption.fold(l)(f => Left(l.value ++ f.toList))
    }

  def propertyVerifier[D <: DObject](
                                      fields: Map[String, Property[D, _]],
                                      obj:D):List[StructuralFailure] =
    fields.flatMap{
      case (_, p:Property[D, Any]@unchecked) =>
        p.$verify(obj)
    }.toList

}


