package dsentric.contracts

import cats.data.NonEmptyList
import dsentric.codecs.std.DCodecs
import dsentric.{Available, DNull, DObject, Failed, Found, NotFound, Path, Raw, RawArray, RawObject, RawObjectOps}
import dsentric.codecs.{
  DCodec,
  DCollectionCodec,
  DContractCodec,
  DCoproductCodec,
  DKeyContractCollectionCodec,
  DMapCodec,
  DParameterisedContractCodec,
  DProductCodec,
  DTypeContractCodec,
  DValueClassCodec,
  DValueCodec
}
import dsentric.failure.{
  AdditionalElementFailure,
  ClosedContractFailure,
  ContractTypeResolutionFailure,
  CoproductTypeValueFailure,
  ExpectedFailure,
  Failure,
  IncorrectKeyTypeFailure,
  IncorrectTypeFailure,
  MissingElementFailure,
  ValidResult
}
import shapeless.HList

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Verifies the structure of the document and removes any Empty objects or Null values.
 * Bad types can be dropped or return failures
 */
private[contracts] trait ReduceOps {

  /**
   * Reduces empty property fields, removing DCodec values that return NotFound
   *
   * Ultimately clearing out empty Objects as well
   * Will also remove any nulls
   */
  def reduce[D <: DObject](baseContract: BaseContract[D], obj: RawObject, badTypes: BadTypes): Available[RawObject] = {

    def reduceAdditionalProperties: ValidResult[RawObject] = {
      val exclude = baseContract._fields.keySet
      baseContract match {
        case a: AdditionalProperties[Any, Any] @unchecked =>
          val (baseObject, additionalObject) = obj.partition(p => exclude(p._1))
          if (additionalObject.isEmpty) ValidResult.success(baseObject)
          else {
            reduceMap(
              a._root,
              a._path,
              badTypes,
              DCodecs.keyValueMapCodec(a._additionalKeyCodec, a._additionalValueCodec),
              additionalObject
            ) match {
              case Found(rawObject) if rawObject == additionalObject =>
                ValidResult.success(obj)
              case Found(rawObject)                                  =>
                ValidResult.success(baseObject ++ rawObject)
              case NotFound                                          =>
                ValidResult.success(baseObject)
              case Failed(head, tail)                                =>
                ValidResult.failure(head, tail)
            }
          }
        case _ if badTypes == DropBadTypes                =>
          ValidResult.success(obj.view.filterKeys(exclude).toMap)
        case _                                            =>
          val closed = obj.view.filterKeys(key => !exclude.contains(key))

          if (closed.isEmpty) ValidResult.success(obj)
          else
            RawObjectOps
              .reduceMap(closed.toMap, DNull)
              .toList
              .flatMap(_.keys.map(key => ClosedContractFailure(baseContract._root, baseContract._path, key))) match {
              case head :: tail =>
                ValidResult.failure(head, tail)
              case Nil          =>
                ValidResult.success(obj -- closed.keys)
            }
      }
    }

    val drop = badTypes.nest == DropBadTypes
    baseContract._fields.foldLeft(reduceAdditionalProperties) {
      case (Right(d), (_, p))      =>
        p.__reduce(d, drop)
      case (l @ Left(nel), (_, p)) =>
        p.__reduce(obj, drop) match {
          case Right(_)   =>
            l
          case Left(nel2) =>
            Left(nel ::: nel2)
        }
    } match {
      case Right(obj) if obj.isEmpty      =>
        NotFound
      case Right(obj)                     =>
        Found(obj)
      case Left(NonEmptyList(head, tail)) =>
        Failed(head, tail)
    }
  }

  def reduce[D <: DObject, T](propertyLens: ValuePropertyLens[D, T], raw: Raw, badTypes: BadTypes): Available[Raw] = {
    val reduce = reduceCodec(propertyLens._root, propertyLens._path, badTypes)(propertyLens._codec -> raw)
    propertyLens.__applyConstraints(reduce, badTypes)
  }

  protected def reduceCodec[D <: DObject, C](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes
  ): Function[(DCodec[C], Raw), Available[Raw]] = {
    case (_, DNull)                                                                   =>
      NotFound
    case (_, rawObject: RawObject @unchecked) if RawObjectOps.reducesEmpty(rawObject) =>
      NotFound
    case (d: DValueCodec[C], raw)                                                     =>
      reduceValue(contract, path, badTypes, d, raw)
    case (d: DMapCodec[C, _, _], rawObject: RawObject @unchecked)                     =>
      reduceMap(contract, path, badTypes, d, rawObject)
    case (d: DCollectionCodec[C, _], rawArray: RawArray @unchecked)                   =>
      reduceCollection(contract, path, badTypes, d, rawArray)
    case (d: DContractCodec[_], rawObject: RawObject @unchecked)                      =>
      reduceContract(contract, path, badTypes, d.contract, rawObject)
    case (d: DParameterisedContractCodec[_, _], rawObject: RawObject @unchecked)      =>
      reduceParameterisedContract(contract, path, badTypes, d, rawObject)
    case (d: DKeyContractCollectionCodec[C, _], rawObject: RawObject @unchecked)      =>
      reduceKeyContractCollection(contract, path, badTypes, d, rawObject)
    case (d: DValueClassCodec[C, _], raw)                                             =>
      reduceValueClass(contract, path, badTypes, d, raw)
    case (d: DProductCodec[C, _, _], rawArray: RawArray @unchecked)                   =>
      reduceProduct(contract, path, badTypes, d, rawArray)
    case (d: DCoproductCodec[C, _], raw)                                              =>
      reduceCoproduct(contract, path, badTypes, d, raw)
    case (d: DTypeContractCodec[_], rawObject: RawObject @unchecked)                  =>
      reduceTypeContract(contract, path, badTypes, d.contracts, d.cstr, rawObject)
    case _ if badTypes == DropBadTypes                                                =>
      NotFound
    case (d, raw)                                                                     =>
      Failed(IncorrectTypeFailure(contract, path, d, raw))
  }

  protected def reduceValue[D <: DObject, V](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codec: DValueCodec[V],
    raw: Raw
  ): Available[Raw] =
    raw match {
      case rawObject: RawObject @unchecked =>
        RawObjectOps.reduceMap(rawObject, DNull).fold[Available[Raw]](NotFound) { r =>
          codec.unapply(r) match {
            case None if badTypes == DropBadTypes =>
              NotFound
            case None                             =>
              Failed(IncorrectTypeFailure(contract, path, codec, raw))
            case Some(_)                          =>
              Found(r)
          }
        }
      case r                               =>
        codec.unapply(r) match {
          case None if badTypes == DropBadTypes =>
            NotFound
          case None                             =>
            Failed(IncorrectTypeFailure(contract, path, codec, raw))
          case Some(_)                          =>
            Found(r)
        }
    }

  protected def reduceMap[D <: DObject, C, K, V](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codec: DMapCodec[C, K, V],
    raw: RawObject
  ): Available[RawObject] = {
    val nest = badTypes.nest
    raw.view
      .map { p =>
        val key   =
          codec.keyCodec.unapply(p._1) -> p._2 match {
            case (None, _) if nest == DropBadTypes                               =>
              NotFound
            case (None, DNull)                                                   =>
              NotFound
            case (None, r: RawObject @unchecked) if RawObjectOps.reducesEmpty(r) =>
              NotFound
            case (None, _)                                                       =>
              Failed(IncorrectKeyTypeFailure(contract, path, codec.keyCodec, p._1))
            case (Some(_), _)                                                    =>
              Found(p._1)
          }
        val value = reduceCodec(contract, path \ p._1, nest)(codec.valueCodec -> p._2)
        Available.sequence2(key, value)
      }
      .foldLeft[Either[ListBuffer[Failure], mutable.Builder[(String, Raw), Map[String, Raw]]]](
        Right(Map.newBuilder[String, Raw])
      ) {
        case (Right(mb), Found(pair))       =>
          Right(mb.addOne(pair))
        case (Right(_), Failed(head, tail)) =>
          Left(new ListBuffer[Failure].addAll(head :: tail))
        case (Left(lb), Failed(head, tail)) =>
          Left(lb.addAll(head :: tail))
        case (result, _)                    =>
          result
      } match {
      case Right(mb)                           =>
        val map = mb.result()
        //Incase codec has constraint like max/min size
        if (map.isEmpty)
          NotFound
        else if (codec.unapply(map).isEmpty)
          if (badTypes == DropBadTypes)
            NotFound
          else
            Failed(IncorrectTypeFailure(contract, path, codec, map))
        else
          Found(map)
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(lb)                            =>
        val head :: tail = lb.result()
        Failed(head, tail)
    }
  }

  protected def reduceCollection[D <: DObject, S, T](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codec: DCollectionCodec[S, T],
    raw: RawArray
  ): Available[RawArray] = {
    val nest = badTypes.nest
    raw.zipWithIndex
      .map { p =>
        reduceCodec(contract, path \ p._2, nest)(codec.valueCodec -> p._1) -> p._2
      }
      .foldLeft[Either[ListBuffer[Failure], mutable.Builder[Raw, Vector[Raw]]]](Right(Vector.newBuilder[Raw])) {
        case (Right(vb), (Found(t), _))          =>
          Right(vb.addOne(t))
        case (Right(_), (Failed(head, tail), _)) =>
          Left(new ListBuffer[Failure].addAll(head :: tail))
        case (Left(lb), (Failed(head, tail), _)) =>
          Left(lb.addAll(head :: tail))
        case (Right(_), (NotFound, index))       =>
          Left(new ListBuffer[Failure].addOne(MissingElementFailure(contract, codec, path \ index)))
        case (Left(lb), (NotFound, index))       =>
          Left(lb.addOne(MissingElementFailure(contract, codec, path \ index)))
        case (result, _)                         =>
          result
      } match {
      case Right(vb)                           =>
        val vector = vb.result()
        if (codec.unapply(vector).isEmpty)
          if (badTypes == DropBadTypes)
            NotFound
          else
            Failed(IncorrectTypeFailure(contract, path, codec, vector))
        else
          Found(vector)
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(lb)                            =>
        val head :: tail = lb.result()
        Failed(head, tail)
    }
  }

  protected def reduceValueClass[D <: DObject, T, S](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codec: DValueClassCodec[T, S],
    raw: Raw
  ): Available[Raw] =
    reduceCodec(contract, path, badTypes)(codec.internalCodec -> raw) match {
      case Found(codecRaw) =>
        codec.unapply(codecRaw) match {
          case Some(_)                          =>
            Found(codecRaw)
          case None if badTypes == DropBadTypes =>
            NotFound
          case None                             =>
            Failed(IncorrectTypeFailure(contract, path, codec, raw))
        }
      case f               =>
        f.asInstanceOf[Available[T]]
    }

  protected def reduceProduct[D <: DObject, T, E <: HList, H <: HList](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codec: DProductCodec[T, E, H],
    raw: RawArray
  ): Available[RawArray] = {
    val nest = badTypes.nest
    val init =
      if (raw.size < codec.codecsArray.length) {
        val lb = new ListBuffer[Failure]
        for (i <- raw.size to codec.codecsArray.length)
          yield lb.addOne(MissingElementFailure(contract, codec, path \ i))
        Left(lb)
      } else
        Right(Vector.newBuilder[Raw])

    raw.zipWithIndex
      .map { p =>
        if (p._2 >= codec.codecsArray.length)
          Failed(AdditionalElementFailure(contract, path \ p._2)) -> p._2
        else
          reduceCodec(contract, path \ p._2, nest)(codec.codecsArray(p._2) -> p._1) -> p._2
      }
      .foldLeft[Either[ListBuffer[Failure], mutable.Builder[Raw, Vector[Raw]]]](init) {
        case (Right(vb), (Found(t), _))          =>
          Right(vb.addOne(t))
        case (Right(_), (Failed(head, tail), _)) =>
          Left(new ListBuffer[Failure].addAll(head :: tail))
        case (Left(lb), (Failed(head, tail), _)) =>
          Left(lb.addAll(head :: tail))
        case (Right(_), (NotFound, index))       =>
          Left(new ListBuffer[Failure].addOne(MissingElementFailure(contract, codec, path \ index)))
        case (Left(lb), (NotFound, index))       =>
          Left(lb.addOne(MissingElementFailure(contract, codec, path \ index)))
        case (result, _)                         =>
          result
      } match {
      case Right(vb)                           =>
        val vector = vb.result()
        if (codec.unapply(vector).isEmpty)
          if (badTypes == DropBadTypes)
            NotFound
          else
            Failed(IncorrectTypeFailure(contract, path, codec, vector))
        else
          Found(vector)
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(lb)                            =>
        val head :: tail = lb.result()
        Failed(head, tail)
    }
  }

  protected def reduceContract[D <: DObject, D2 <: DObject](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codecContract: ContractLike[D2],
    raw: RawObject
  ): Available[RawObject] =
    codecContract.__reduce(raw, badTypes.nest == DropBadTypes) match {
      case f: Found[RawObject]                   =>
        f
      case _: Failed if badTypes == DropBadTypes =>
        NotFound
      case f: Failed                             =>
        f.rebase(contract, path)
      case NotFound                              =>
        NotFound
    }

  protected def reduceParameterisedContract[D <: DObject, D2 <: DObject, H <: HList](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codec: DParameterisedContractCodec[D2, H],
    raw: RawObject
  ): Available[RawObject] = {
    val (valueRawObject, extracted) = codec.extractParameters(raw)
    val resolvedFailure             =
      extracted.left.map(_.map {
        case (field, None)                           =>
          ExpectedFailure(contract, path \ field)
        case (field, Some((failedRaw, failedCodec))) =>
          IncorrectTypeFailure(contract, path \ field, failedCodec, failedRaw)
      })

    codec.contract.__reduce(valueRawObject, badTypes.nest == DropBadTypes) -> resolvedFailure match {
      case (Found(rawObject), Right(function)) =>
        Found(codec(function(rawObject)))
      case _ if badTypes.nest == DropBadTypes  =>
        NotFound
      case (f: Failed, Left(failures))         =>
        f.rebase(contract, path) ++ failures.toList
      case (_, Left(failures))                 =>
        Failed(failures.head, failures.tail)
      case (f: Failed, _)                      =>
        f.rebase(contract, path)
      case (NotFound, Right(function))         =>
        Found(codec(function(RawObject.empty)))
    }
  }

  protected def reduceKeyContractCollection[S, D <: DObject, D2 <: DObject](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codec: DKeyContractCollectionCodec[S, D2],
    raw: RawObject
  ): Available[RawObject] =
    raw.foldLeft[Either[ListBuffer[Failure], mutable.Builder[(String, Raw), Map[String, Raw]]]](
      Right(Map.newBuilder[String, Raw])
    ) {
      case (Right(b), (key, value: RawObject @unchecked)) =>
        codec.cstr(key, value) match {
          case None if badTypes == DropBadTypes =>
            Right(b)
          case None =>
            Left(new ListBuffer[Failure].addOne(IncorrectTypeFailure(contract, path, codec, raw)))
          case Some(entity) =>
            codec.contract.__reduce(entity.value, badTypes.nest == DropBadTypes) match {
              case Found(rawObject) =>
                Right(b.addOne(codec.dstr(entity.internalWrap(rawObject).asInstanceOf[D2])))
              case _: Failed if badTypes == DropBadTypes =>
                Right(b)
              case NotFound =>
                Right(b)
              case f: Failed =>
                val rebase = f.rebase(contract, path \ key)
                Left(new ListBuffer[Failure].addAll(rebase.failure :: rebase.tail))
            }
        }
      case (Left(vf), (key, value: RawObject @unchecked)) =>
        codec.cstr(key, value) match {
          case None =>
            Left(vf.addOne(IncorrectTypeFailure(contract, path, codec, raw)))
          case Some(entity) =>
            Left(vf.addAll(codec.contract.__verify(entity.value).map(_.rebase(contract, path \ key))))
        }
      case (Right(_), (key, value))                       =>
        Left(new ListBuffer[Failure].addOne(IncorrectTypeFailure(contract, path \ key, DCodecs.dObjectCodec, value)))
      case (Left(vf), (key, value))                       =>
        Left(vf.addOne(IncorrectTypeFailure(contract, path \ key, DCodecs.dObjectCodec, value)))
    } match {
      case Right(mb)                           =>
        val newRawObject = mb.result()
        if (newRawObject.isEmpty)
          NotFound
        else if (codec.unapply(newRawObject).isEmpty)
          if (badTypes == DropBadTypes)
            NotFound
          else
            Failed(IncorrectTypeFailure(contract, path, codec, newRawObject))
        else
          Found(newRawObject)
      case Left(_) if badTypes == DropBadTypes =>
        NotFound
      case Left(lb)                            =>
        val head :: tail = lb.result()
        Failed(head, tail)
    }

  protected def reduceCoproduct[D <: DObject, T, H <: HList](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    codec: DCoproductCodec[T, H],
    raw: Raw
  ): Available[Raw] =
    codec.codecsList.foldLeft[Available[Raw]](NotFound) {
      case (a: Found[Raw], _) =>
        a
      case (a, c)             =>
        reduceCodec(contract, path, badTypes)(c -> raw) match {
          case f: Failed =>
            a match {
              case f2: Failed => f2 ++ f
              case _          => f
            }
          case a2        =>
            a2
        }
    } match {
      case _: Failed if badTypes == DropBadTypes =>
        NotFound
      case Failed(head, tail)                    =>
        Failed(CoproductTypeValueFailure(contract, codec, path, head :: tail, raw))
      case a                                     =>
        a
    }

  protected def reduceTypeContract[D <: DObject, D2 <: DObject](
    contract: ContractLike[D],
    path: Path,
    badTypes: BadTypes,
    typeCodecs: PartialFunction[D2, ContractLike[D2]],
    d2Cstr: RawObject => D2,
    raw: RawObject
  ): Available[RawObject] =
    typeCodecs.lift(d2Cstr(raw)) match {
      case None if badTypes == DropBadTypes =>
        NotFound
      case None                             =>
        Failed(ContractTypeResolutionFailure(contract, path, raw))
      case Some(typeContract)               =>
        reduceContract(contract, path, badTypes, typeContract, raw)
    }
}

private[contracts] object ReduceOps extends ReduceOps
