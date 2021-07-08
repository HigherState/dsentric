package dsentric.contracts

import dsentric.codecs.std.DCodecs
import dsentric.codecs._
import dsentric.failure._
import dsentric._
import shapeless.HList

/**
 * Verifies the structure of the document and removes any Empty objects or Null values.
 * Bad types can be dropped or return failures
 */
private[dsentric] trait VerifyOps {

  /**
   * Reduces empty property fields, removing DCodec values that return NotFound
   *
   * Ultimately clearing out empty Objects as well
   * Will also remove any nulls
   * */
  def verify[D <: DObject](baseContract:BaseContract[D], obj:RawObject):List[Failure] = {

    def verifyAdditionalProperties:Iterator[Failure] = {
      val exclude = baseContract._fields.keySet
      val additionalProperties = obj.view.filterKeys(key => !exclude.contains(key))
      if (additionalProperties.isEmpty) Iterator.empty
      else
        baseContract match {
          case a: AdditionalProperties[Any, Any]@unchecked =>
            verifyMap(a._root, a._path, DCodecs.keyValueMapCodec(a._additionalKeyCodec, a._additionalValueCodec), additionalProperties.toMap).iterator
          case _ =>
            additionalProperties.keys.iterator.map(key => ClosedContractFailure(baseContract._root, baseContract._path, key))
        }
    }

    (baseContract._fields.values.iterator.flatMap(_.__verify(obj)) ++ verifyAdditionalProperties).toList
  }

  def verify[D <: DObject, T](propertyLens: ValuePropertyLens[D, T], raw:Raw):List[Failure] = {
    verifyCodec(propertyLens._root, propertyLens._path)(propertyLens._codec -> raw)
  }

  protected def verifyCodec[D <: DObject, C](contract:ContractFor[D], path:Path):Function[(DCodec[C], Raw), List[Failure]] = {
    case (d:DValueCodec[C], raw) =>
      verifyValue(contract, path,  d, raw)
    case (d:DMapCodec[C, _, _], rawObject:RawObject@unchecked) =>
      verifyMap(contract, path, d, rawObject)
    case (d:DCollectionCodec[C, _], rawArray: RawArray@unchecked) =>
      verifyCollection(contract, path, d, rawArray)
    case (d:DContractCodec[_], rawObject:RawObject@unchecked) =>
      verifyContract(contract, path, d.contract, rawObject)
    case (d:DValueClassCodec[C, _], raw) =>
      verifyCodec(contract, path)(d.internalCodec -> raw)
    case (d:DProductCodec[C, _, _], rawArray:RawArray@unchecked) =>
      verifyProduct(contract, path, d, rawArray)
    case (d:DCoproductCodec[C, _], raw) =>
      verifyCoproduct(contract, path, d, raw)
    case (d:DTypeContractCodec[_], rawObject: RawObject@unchecked) =>
      verifyTypeContract(contract, path, d.contracts, d.cstr, rawObject)
    case (d, raw) =>
      List(IncorrectTypeFailure(contract, path, d, raw))
  }

  protected def verifyValue[D <: DObject, V](contract:ContractFor[D], path:Path, codec:DValueCodec[V], raw:Raw):List[Failure] =
    codec.unapply(raw) match {
      case None =>
        List(IncorrectTypeFailure(contract, path, codec, raw))
      case Some(_) =>
        Nil
    }

  protected def verifyMap[D <: DObject, C, K, V](contract:ContractFor[D], path:Path, codec:DMapCodec[C, K, V], raw:RawObject):List[Failure] =
    raw.view.flatMap { p =>
      val key =
        codec.keyCodec.unapply(p._1) -> p._2 match {
          case (None, _) =>
            Some(IncorrectKeyTypeFailure(contract, path, codec.keyCodec, p._1))
          case (Some(_), _) =>
            None
        }
      val value = verifyCodec(contract, path \ p._1)(codec.valueCodec -> p._2)
      key.foldRight(value)(_ :: _)
    }.toList match {
      case Nil =>
        codec.unapply(raw) match {
          case None =>
            List(IncorrectTypeFailure(contract, path, codec, raw))
          case Some(_) =>
            Nil
        }
      case failures =>
        failures
    }


  protected def verifyCollection[D <: DObject, S, T](contract:ContractFor[D], path:Path, codec:DCollectionCodec[S, T], raw:RawArray):List[Failure] =
    raw.iterator.zipWithIndex.flatMap { p =>
      verifyCodec(contract, path \ p._2)(codec.valueCodec -> p._1)
    }.toList match {
      case Nil =>
        codec.unapply(raw) match {
          case None =>
            List(IncorrectTypeFailure(contract, path, codec, raw))
          case Some(_) =>
            Nil
        }
      case failures =>
        failures
    }

  protected def verifyContract[D <: DObject, D2 <: DObject](contract:ContractFor[D], path:Path, codecContract:ContractFor[D2], raw:RawObject):List[Failure] =
    codecContract.__verify(raw).map(_.rebase(contract, path))

  protected def verifyProduct[D <: DObject, T, E <: HList, H <: HList](contract:ContractFor[D], path:Path, codec:DProductCodec[T, E, H], raw:RawArray):List[Failure] = {
    val missingFailures =
      if (raw.size < codec.codecsArray.length) {
        val failures =
          for (i <- raw.size to codec.codecsArray.length)
            yield MissingElementFailure(contract, codec, path \ i)
        failures.toList
      }
      else
        Nil
    val elementFailures =
      raw.iterator.zipWithIndex.flatMap { p =>
        if (p._2 >= codec.codecsArray.length)
          List(AdditionalElementFailure(contract, path \ p._2))
        else
          verifyCodec(contract, path \ p._2)(codec.codecsArray(p._2) -> p._1)
      }.toList
    elementFailures ++ missingFailures match {
      case Nil =>
        codec.unapply(raw) match {
          case None =>
            Nil
          case Some(_) =>
            List(IncorrectTypeFailure(contract, path, codec, raw))
        }
      case failures =>
        failures
    }

  }


  protected def verifyCoproduct[D <: DObject, T, H <: HList](contract:ContractFor[D], path:Path, codec:DCoproductCodec[T, H], raw:Raw): List[Failure] = {

    codec.codecsList.foldLeft[Option[List[Failure]]](Some(Nil)){
      case (None, _) =>
        None
      case (Some(l), c) =>
        val v = verifyCodec(contract, path)(c -> raw)
        if (v.nonEmpty)
          Some(l ::: v)
        else
          None
    } match {
      case None | Some(Nil) =>
        Nil
      case Some(head :: tail) =>
        List(CoproductTypeValueFailure(contract, codec, path, head :: tail, raw))
    }
  }

  protected def verifyTypeContract[D <: DObject, D2 <: DObject](contract:ContractFor[D], path:Path, typeCodecs:PartialFunction[D2, ContractFor[D2]], d2Cstr: RawObject => D2, raw:RawObject):List[Failure] =
    typeCodecs.lift(d2Cstr(raw)) match {
      case None =>
        List(ContractTypeResolutionFailure(contract, path, raw))
      case Some(typeContract) =>
        verifyContract(contract, path, typeContract, raw)
    }
}

private[dsentric] object VerifyOps extends VerifyOps

