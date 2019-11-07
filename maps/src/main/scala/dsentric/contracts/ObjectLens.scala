package dsentric.contracts

import dsentric.{DObject, Path}
import dsentric.failure._
import cats.data._

private[dsentric] sealed trait ObjectLens[D <: DObject] extends PropertyLens[D, DObject]{

  def _fields: Map[String, Property[D, _]]

  def $modify(f:this.type => D => D):D => D =
    f(this)

  def $validModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  def $verify(obj:D):List[Failure] =
    __incorrectTypeBehaviour.verify(obj.value, this, false) ++
    ObjectLens.propertyVerifier(_fields, obj)

  private[dsentric] def __get(obj:D):ValidResult[Option[DObject]] =
    ObjectLens.propertyApplicator(_fields, obj)
      .flatMap{d =>
        __incorrectTypeBehaviour.traverse(d.value, this)
      }
}

private[dsentric] trait ExpectedObjectLens[D <: DObject] extends ObjectLens[D]{

  def _fields: Map[String, Property[D, _]]

  def $get(obj:D):ValidResult[DObject] =
    __get(obj).map(_.getOrElse(DObject.empty))

  //Dynamic object so validation is against root contract
  def $set(d:DObject):ValidPathSetter[D] =
    VerifyValueSetter(_path, d.value, ObjectLens.propertyVerifier(_fields, _))

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


private[dsentric] object ObjectLens {

  def propertyApplicator[D <: DObject](fields: Map[String, Property[D, _]], obj:D):ValidResult[D] =
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
        p.__get(obj).swap.toOption.fold(l)(f => Left(l.value ++ f.toList))
    }

  def propertyVerifier[D <: DObject](fields: Map[String, Property[D, _]], obj:D):List[Failure] =
    fields.flatMap{
      case (_, p:Property[D, Any]@unchecked) =>
        p.$verify(obj)
    }.toList
}


//Codec implies it doesnt actually have to be an array as raw type
private[dsentric] trait ObjectsLens[D <: DObject, T <: DObject] extends PropertyLens[D, Vector[T]] {
  def _contract:ContractFor[T]

  private[contracts] def __get(obj: D): ValidResult[Option[Vector[T]]] =
    __incorrectTypeBehaviour.traverse(obj.value, this)
    .flatMap{
      case None =>
        Right(None)
      case Some(v) =>
        val results:Vector[ValidResult[T]] =
          v.zipWithIndex
            .map{p =>
              _contract.$get(p._1)
                .left
                .map(_.map(_.rebase(_root, _path \ p._2)))
            }
        ValidResult.parSequence(results).map(Some.apply)
    }

  def $get(obj:D):ValidResult[Vector[T]] =
    __get(obj).map(_.getOrElse(Vector.empty))

  def $verify(obj:D):List[Failure] =
    __incorrectTypeBehaviour.traverse(obj.value, this)
    .fold(_.toList, mv => mv.getOrElse(Vector.empty).toList.zipWithIndex.flatMap { e =>
      _contract.$verify(e._1).map(_.rebase(_root, _path \ e._2))
    })

  def $set(objs:Vector[T]):ValidPathSetter[D] = {
    val validRaw =
      objs.toList.zipWithIndex.flatMap{e =>
        _contract.$verify(e._1).map(_.rebase(_contract, Path(e._2)))
      } match {
        case head :: tail =>
          Left(NonEmptyList(head, tail))
        case Nil =>
          Right(_codec.apply(objs))
      }
    ValidValueSetter(_path, validRaw)
  }

  def $clear:PathSetter[D] =
    ValueDrop(_path)

  def $append(element:T):ValidPathSetter[D] = {
    def get(d:D): Either[NonEmptyList[Failure], Option[Vector[T]]] = ValidResult.sequence2(_contract.$get(element), __get(d)).map(_._2)
    MaybeModifySetter[D, Vector[T]](get, _.getOrElse(Vector.empty) :+ element, __set)
  }

  def $map(f:T => T):ValidPathSetter[D] =
    ModifySetter[D, Vector[T]](__get, _.map(f), __set)

}

