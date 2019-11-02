package dsentric.contracts

import cats.data.NonEmptyList
import dsentric.DObject
import dsentric.failure._


private[dsentric] sealed trait ObjectLens[D <: DObject] extends PropertyLens[D, DObject]{

  def _fields: Map[String, Property[D, _]]

  private[dsentric] def __get(obj:D):ValidResult[Option[DObject]] =
    ObjectLens.propertyApplicator(_fields, obj)
      .flatMap{d =>
        __incorrectTypeBehaviour.traverse(d.value, _path, _codec)
      }
}

private[dsentric] trait ExpectedObjectLens[D <: DObject] extends ObjectLens[D]{

  def _fields: Map[String, Property[D, _]]

  def $get(obj:D):ValidResult[DObject] =
    __get(obj).map(_.getOrElse(DObject.empty))

  def unapply(obj:D):Option[D] =
    ???
}

private[dsentric] trait MaybeObjectLens[D <: DObject] extends ObjectLens[D] {

  def _fields: Map[String, Property[D, _]]

  def unapply(obj:D):Option[Option[D]] =
    ???

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
}

