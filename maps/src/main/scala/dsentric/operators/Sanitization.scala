package dsentric.operators

import dsentric.{DCodec, DObject, Path, PathFailures, PathLensOps}
import dsentric.contracts.{BaseContract, Property}

object Sanitization {

//  def sanitize[D <: DObject](contract:BaseContract[D], value:D):D = {
//    contract._fields.foldLeft(value){
//      case (v, (field, property)) =>
//        property._dataOperators.collectFirst{ case s:Sanitizer[_] => s} match {
//          case None =>
//
//          case Some(s) =>
//            PathLensOps.maybeModifyOrDrop(value, contract._path, property._codec, property, s.sanitize)
//            s.sanitize {
//              value.get(contract._path).flatMap(d => property._codec.unapply(d.value))
//            }
//        }
//    }
//  }

}
