package dsentric.operators

import cats.data.NonEmptyList
import dsentric.contracts.{BaseContract, Property}
import dsentric._

trait Validation[D <: DObject] {
  this: BaseContract[D] =>

  //private lazy val __validationOperatorTree:OperatorTree[Validator] =

//  def $validate(value:D):List[(Path, String)] =
//    _validateFields(Path.empty, value.value, None) match {
//      case head +: tail =>
//        Left(NonEmptyList(head, tail.toList))
//      case _ =>
//        Right(value)
//    }
//
//  def $validate(value:DObject, currentState:D):List[(Path, String)] =
//    _validateFields(Path.empty, value.value, Some(currentState.value)) match {
//      case head +: tail =>
//        Left(NonEmptyList(head, tail.toList))
//      case _ =>
//        Right(value)
//    }
//
//  def $validate(value:D, maybeState:Option[D]):List[(Path, String)] =
//    _validateFields(Path.empty, value.value, maybeState.map(_.value)) match {
//      case head +: tail =>
//        Left(NonEmptyList(head, tail.toList))
//      case _ =>
//        Right(value)
//    }
}