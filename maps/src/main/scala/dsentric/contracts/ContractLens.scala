package dsentric.contracts

import cats.data.NonEmptyList
import dsentric.{DObject, Path}
import dsentric.failure.{StructuralFailure, ValidResult}

trait ContractLens[D <: DObject] { this:ContractFor[D] =>

  def _fields: Map[String, Property[D, _]]

  //includes any default values and checks expected values and types
  final def $get(obj:D):ValidResult[D] = {
    val properties = ObjectLens.propertyApplicator(_fields, obj)
    val closed = ObjectLens.closedFailures(this.isInstanceOf[ClosedFields], this, Path.empty, _fields.keySet, obj.keySet)

    properties -> closed match {
      case (Left(failures), f) =>
        Left(failures ++ f)
      case (Right(_), head :: tail) =>
        Left(NonEmptyList(head, tail))
      case (Right(v), Nil) =>
        Right(v)
    }
  }

  final def $verify(obj:D):List[StructuralFailure] =
    ObjectLens.propertyVerifier(_fields, obj) ++
    ObjectLens.closedFailures(this.isInstanceOf[ClosedFields], this, Path.empty, _fields.keySet, obj.keySet)

  final def $modify(f:this.type => D => D):D => D =
    f(this)

  final def $validModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  final def $delta(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)
}
