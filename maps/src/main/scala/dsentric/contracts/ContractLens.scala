package dsentric.contracts

import cats.data.NonEmptyList
import dsentric.{DObject, Path}
import dsentric.failure.{StructuralFailure, ValidResult}

trait ContractLens[D <: DObject] { this:ContractFor[D] =>

  def _fields: Map[String, Property[D, _]]

  //includes any default values and checks expected values and types
  final def $get(obj:D):ValidResult[D] = {
    val properties = ObjectLens.propertyApplicator(_fields, obj)
    val closed = $additionalProperties.applicator(this, Path.empty, obj.value -- _fields.keys)
    ValidResult.sequence2(properties, closed).map{p => p._1.internalWrap(p._1.value ++ p._2).asInstanceOf[D]}
  }

  final def $verify(obj:D):List[StructuralFailure] =
    ObjectLens.propertyVerifier(_fields, obj) ++
    $additionalProperties.verify(this, Path.empty, obj.value -- _fields.keys)

  final def $modify(f:this.type => D => D):D => D =
    f(this)

  final def $validModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  final def $delta(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)
}

