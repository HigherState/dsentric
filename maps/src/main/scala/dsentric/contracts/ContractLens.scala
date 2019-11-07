package dsentric.contracts

import dsentric.DObject
import dsentric.failure.{Failure, ValidResult}


trait ContractLens[D <: DObject] {

  def _fields: Map[String, Property[D, _]]

  //includes any default values and checks expected values and types
  def $get(obj:D):ValidResult[D] =
    ObjectLens.propertyApplicator(_fields, obj)

  def $verify(obj:D):List[Failure] =
    ObjectLens.propertyVerifier(_fields, obj)

  def $modify(f:this.type => D => D):D => D =
    f(this)

  def $validModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  def $delta(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)


}
