package dsentric.contracts

import dsentric.DObject
import dsentric.failure.ValidResult


trait ContractLens[D <: DObject] {

  def _fields: Map[String, Property[D, _]]

  //includes any default values and checks expected values and types
  def $get(obj:D):ValidResult[D] =
    ObjectLens.propertyApplicator(_fields, obj)

  def $modify(d:D)(f:this.type => D => D):D =
    f(this)(d)

  def $delta(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)


}
