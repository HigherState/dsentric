package dsentric.contracts

import dsentric.{DObject, Path}
import dsentric.failure.{StructuralFailure, ValidResult}

trait ContractLens[D <: DObject] { this:ContractFor[D] =>

  def _fields: Map[String, Property[D, _]]

  /**
   * Returns object against the contract, applying any default values
   * and verifying the structural integrity of the object
   * @param obj
   * @return
   */
  final def $get(obj:D):ValidResult[D] =
    ObjectLens.propertyApplicator(this, obj)


  /**
   * Verifies the structural integrity of the object against the contract
   * @param obj
   * @return
   */
  final def $verify(obj:D):List[StructuralFailure] =
    ObjectLens.propertyVerifier(this, obj)

  final def $modify(f:this.type => D => D):D => D =
    f(this)

  final def $validModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  final def $delta(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)
}

