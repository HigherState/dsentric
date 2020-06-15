package dsentric.contracts

import dsentric.{DObject, Path}
import dsentric.failure.{StructuralFailure, ValidResult}

trait ContractLens[D <: DObject] { this:ContractFor[D] =>

  def _fields: Map[String, Property[D, _]]

  /**
   * returns object against the contract, applying any default values
   * and verifying the structural integrity of the object
   * @param obj
   * @return
   */
  final def $get(obj:D):ValidResult[D] = {
    val properties =
      this match {
        case _:Closed =>
          ObjectLens.closedPropertyApplicator(_fields, obj)
        case a:AdditionalProperties =>
          ObjectLens.withAdditionalPropertyApplicator(_fields, obj, a.$additionalProperties)
        case _ =>
          ObjectLens.openPropertyApplicator(_fields, obj)
      }
    val closed = $additionalProperties.applicator(this, Path.empty, obj.value -- _fields.keys)
    ValidResult.sequence2(properties, closed).map{p => p._1.internalWrap(p._1.value ++ p._2).asInstanceOf[D]}
  }

  /**
   * verifies the structural integrity of the object against the contract
   * @param obj
   * @return
   */
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

