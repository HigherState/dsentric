package dsentric.contracts

import dsentric.{DObject, Delta, DeltaEmpty, DeltaFailed, DeltaInst, DeltaReduced, DeltaRemove, DeltaRemoving, Failed, Found, NotFound, RawObject}
import dsentric.failure.{Failure, ValidResult}

trait ContractLens[D <: DObject] { this:ContractFor[D] =>

  def _fields: Map[String, Property[D, _]]

  /**
   * Returns object against the contract, verifying the structural integrity of the object as well as
   * removing empty Objects and Null values.
   * @param obj
   * @return
   */
  final def $reduce(obj:D, dropBadTypes:Boolean = false):ValidResult[D] = {
    val badTypes = if (dropBadTypes) DropBadTypes else FailOnBadTypes
    ObjectPropertyLensOps.reduce(this, obj.value, badTypes) match {
      case Found(raw) =>
        ValidResult.success(obj.internalWrap(raw).asInstanceOf[D])
      case NotFound =>
        ValidResult.success(obj.internalWrap(RawObject.empty).asInstanceOf[D])
      case Failed(head, tail) =>
        ValidResult.failure(head, tail)
    }
  }

  final def $reduceDelta(obj:D, delta:Delta, dropBadTypes:Boolean = false):ValidResult[Delta] = {
    val badTypes = if (dropBadTypes) DropBadTypes else FailOnBadTypes
    ObjectPropertyLensOps.deltaReduce(this, delta.value, obj.value, badTypes) match {
      case DeltaEmpty | DeltaRemove =>
        ValidResult.success(Delta.empty)
      case DeltaReduced(delta) =>
        ValidResult.success(new DeltaInst(delta))
      case DeltaRemoving(delta) =>
        ValidResult.success(new DeltaInst(delta))
      case DeltaFailed(head, tail)  =>
        ValidResult.failure(head, tail)
    }
  }

  /**
   * Verifies the structural integrity of the object against the contract
   * @param obj
   * @return
   */
  final def $verify(obj:D):List[Failure] =
    ObjectPropertyLensOps.reduce(this, obj.value, FailOnBadTypes) match {
      case Failed(head, tail) => head :: tail
      case _ => Nil
    }

  final def $modify(f:this.type => D => D):D => D =
    f(this)

  final def $validModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  final def $delta(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)
}

