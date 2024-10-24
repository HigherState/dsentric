package dsentric.contracts

import dsentric.{
  Available,
  DObject,
  DObjectOps,
  Delta,
  DeltaEmpty,
  DeltaFailed,
  DeltaReduce,
  DeltaReduced,
  DeltaRemove,
  DeltaRemoving,
  Failed,
  Found,
  NotFound,
  RawObject,
  Valid,
  Validated
}
import dsentric.failure.{Failure, ValidResult}
import dsentric.operators.DataOperationOps

trait ContractLens[D <: DObject] { this: BaseContract[D] =>

  def _fields: Map[String, Property[D, ?]]

  //Currently not supportive of additional properties in D Constructor when nested
  private[contracts] def __get(obj: RawObject, dropBadTypes: Boolean, setDefaultValues: Boolean): Valid[RawObject] = {
    val badTypes = if (dropBadTypes) DropBadTypes else FailOnBadTypes
    GetOps.get(this, obj, badTypes, setDefaultValues)
  }
  private[contracts] def __verify(obj: RawObject): List[Failure] =
    VerifyOps.verify(this, obj)
  private[contracts] def __reduce(obj: RawObject, dropBadTypes: Boolean = false): Available[RawObject] = {
    val badTypes = if (dropBadTypes) DropBadTypes else FailOnBadTypes
    ReduceOps.reduce(this, obj, badTypes)
  }
  private[contracts] def __reduceDelta(
    delta: RawObject,
    current: RawObject,
    dropBadTypes: Boolean = false
  ): DeltaReduce[RawObject] = {
    val badTypes = if (dropBadTypes) DropBadTypes else FailOnBadTypes
    DeltaReduceOps.deltaReduce(this, delta, current, badTypes)
  }

  /**
   * Returns object against the contract, verifying the structural integrity of the object as well as
   * removing empty Objects and Null values.
   * Also validates against constraints.
   * @param obj
   * @return
   */
  final def $reduce[D2 <: D](obj: D2, dropBadTypes: Boolean = false): ValidResult[D2] =
    __reduce(obj.value, dropBadTypes) match {
      case Found(raw)         =>
        ValidResult.success(obj.internalWrap(raw).asInstanceOf[D2])
      case NotFound           =>
        ValidResult.success(obj.internalWrap(RawObject.empty).asInstanceOf[D2])
      case Failed(head, tail) =>
        ValidResult.failure(head, tail)
    }

  /**
   * Reduces the object and then lifts as Validated
   * @param obj
   * @param dropBadTypes
   * @tparam D2
   * @return
   */
  final def $validated[D2 <: D](obj: D2, dropBadTypes: Boolean = false): ValidResult[Validated[D2]] =
    $reduce(obj, dropBadTypes).map(d2 => Validated(d2))

  /**
   * Verifies the object, this doesnt alter it in anyway and constraints are not checked against.
   */
  final def $verify[D2 <: D](obj: D2): ValidResult[Validated[D2]] =
    __verify(obj.value) match {
      case head :: tail =>
        ValidResult.failure(head, tail)
      case Nil          =>
        ValidResult.success(Validated(obj))
    }

  final def $reduceDelta[DD <: Delta & DObjectOps[DD]](
    obj: D,
    delta: DD,
    dropBadTypes: Boolean = false
  ): ValidResult[DD] =
    __reduceDelta(delta.value, obj.value, dropBadTypes) match {
      case DeltaEmpty | DeltaRemove     =>
        ValidResult.success(delta.internalWrap(RawObject.empty))
      case DeltaReduced(deltaReduced)   =>
        ValidResult.success(delta.internalWrap(deltaReduced))
      case DeltaRemoving(deltaRemoving) =>
        ValidResult.success(delta.internalWrap(deltaRemoving))
      case DeltaFailed(head, tail)      =>
        ValidResult.failure(head, tail)
    }

  final def $get[D2 <: D](obj: D2, dropBadTypes: Boolean = false, setDefaultValues: Boolean = true): ValidResult[D2] =
    __get(obj.value, dropBadTypes, setDefaultValues) match {
      case Found(raw)         =>
        ValidResult.success(obj.internalWrap(raw).asInstanceOf[D2])
      case Failed(head, tail) =>
        ValidResult.failure(head, tail)
    }

  final def $modify(f: this.type => D => D): D => D =
    f(this)

  final def $validModify(f: this.type => D => ValidResult[D]): D => ValidResult[D] =
    f(this)

  final lazy val $sanitize: PathSetter[D] =
    DataOperationOps.sanitize(this)
}
