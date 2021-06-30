package dsentric.contracts

import dsentric.operators.DataOperationOps
import dsentric._

trait SubContractFor[D <: DObject]
  extends BaseContract[D]
    with ExpectedPropertyOps[D]
    with ExpectedPropertyObjectOps[D] {
  type Out = D

  override protected def __self: BaseContract[D] = this
}

trait ContractFor[D <: DObject]
  extends BaseContract[D]
  with ExpectedPropertyOps[D]
  with ExpectedPropertyObjectOps[D]
  with ContractLens[D] {

  def _path:Path = Path.empty

  def $sanitize:PathSetter[D] =
    DataOperationOps.sanitize(this)

  override protected def __self: BaseContract[D] = this
  def _root: ContractFor[D] = this
  def _parent: BaseContract[D] = this
}

trait SubContract extends SubContractFor[DObject]

trait Contract extends ContractFor[DObject] {
  def $create[R](f:this.type => DObject => R):R =
    f(this)(DObject.empty)
}
