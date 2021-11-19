package dsentric.contracts

import dsentric._

trait SubContractFor[D <: DObject] extends BaseContract[D] with ExpectedPropertyOps[D] with ExpectedPropertyObjectOps[D] {
  type Out = D

  override protected def __self: BaseContract[D] = this
}

trait MaybeSubContractFor[D <: DObject]
    extends BaseContract[D]
    with MaybeExpectedPropertyOps[D]
    with MaybeExpectedPropertyObjectOps[D] {
  type Out = D

  override protected def __self: BaseContract[D] = this
}

// Abstracted to include Aspects and Contracts.
trait ContractLike[D <: DObject]
  extends BaseContract[D]
    with ExpectedPropertyOps[D]
    with ExpectedPropertyObjectOps[D]
    with ContractLens[D] {

  def _path: Path = Path.empty

  override protected def __self: BaseContract[D] = this
  def _parent: BaseContract[D]                   = this
}

trait ContractFor[D <: DObject]
    extends ContractLike[D]
    with FieldResolver[D] {

  def _root: ContractFor[D]                      = this
}

trait SubContract extends SubContractFor[DObject]

trait MaybeSubContract extends MaybeSubContractFor[DObject]

trait Contract extends ContractFor[DObject] {
  def $create[R](f: this.type => DObject => R): R =
    f(this)(DObject.empty)
}
