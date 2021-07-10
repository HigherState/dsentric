package dsentric.contracts

import dsentric._
import dsentric.operators.DataOperationOps

trait SubContractFor[D <: DObject] extends BaseContract[D] with ExpectedPropertyOps[D] with ExpectedPropertyObjectOps[D] {
  type Out = D

  override protected def __self: BaseContract[D] = this
}

trait ContractFor[D <: DObject]
    extends BaseContract[D]
    with ExpectedPropertyOps[D]
    with ExpectedPropertyObjectOps[D]
    with FieldResolver[D]
    with ContractLens[D] {

  def _path: Path = Path.empty

  override protected def __self: BaseContract[D] = this
  def _root: ContractFor[D]                      = this
  def _parent: BaseContract[D]                   = this

  final def $sanitize: PathSetter[D] =
    DataOperationOps.sanitize(this)
}

trait SubContract extends SubContractFor[DObject]

trait Contract extends ContractFor[DObject] {
  def $create[R](f: this.type => DObject => R): R =
    f(this)(DObject.empty)
}
