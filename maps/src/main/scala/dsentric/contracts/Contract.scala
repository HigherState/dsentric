package dsentric.contracts

import dsentric.{DObject, Path}

trait SubContractFor[D <: DObject]
  extends BaseContract[D]
  with PropertyOps[D]
  with PropertyObjectOps[D] {
  type Out = D
}

trait ContractFor[D <: DObject]
  extends BaseContract[D]
  with PropertyOps[D]
  with PropertyObjectOps[D] {

  def _path:Path = Path.empty
}

trait SubContract extends SubContractFor[DObject]

trait Contract extends ContractFor[DObject] {
  def $create(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)
}
