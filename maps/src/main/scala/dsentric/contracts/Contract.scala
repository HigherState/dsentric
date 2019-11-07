package dsentric.contracts

import dsentric.operators.DataOperationOps
import dsentric._
import dsentric.failure.{EmptyOnIncorrectTypeBehaviour, FailOnIncorrectTypeBehaviour, IncorrectTypeBehaviour}

trait SubContractFor[D <: DObject]
  extends BaseContract[D]
  with PropertyOps[D]
  with PropertyObjectOps[D] {
  type Out = D

  override protected def __self: BaseContract[D] = this
}

trait ContractFor[D <: DObject]
  extends BaseContract[D]
  with PropertyOps[D]
  with PropertyObjectOps[D]
  with ContractLens[D] {

  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour =
    FailOnIncorrectTypeBehaviour

  def _path:Path = Path.empty

  val $ops:DataOperationOps[D] =
    new DataOperationOps[D](this)

  override protected def __self: BaseContract[D] = this
  def _root: ContractFor[D] = this
}

trait EmptyOnIncorrectType extends WithIncorrectTypeBehaviour {
  private[dsentric] override def __incorrectTypeBehaviour: IncorrectTypeBehaviour =
    EmptyOnIncorrectTypeBehaviour
}


trait SubContract extends SubContractFor[DObject]

trait Contract extends ContractFor[DObject] {
  def $create(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)
}


abstract class ContractTypeFor[D <: DObject](val $typeKey:String, val $keyMatcher:Matcher = ExistenceMatcher) extends ContractFor[D] {
  val $isType = new MatcherUnapply($typeKey, $keyMatcher)
}

abstract class ContractType(override val $typeKey:String, override val $keyMatcher:Matcher = ExistenceMatcher) extends ContractTypeFor[DObject]($typeKey, $keyMatcher) {
  //TODO create has matcher if possible.
  def $create():DObject = {
    val seed = $keyMatcher match {
      case ExistenceMatcher =>
        true
      case v:ValueMatcher[_]@unchecked =>
        v.default
    }
    new DObjectInst(Map($typeKey -> seed))
  }

  def $create(f:this.type => DObject => DObject):DObject =
    f(this)($create())
}
