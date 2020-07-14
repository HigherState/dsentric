package dsentric.contracts

import dsentric.failure.IncorrectTypeBehaviour
import dsentric.{DObject, DProjection, Path}

private[dsentric] trait WithIncorrectTypeBehaviour {
  private[dsentric] def __incorrectTypeBehaviour:IncorrectTypeBehaviour
}

private[dsentric] trait BaseContractAux extends WithIncorrectTypeBehaviour {
  type AuxD <: DObject

  def _root: ContractFor[AuxD]

  def _path:Path

  def _fields: Map[String, Property[AuxD, _]]
}

private[dsentric] trait BaseContract[D <: DObject] extends BaseContractAux {
  type AuxD = D
  private var __fields:Map[String, Property[D, Any]] = _
  @volatile
  private var _bitmap0:Boolean = false

  def apply[R](f:this.type => R):R = f(this)

  final def _fields: Map[String, Property[D, _]] =
    if (_bitmap0) __fields
    else {
      this.synchronized{
        __fields = this.getClass.getMethods.flatMap { m =>
          if (classOf[Property[D, _]].isAssignableFrom(m.getReturnType) && m.getTypeParameters.isEmpty && m.getParameterTypes.isEmpty)
            m.invoke(this) match {
              case prop: Property[D, Any]@unchecked =>
                Some(prop.__nameOverride.getOrElse(m.getName) -> prop)
              case _ =>
                None
            }
          else
            None
        }.toMap
        _bitmap0 = true
      }
      __fields
    }
  final def _keys:Set[String] =
    _fields.keySet

  final def $$(projection:DProjection):DProjection =
    projection.nest(this._path)

  final def $$(paths:Path*):DProjection =
    DProjection(paths:_*).nest(this._path)
}
