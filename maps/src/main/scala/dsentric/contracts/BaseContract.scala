package dsentric.contracts

import dsentric.{DObject, DProjection, Path}

/**
 * Use supports mixin of AdditionalProperties: Open
 */
private[dsentric] trait BaseAux {
  type AuxD <: DObject

  def _root: ContractFor[AuxD]

  def _path: Path
}

private[dsentric] trait BaseContractAux extends BaseAux {

  def _fields: Map[String, Property[AuxD, _]]
}

/**
 * Prevents clashes with PropertyLens
 * @tparam D
 */
private[dsentric] trait ParameterisedAux[D <: DObject] extends BaseAux {
  type AuxD = D
}

private[dsentric] trait BaseContract[D <: DObject] extends BaseContractAux with ParameterisedAux[D] {

  def apply[R](f: this.type => R): R = f(this)

  final def _keys: Set[String] =
    _fields.keySet

  final def $$(projection: DProjection): DProjection =
    projection.nest(this._path)

  final def $$(paths: Path*): DProjection =
    DProjection(paths: _*).nest(this._path)
}

private[contracts] trait FieldResolver[D <: DObject] {

  private var __fields: Map[String, Property[D, Any]] = _
  @volatile
  private var _bitmap0: Boolean                       = false

  final def _fields: Map[String, Property[D, _]] =
    if (_bitmap0) __fields
    else {
      this.synchronized {
        __fields = this.getClass.getMethods.flatMap { m =>
          if (
            classOf[Property[D, _]].isAssignableFrom(
              m.getReturnType
            ) && m.getTypeParameters.isEmpty && m.getParameterTypes.isEmpty
          )
            m.invoke(this) match {
              case prop: PropertyResolver[D, Any] @unchecked =>
                Some(prop.__nameOverride.getOrElse(m.getName) -> prop)
              case _                                         =>
                None
            }
          else
            None
        }.toMap
        _bitmap0 = true
      }
      __fields
    }
}
