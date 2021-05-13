package dsentric.contracts

import cats.data.NonEmptyList
import dsentric.{DObject, Delta}
import dsentric.failure.{StructuralFailure, ValidResult, ValidStructural}

trait ContractLens[D <: DObject] { this:ContractFor[D] =>

  def _fields: Map[String, Property[D, _]]

  /**
   * Returns object against the contract, verifying the structural integrity of the object
   * @param obj
   * @return
   */
  final def $get(obj:D, dropBadTypes:Boolean = false):ValidStructural[D] =
    ObjectLens.propertyVerifier(this, obj.value) match {
      case head :: tail => Left(NonEmptyList(head, tail))
      case Nil => Right(obj)
    }

  /**
   * Returns object against the contract, verifying the structural integrity of the object as well as
   * removing empty Objects and Null values.
   * @param obj
   * @return
   */
  final def $reduce(obj:D, dropBadTypes:Boolean = false):ValidStructural[D] =
    ObjectLens.propertyVerifier(this, obj.value) match {
      case head :: tail => Left(NonEmptyList(head, tail))
      case Nil => Right(obj)
    }

  final def $reduceDelta(obj:D, delta:Delta, dropBadTypes:Boolean = false):ValidResult[Delta] =
    ???


  /**
   * Verifies the structural integrity of the object against the contract
   * @param obj
   * @return
   */
  final def $verify(obj:D):List[StructuralFailure] =
    ObjectLens.propertyVerifier(this, obj.value)

  final def $modify(f:this.type => D => D):D => D =
    f(this)

  final def $validModify(f:this.type => D => ValidResult[D]):D => ValidResult[D] =
    f(this)

  final def $delta(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)
}

