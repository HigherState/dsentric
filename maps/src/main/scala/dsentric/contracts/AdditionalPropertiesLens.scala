package dsentric.contracts

import dsentric.DObject
import dsentric.failure.StructuralFailure

private[dsentric] trait AdditionalPropertyLens[D <: DObject, T] {

  def $verify(obj:D):List[StructuralFailure]

}
