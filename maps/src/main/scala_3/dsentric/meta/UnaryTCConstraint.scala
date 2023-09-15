package dsentric.meta

import dsentric.{Id, Const}

trait UnaryTCConstraint[L <: Tuple, TC[_]] extends Serializable

trait LowPriorityUnaryTCConstraint0 {
  implicit def hlistIdUnaryTC[L <: Tuple]: UnaryTCConstraint[L, Id] = new UnaryTCConstraint[L, Id] {}
}

trait LowPriorityUnaryTCConstraint extends LowPriorityUnaryTCConstraint0 {

  implicit def hnilUnaryTC[TC[_]]: UnaryTCConstraint[EmptyTuple, TC] = new UnaryTCConstraint[EmptyTuple, TC] {}

  implicit def hlistConstUnaryTC[H, T <: Tuple](implicit utct : UnaryTCConstraint[T, Const[H]#λ]): UnaryTCConstraint[H *: T, Const[H]#λ] =
    new UnaryTCConstraint[H *: T, Const[H]#λ] {}
}

object UnaryTCConstraint extends LowPriorityUnaryTCConstraint {
  def apply[L <: Tuple, TC[_]](implicit utcc: UnaryTCConstraint[L, TC]): UnaryTCConstraint[L, TC] = utcc

  type *->*[TC[_]] = {
    type λ[L <: Tuple] = UnaryTCConstraint[L, TC]
  }

  implicit def hnilConstUnaryTC[H]: UnaryTCConstraint[EmptyTuple, Const[H]#λ] =
    new UnaryTCConstraint[EmptyTuple, Const[H]#λ] {}

  implicit def hlistUnaryTC[H, T <: Tuple, TC[_]](implicit utct : UnaryTCConstraint[T, TC]): UnaryTCConstraint[TC[H] *: T, TC] =
    new UnaryTCConstraint[TC[H] *: T, TC] {}
}