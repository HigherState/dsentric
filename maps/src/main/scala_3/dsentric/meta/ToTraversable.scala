package dsentric.meta

import scala.collection.Factory
import scala.collection.mutable

trait DepFn1[T] {
  type Out

  def apply(t: T): Out
}

trait ToTraversable[L <: Tuple, M[_]] extends DepFn1[L] with Serializable {
  type Lub

  def builder(): mutable.Builder[Lub, M[Lub]]

  def append[LLub](l: L, b: mutable.Builder[LLub, M[LLub]], f: Lub => LLub): Unit

  type Out = M[Lub]

  def apply(l: L): Out = {
    val b = builder()
    append(l, b, identity)
    b.result()
  }
}

object ToTraversable {
  def apply[L <: Tuple, M[_]](implicit toTraversable: ToTraversable[L, M]): Aux[L, M, toTraversable.Lub] = toTraversable

  type Aux[L <: Tuple, M[_], Lub0] = ToTraversable[L, M] {type Lub = Lub0}

  implicit def hnilToTraversable[L <: EmptyTuple, M[_], T](implicit cbf: Factory[T, M[T]]): Aux[L, M, T] =
    new ToTraversable[L, M] {
      type Lub = T

      def builder() = cbf.newBuilder

      def append[LLub](l: L, b: mutable.Builder[LLub, M[LLub]], f: Lub => LLub) = {}
    }

  implicit def hnilToTraversableNothing[L <: EmptyTuple, M[_]](implicit cbf: Factory[Nothing, M[Nothing]]): Aux[L, M, Nothing] =
    hnilToTraversable[L, M, Nothing]

  implicit def hsingleToTraversable[T, M[_], Lub0](implicit ev: T <:< Lub0, cbf: Factory[Lub0, M[Lub0]]): Aux[T *: EmptyTuple, M, Lub0] =
    new ToTraversable[T *: EmptyTuple, M] {
      type Lub = Lub0

      def builder() = cbf.newBuilder

      def append[LLub](l: T *: EmptyTuple, b: mutable.Builder[LLub, M[LLub]], f: Lub0 => LLub) = {
        b += f(l.head)
      }
    }

  implicit def hlistToTraversable[H1, H2, T <: Tuple, LubT, Lub0, M[_]](implicit 
    tttvs: Aux[H2 *: T, M, LubT],
    u: Lub[H1, LubT, Lub0],
    cbf: Factory[Lub0, M[Lub0]]
  ): Aux[H1 *: H2 *: T, M, Lub0] =
    new ToTraversable[H1 *: H2 *: T, M] {
      type Lub = Lub0

      def builder() = cbf.newBuilder

      def append[LLub](l: H1 *: H2 *: T, b: mutable.Builder[LLub, M[LLub]], f: Lub0 => LLub): Unit = {
        b += f(u.left(l.head));
        tttvs.append[LLub](l.tail, b, f compose u.right)
      }
    }
}