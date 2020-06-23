package dsentric.failure

import cats.data.NonEmptyList
import dsentric.{DObject, Path}
import dsentric.contracts.ContractFor

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ListBuffer

object ValidResult {

  def failure[T](failure:Failure, additional:Failure*):ValidResult[T] =
    Left(NonEmptyList(failure, additional.toList))

  def success[T](t:T):ValidResult[T] =
    Right(t)

  val none: ValidResult[None.type] = ValidResult.success(None)

  //TODO replace when cats 2.0.0 comes
  def parSequence[F <: Failure, T](v:Vector[Either[NonEmptyList[F], T]]):Either[NonEmptyList[F], Vector[T]] = {
    val lb = new ListBuffer[F]
    val vb = new VectorBuilder[T]
    v.foreach{
      case Right(e) =>
        if (lb.isEmpty) vb += e
      case Left(v) =>
        lb.appendAll(v.toList)
    }
    if (lb.nonEmpty) {
      val l = lb.result()
      Left(NonEmptyList(l.head, l.tail))
    }
    else {
      Right(vb.result())
    }
  }

  def sequence2[S, T](s:ValidResult[S], t:ValidResult[T]):ValidResult[(S, T)] = {
    (s, t) match {
      case (Right(sv), Right(tv)) =>
        Right(sv -> tv)
      case (Left(sf), Left(tf)) =>
        Left(sf ++ tf.toList)
      case (Left(sf), _) =>
        Left(sf)
      case (_, Left(tf)) =>
        Left(tf)
    }
  }
}
