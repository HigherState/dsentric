package dsentric.codecs.std

import dsentric.{Available, DArray, Failed, Found, NotFound, Path, Raw, RawArray}
import dsentric.codecs.{DArrayCodec, DCodec, DCollectionCodec, DirectCodec}
import dsentric.failure.{DCodecMissingElementFailure, DCodecTypeFailure, StructuralFailure}
import dsentric.schema.{ArrayDefinition, TypeDefinition}

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

class DCollectionCodecOps[S, T](val valueCodec:DCodec[T])(vectorToS:Vector[T] => S)(sToVector:S => Vector[T]) extends DCollectionCodec[S, T] {

  def verify(a: Raw): List[StructuralFailure] =
    a match {
      case raw:RawArray@unchecked =>
        raw.zipWithIndex
          .flatMap( p => valueCodec.verify(p._1).map(_.rebase(Path(p._2))))
          .toList
      case _ =>
        List(DCodecTypeFailure(this, a))
    }

  def get(a: Raw): Available[S] =
    a match {
      case raw:RawArray@unchecked =>
        raw
          .map(valueCodec.get)
          .zipWithIndex
          .foldLeft[Either[ListBuffer[StructuralFailure], VectorBuilder[T]]](Right(new VectorBuilder[T])){
            case (Right(vb), (Found(t), _)) =>
              Right(vb.addOne(t))
            case (Right(_), (Failed(head, tail), index)) =>
              Left(new ListBuffer[StructuralFailure].addAll((head :: tail).map(_.rebase(Path(index)))))
            case (Right(_), (NotFound, index)) =>
              Left(new ListBuffer[StructuralFailure].addOne(DCodecMissingElementFailure(this, Path(index))))
            case (Left(lb), (Failed(head, tail), index)) =>
              Left(lb.addAll((head :: tail).map(_.rebase(Path(index)))))
            case (Left(lb), (NotFound, index)) =>
              Left(lb.addOne(DCodecMissingElementFailure(this, Path(index))))
            case (result, _) =>
              result
          } match {
          case Right(vb) =>
            Found(vectorToS(vb.result()))
          case Left(lb) =>
            val head :: tail = lb.result()
            Failed(head, tail)
        }

      case _ =>
        Failed(DCodecTypeFailure(this, a), Nil)
    }

  def apply(t: S): RawArray = {
    valueCodec match {
      case _:DirectCodec[T] =>
        sToVector(t)
      case _ =>
        sToVector(t).map(valueCodec.apply)
    }

  }

  def unapply(a: Raw): Option[S] =
    a match {
      case s:RawArray@unchecked =>
        s.map(valueCodec.unapply).foldLeft[Option[VectorBuilder[T]]](Some(new VectorBuilder[T])){
          case (Some(vb), Some(t)) => Some(vb += t)
          case _ => None
        }.map(vb => vectorToS(vb.result()))
      case _ =>
        None
    }


  def typeDefinition: TypeDefinition =
    ArrayDefinition(items = Vector(valueCodec.typeDefinition))
}

trait DArrayCodecs {

  implicit val dArrayCodec:DArrayCodec[DArray] =
    new DArrayCodec[DArray] {
      override def apply(t: DArray): RawArray = t.value

      def unapply(a: Raw): Option[DArray] =
        a match {
          case raw: RawArray@unchecked =>
            Some(new DArray(raw))
          case _ =>
            None
        }

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def verify(a: Raw): List[StructuralFailure] =
        a match {
          case _:RawArray@unchecked =>
            Nil
          case _ =>
            List(DCodecTypeFailure(this, a))
        }

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def get(a: Raw): Available[DArray] =
        a match {
          case raw: RawArray@unchecked =>
            Found(new DArray(raw))
          case _ =>
            Failed(DCodecTypeFailure(this, a), Nil)
        }

      def typeDefinition: TypeDefinition =
        ArrayDefinition(items = Vector(TypeDefinition.anyDefinition))
  }

  implicit def tupleCodec[T1, T2](implicit T1:DCodec[T1], T2:DCodec[T2]):DArrayCodec[(T1, T2)] =
    new DArrayCodec[(T1, T2)] {
      override def apply(t: (T1, T2)): RawArray =
        Vector(T1(t._1), T2(t._2))

      def unapply(a: Raw): Option[(T1, T2)] =
        a match {
          case Vector(a1, a2) =>
            for {
              t1 <- T1.unapply(a1)
              t2 <- T2.unapply(a2)
            } yield t1 -> t2
          case _ =>
            None
        }

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def verify(a: Raw): List[StructuralFailure] =
        a match {
          case Vector(a1, a2) =>
            T1.verify(a1).map(_.rebase(Path(0))) ++
            T2.verify(a2).map(_.rebase(Path(1)))
          case _ =>
            List(DCodecTypeFailure(this, a))
        }

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def get(a: Raw): Available[(T1, T2)] =
        a match {
          case Vector(a1, a2) =>
            Available.sequence2(
              T1.get(a1).rebase(Path(0)),
              T2.get(a2).rebase(Path(1))
            )
          case _ =>
            Failed(DCodecTypeFailure(this, a))
        }


      def typeDefinition: TypeDefinition =
        ArrayDefinition(Vector(T1.typeDefinition, T2.typeDefinition))
  }

  implicit def arrayCodec[T](implicit D:DCodec[T], C:ClassTag[T]):DCollectionCodec[Array[T], T] =
    new DCollectionCodecOps(D)(_.toArray)(_.toVector)

  implicit def vectorCodec[T](implicit D:DCodec[T]):DCollectionCodec[Vector[T], T] =
    new DCollectionCodecOps(D)(identity)(identity)

  implicit def listCodec[T](implicit D:DCodec[T]):DCollectionCodec[List[T], T] =
    new DCollectionCodecOps(D)(_.toList)(_.toVector)

}
