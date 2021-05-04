package dsentric.codecs.std

import dsentric._
import dsentric.codecs.{DCodec, DMapCodec, DObjectCodec, DStringCodec, DirectCodec}
import dsentric.failure.{DCodecMissingElementFailure, DCodecTypeFailure, DCodecUnexpectedValueFailure, StructuralFailure}
import dsentric.schema.{ObjectDefinition, TypeDefinition}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait DObjectCodecs {

  implicit val dObjectCodec:DObjectCodec[DObject] =
    new DObjectCodec[DObject]{
    override def apply(t: DObject): RawObject = t.value

    def unapply(a: Raw): Option[DObject] =
      a match {
        case raw: RawObject@unchecked =>
          Some(new DObjectInst(raw))
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
        case _: RawObject@unchecked =>
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
    def get(a: Raw): Available[DObject] =
      a match {
        case raw: RawObject@unchecked =>
          Found(new DObjectInst(raw))
        case _ =>
          Failed(DCodecTypeFailure(this, a), Nil)
      }

    def typeDefinition: TypeDefinition =
      ObjectDefinition.empty
  }

  implicit def stringValueMapCodec[T](implicit T:DCodec[T]):DMapCodec[String, T] =
    new DMapCodec[String, T]  {

      def valueCodec: DCodec[T] = T

      def keyCodec: DStringCodec[String] =
        DValueCodecs.stringCodec

      def verify(a: Raw): List[StructuralFailure] =
        a match {
          case raw: RawObject@unchecked =>
            raw
              .flatMap( p => valueCodec.verify(p._2).map(_.rebase(Path(p._1))))
              .toList
          case _ =>
            List(DCodecTypeFailure(this, a))
        }

      def unapply(a: Raw): Option[Map[String, T]] =
        a match {
          case raw:RawObject@unchecked =>
            raw.view.mapValues(valueCodec.unapply)
              .foldLeft(Option(Map.newBuilder[String, T])){
                case (Some(mb), (k, Some(t))) => Some(mb += (k -> t))
                case _ => None
              }.map(_.result())
          case _ =>
            None
        }

      def get(a: Raw): Available[Map[String, T]] =
        a match {
          case raw:RawObject@unchecked =>
            raw.view.mapValues(valueCodec.get)
              .foldLeft[Either[ListBuffer[StructuralFailure], mutable.Builder[(String, T), Map[String, T]]]](Right(Map.newBuilder[String, T])){
                case (Right(mb), (key, Found(t))) =>
                  Right(mb.addOne(key -> t))
                case (Right(_), (key, Failed(head, tail))) =>
                  Left(new ListBuffer[StructuralFailure].addAll((head :: tail).map(_.rebase(Path(key)))))
                case (Right(_), (key, NotFound)) =>
                  Left(new ListBuffer[StructuralFailure].addOne(DCodecMissingElementFailure(this, Path(key))))
                case (Left(lb), (key, Failed(head, tail))) =>
                  Left(lb.addAll((head :: tail).map(_.rebase(Path(key)))))
                case (Left(lb), (key, NotFound)) =>
                  Left(lb.addOne(DCodecMissingElementFailure(this, Path(key))))
                case (result, _) =>
                  result
              } match {
              case Right(mb) =>
                Found(mb.result())
              case Left(lb) =>
                val head :: tail = lb.result()
                Failed(head, tail)
            }

          case _ =>
            Failed(DCodecTypeFailure(this, a), Nil)
        }

      def apply(t: Map[String, T]): RawObject =
        valueCodec match {
          case _:DirectCodec[T]@unchecked =>
            t
          case _ =>
            t.view.mapValues(valueCodec.apply).toMap
        }

      def typeDefinition: TypeDefinition = ???
    }

  implicit def keyValueMapCodec[K, V](implicit K:DStringCodec[K], V:DCodec[V]):DMapCodec[K, V] =
    new DMapCodec[K, V]{
      def valueCodec: DCodec[V] = V

      def keyCodec: DStringCodec[K] = K

      def verify(a: Raw): List[StructuralFailure] =
        a match {
          case raw: RawObject@unchecked =>
            raw
              .flatMap( p => keyCodec.verify(p._1) ++ valueCodec.verify(p._2).map(_.rebase(Path(p._1))))
              .toList
          case _ =>
            List(DCodecTypeFailure(this, a))
        }

      def unapply(a: Raw): Option[Map[K, V]] =
        a match {
          case raw:RawObject@unchecked =>
            raw.view.map(p => keyCodec.unapply(p._1) -> valueCodec.unapply(p._2))
              .foldLeft(Option(Map.newBuilder[K, V])){
                case (Some(mb), (Some(k), Some(t))) => Some(mb.addOne(k -> t))
                case _ => None
              }.map(_.result())
          case _ =>
            None
        }

      def get(a: Raw): Available[Map[K, V]] =
        a match {
          case raw:RawObject@unchecked =>
            raw.view.map { p =>
              Available.sequence2(keyCodec.get(p._1), valueCodec.get(p._2).rebase(Path(p._1)).failNotFound(DCodecMissingElementFailure(this, Path(p._1))))
            }
              .foldLeft[Either[ListBuffer[StructuralFailure], mutable.Builder[(K, V), Map[K, V]]]](Right(Map.newBuilder[K, V])){
                case (Right(mb), Found(pair)) =>
                  Right(mb.addOne(pair))
                case (Right(_), Failed(head, tail)) =>
                  Left(new ListBuffer[StructuralFailure].addAll(head :: tail))
                case (Right(_), NotFound) =>
                  Left(new ListBuffer[StructuralFailure].addOne(DCodecMissingElementFailure(this, Path.empty)))
                case (Left(lb), Failed(head, tail)) =>
                  Left(lb.addAll(head :: tail))
                case (Left(lb), NotFound) =>
                  Left(lb.addOne(DCodecMissingElementFailure(this, Path.empty)))
                case (result, _) =>
                  result
              } match {
              case Right(mb) =>
                Found(mb.result())
              case Left(lb) =>
                val head :: tail = lb.result()
                Failed(head, tail)
            }

          case _ =>
            Failed(DCodecTypeFailure(this, a), Nil)
        }

      def apply(t: Map[K, V]): RawObject =
        t.map(p => K.apply(p._1) -> V.apply(p._2))

      def typeDefinition: TypeDefinition = ???
    }

  implicit def setCodec[T](implicit D:DStringCodec[T]):DObjectCodec[Set[T]] =
    new DObjectCodec[Set[T]] {
      def apply(t: Set[T]): RawObject =
        t.map(D.apply(_) -> 1).toMap

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def verify(a: Raw): List[StructuralFailure] =
        a match {
          case obj:RawObject@unchecked =>
            obj.flatMap(p =>
              D.verify(p._1) ++ (
                if (p._2 != 1) List(DCodecUnexpectedValueFailure(this, Path(p._1), 1, p._2))
                else Nil
              )
            ).toList
          case _ =>
            List(DCodecTypeFailure(this, a))
        }

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def get(a: Raw): Available[Set[T]] =
        a match {
          case raw:RawObject@unchecked =>
            raw.view.map { p =>
              val v =
                if (p._2 != 1) Failed(DCodecUnexpectedValueFailure(this, Path(p._1), 1, p._2))
                else Found(())
              Available.sequence2(D.get(p._1), v)
            }
              .foldLeft[Either[ListBuffer[StructuralFailure], mutable.Builder[T, Set[T]]]](Right(Set.newBuilder[T])){
                case (Right(mb), Found((value, _))) =>
                  Right(mb.addOne(value))
                case (Right(_), Failed(head, tail)) =>
                  Left(new ListBuffer[StructuralFailure].addAll(head :: tail))
                case (Right(_), NotFound) =>
                  Left(new ListBuffer[StructuralFailure].addOne(DCodecMissingElementFailure(this, Path.empty)))
                case (Left(lb), Failed(head, tail)) =>
                  Left(lb.addAll(head :: tail))
                case (Left(lb), NotFound) =>
                  Left(lb.addOne(DCodecMissingElementFailure(this, Path.empty)))
                case (result, _) =>
                  result
              } match {
              case Right(mb) =>
                Found(mb.result())
              case Left(lb) =>
                val head :: tail = lb.result()
                Failed(head, tail)
            }
        }

      def unapply(a: Raw): Option[Set[T]] =
        a match {
          case obj:RawObject@unchecked =>
            obj.foldLeft(Option(Set.empty[T])){
              case (Some(a), (key, 1)) =>
                D.unapply(key).map(a + _)
              case _ =>
                None
            }
          case _ =>
            None
        }

      def typeDefinition: TypeDefinition = ???
    }

  /**
   * TODO proper validation
   */
  implicit val dQueryCodec:DObjectCodec[DQuery] =
    new DObjectCodec[DQuery] {
      def unapply(a:Raw): Option[DQuery] =
        a match {
          case a:RawObject@unchecked =>
           Some(DQuery(a))
          case _ =>
            None
        }
      def apply(t: DQuery): RawObject =
        t.value


      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def verify(a: Raw): List[StructuralFailure] =
        a match {
          case a:RawObject@unchecked =>
            Nil
          case raw =>
            List(DCodecTypeFailure(this, raw))
        }

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def get(a: Raw): Available[DQuery] =
        a match {
          case a:RawObject@unchecked =>
            Found(DQuery(a))
          case raw =>
            Failed(DCodecTypeFailure(this, raw))
        }

      def typeDefinition:TypeDefinition =
        ObjectDefinition.empty
    }

  /**
   * TODO proper validation
   */
  implicit val dProjectionCodec:DObjectCodec[DProjection] =
    new DObjectCodec[DProjection] {
      def unapply(a:Raw): Option[DProjection] =
        a match {
          case a:RawObject@unchecked =>
            Some(new DProjection(a))
          case _ =>
            None
        }
      def apply(t: DProjection): RawObject =
        t.value


      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def verify(a: Raw): List[StructuralFailure] =
        a match {
          case _:RawObject@unchecked =>
            Nil
          case raw =>
            List(DCodecTypeFailure(this, raw))
        }

      /**
       * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
       *
       * @param a
       * @return
       */
      def get(a: Raw): Available[DProjection] =
        a match {
          case a:RawObject@unchecked =>
            Found(new DProjection(a))
          case raw =>
            Failed(DCodecTypeFailure(this, raw))
        }

      def typeDefinition:TypeDefinition =
        ObjectDefinition.empty
    }
}
