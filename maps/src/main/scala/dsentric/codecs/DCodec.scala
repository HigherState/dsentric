package dsentric.codecs

import dsentric._
import dsentric.contracts.Contract
import dsentric.failure.Failure
import dsentric.schema._
import shapeless.HList
import shapeless.UnaryTCConstraint.*->*
import shapeless.ops.hlist.ToTraversable

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable

sealed trait DCodec[T] {
  def apply(t:T):Raw
  def unapply(a:Raw):Option[T]

//  /**
//   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
//   * @param a
//   * @return
//   */
//  def verify(a:Raw):List[StructuralFailure]
//
//  def verifyAndReduce(value:Raw):Option[Raw]
//
//  def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):DeltaReduce[Raw]
//

  def typeDefinition:TypeDefinition

  @inline
  protected def deltaNull(deltaValue:Raw):Boolean =
    deltaValue.isInstanceOf[DNull.type]
}

trait DValueCodec[T] extends DCodec[T] {
  override def apply(t:T):RawValue

//  /**
//   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
//   * @param a
//   * @return
//   */
//  def verify(a:Raw):List[StructuralFailure] =
//    unapply(a) match {
//      case None =>
//        List(DCodecTypeFailure(this, a))
//      case _ =>
//        Nil
//    }
//
//  def verifyAndReduceDelta(value:Raw):Option[Raw] =
//    deltaValue

//  def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):DeltaReduce[Raw] =
//    if (deltaNull(deltaValue)){
//      if (currentValue.isEmpty)
//        DeltaEmpty
//      else
//        DeltaRemove
//    }
//    else verify(deltaValue) match {
//      case head :: tail =>
//        DeltaFailed(head, tail)
//      case _ =>
//        DeltaReduced(deltaValue)
//    }

}

trait DStringCodec[T] extends DValueCodec[T] {
  override def apply(t:T):String

  def unapply(a:Raw): Option[T] =
    a match {
      case s:String =>
        fromString(s)
      case _ =>
        None
    }

  def fromString(s:String):Option[T]

  def typeDefinition:StringDefinition
}

trait DMapCodec[M, K, T] extends DCodec[M] {

  def keyCodec:DStringCodec[K]
  def valueCodec:DCodec[T]

  def build(m:Map[K, T]):M

  def extract(m:M):Map[K,T]

  def apply(t: M): RawObject =
    extract(t).map(p => keyCodec(p._1) -> valueCodec(p._2))

  def unapply(a: Raw): Option[M] =
    a match {
      case s:RawObject@unchecked =>
        s.map(p => keyCodec.unapply(p._1) -> valueCodec.unapply(p._2))
          .foldLeft[Option[mutable.Builder[(K, T), Map[K, T]]]](Some(Map.newBuilder[K, T])){
          case (Some(mb), (Some(k), Some(v))) =>
            Some(mb.addOne(k -> v))
          case _ =>
            None
        }.map(mb => build(mb.result()))
      case _ =>
        None
    }

  def typeDefinition: TypeDefinition = ???
}

trait DCollectionCodec[S, T] extends DCodec[S] {
  def valueCodec:DCodec[T]

  def build(t:Vector[T]):S

  def extract(s:S):Vector[T]

  def apply(t: S): RawArray = {
    valueCodec match {
      case _:DirectCodec[T] =>
        extract(t)
      case _ =>
        extract(t).map(valueCodec.apply)
    }
  }

  def unapply(a: Raw): Option[S] =
    a match {
      case s:RawArray@unchecked =>
        s.map(valueCodec.unapply).foldLeft[Option[VectorBuilder[T]]](Some(new VectorBuilder[T])){
          case (Some(vb), Some(t)) => Some(vb += t)
          case _ => None
        }.map(vb => build(vb.result()))
      case _ =>
        None
    }

  def typeDefinition: TypeDefinition =
    ArrayDefinition(items = Vector(valueCodec.typeDefinition))
}

case class DContractCodec(contract:Contract) extends DCodec[DObject] {

  def unapply(a: Raw): Option[DObject] =
    a match {
      case m:RawObject@unchecked =>
        //TODO could be a disconnect between unapply in ObjectLens
        contract.$reduce(new DObjectInst(m)).toOption
      case _ =>
        None
    }

  def apply(t: DObject): Raw =
    t.value

  def typeDefinition: TypeDefinition = ???
}

case class DTypeContractCodec(typeDefinition: TypeDefinition)(val contracts:PartialFunction[DObject, Contract]) extends DCodec[DObject] {

  def this(contracts:PartialFunction[DObject, Contract]) =
    this(ObjectDefinition.empty)(contracts)

  def unapply(a: Raw): Option[DObject] =
    a match {
      case m:RawObject@unchecked =>
        contracts
          .lift(new DObjectInst(m))
          .flatMap(_.$reduce(new DObjectInst(m)).toOption)
      case _ =>
        None
    }

  def apply(t: DObject): Raw =
    t.value
}

abstract class DCoproductCodec[T, H <: HList : *->*[DCodec]#λ](val codecs:H)(implicit T:ToTraversable.Aux[H, List, DCodec[_]]) extends DCodec[T] {
  def codecsList: List[DCodec[_]] = codecs.toList

  def lift[A](a:A, codec:DCodec[A]):Option[T]
}

object DataCodec extends DValueCodec[Data]{
  def apply(t: Data): Raw =
    t.value

  def unapply(a: Raw): Option[Data] =
    a match {
      case a:RawObject@unchecked =>
        Some(new DObjectInst(a))
      case v:RawArray@unchecked =>
        Some(new DArray(v))
      case DNull =>
        Some(DNull)
      case j =>
        Some(new DValue(j))
    }

  /**
   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
   *
   * @param a
   * @return
   */
  def verify(a: Raw): List[Failure] = Nil

  def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):DeltaReduce[Raw] =
    (deltaValue -> currentValue) match {
      case (deltaObject: RawObject@unchecked, Some(currentObject:RawObject@unchecked)) =>
        DeltaReduced(RawObjectOps.rightDifferenceReduceMap(currentObject, deltaObject))
      case (deltaObject: RawObject@unchecked, _) =>
        DeltaReduced(RawObjectOps.reduceMap(deltaObject))
      case (d, Some(v)) if d == v =>
        DeltaEmpty
      case (DNull, None) =>
        DeltaEmpty
      case (DNull, _) =>
        DeltaRemove
      case (d, _) =>
        DeltaReduced(d)
    }


  def typeDefinition: TypeDefinition =
    TypeDefinition.anyDefinition
}

object DValueCodec {
  def literal[T](t:T)(implicit D:DValueCodec[T]):DValueCodec[T] =
   new DValueCodec[T] {
     private val rawT:Raw = D.apply(t)
     override def apply(t: T): RawValue =
       rawT

     def unapply(a: Raw): Option[T] =
       if (a == rawT) Some(t)
       else None

     def typeDefinition: TypeDefinition = ???
   }
}