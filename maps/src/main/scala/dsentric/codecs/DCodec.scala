package dsentric.codecs

import dsentric._
import dsentric.contracts.ContractFor
import dsentric.failure.{DCodecTypeFailure, Failure, StructuralFailure}
import dsentric.schema._

sealed trait DCodec[T] {
  def apply(t:T):Raw
  def unapply(a:Raw):Option[T]

  /**
   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
   * @param a
   * @return
   */
  def verify(a:Raw):List[StructuralFailure]

  def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):DeltaReduce

  /**
   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
   * @param a
   * @return
   */
  def get(a:Raw):Available[T]

  def typeDefinition:TypeDefinition

  @inline
  protected def deltaNull(deltaValue:Raw):Boolean =
    deltaValue.isInstanceOf[DNull.type]
}

trait DValueCodec[T] extends DCodec[T] {
  override def apply(t:T):RawValue

  /**
   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
   * @param a
   * @return
   */
  def verify(a:Raw):List[StructuralFailure] =
    unapply(a) match {
      case None =>
        List(DCodecTypeFailure(this, a))
      case _ =>
        Nil
    }

  /**
   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
   * @param a
   * @return
   */
  def get(a:Raw):Available[T] =
    unapply(a) match {
      case None =>
        Failed(DCodecTypeFailure(this, a))
      case Some(t) =>
        Found(t)
    }

  def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):DeltaReduce =
    if (deltaNull(deltaValue)){
      if (currentValue.isEmpty || currentValue.contains(deltaValue))
        DeltaEmpty
      else
        DeltaReduced(deltaValue)
    }
    else verify(deltaValue) match {
      case head :: tail =>
        DeltaFailed(head, tail)
      case _ =>
        DeltaReduced(deltaValue)
    }

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

trait DObjectCodec[T] extends DCodec[T] {

  override def apply(t:T):RawObject

  // Allows us to support NotFound if wrong type behaviours
  // Should not validate the object in entirety
  def getForTraversal(a: Raw): Available[RawObject] =
    a match {
      case m:RawObject@unchecked =>
        Found(m)
      case _ =>
        Failed(DCodecTypeFailure(this, a))
    }
}

/**
 * Default definition for values stored as Arrays, we
 * dont use HigherKinded type as S may be any custom type
 * @tparam S
 * @tparam T
 */
trait DArrayCodec[T] extends DCodec[T] {
  override def apply(t:T):RawArray

  def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):DeltaReduce =
    if (deltaNull(deltaValue)){
      if (currentValue.isEmpty || currentValue.contains(deltaValue))
        DeltaEmpty
      else
        DeltaReduced(deltaValue)
    }
    else verify(deltaValue) match {
      case head :: tail =>
        DeltaFailed(head, tail)
      case _ =>
        DeltaReduced(deltaValue)
    }
}

trait DMapCodec[K, T] extends DObjectCodec[Map[K, T]] {

  def keyCodec:DStringCodec[K]
  def valueCodec:DCodec[T]
}

trait DCollectionCodec[S, T] extends DArrayCodec[S] {
  def valueCodec:DCodec[T]
}


trait DContractCodec[D <: DObject] extends DObjectCodec[D] {
  def contract:ContractFor[D]
}

trait DCoproductCodec[T] extends DCodec[T]

object DataCodec extends DCodec[Data]{
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
  def verify(a: Raw): List[StructuralFailure] = Nil

  def verifyAndReduceDelta(deltaValue:Raw, currentValue:Option[Raw]):DeltaReduce =
    (deltaValue -> currentValue) match {
      case (deltaObject: RawObject@unchecked, Some(currentObject:RawObject@unchecked)) =>
        DeltaReduced(RawObjectOps.rightDifferenceReduceMap(currentObject, deltaObject))
      case (deltaObject: RawObject@unchecked, _) =>
        DeltaReduced(RawObjectOps.reduceMap(deltaObject))
      case (d, Some(v)) if d == v =>
        DeltaEmpty
      case (DNull, None) =>
        DeltaEmpty
      case (d, _) =>
        DeltaReduced(d)
    }

  /**
   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
   *
   * @param a
   * @return
   */
  def get(a: Raw): Available[Data] =
    a match {
      case a:RawObject@unchecked =>
        Found(new DObjectInst(a))
      case v:RawArray@unchecked =>
        Found(new DArray(v))
      case DNull =>
        Found(DNull)
      case j =>
        Found(new DValue(j))
    }

  def typeDefinition: TypeDefinition =
    TypeDefinition.anyDefinition
}
