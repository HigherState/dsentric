package dsentric.codecs

import dsentric.{Available, Failed, Found, Raw}
import dsentric.failure.{DCodecTypeFailure, StructuralFailure}

/**
 * Short hand for codecs whos value is a Data primitive
 * @tparam T
 */
trait DirectCodec[T]  {
  this:DCodec[T] =>

  override def apply(t: T): T =
    t
}

/**
 * Short hand for codec whos value is Data primitive and can be returned matching against type
 * @tparam T
 */
trait MatchCodec[T] extends DirectCodec[T] {
  this:DCodec[T] =>

  /**
   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
   *
   * @param a
   * @return
   */
  def verify(a: Raw): List[StructuralFailure] =
    if (isMatch(a)) Nil
    else List(DCodecTypeFailure(this, a))


  /**
   * Standard type failure check, override for targeted behaviour, like NotFound if wrong type
   *
   * @param a
   * @return
   */
  def get(a: Raw): Available[T] =
    if (isMatch(a)) Found(a.asInstanceOf[T])
    else Failed(DCodecTypeFailure(this, a))

  def unapply(a:Raw):Option[T] =
    if (isMatch(a)) Some(a.asInstanceOf[T])
    else None

  protected def isMatch(a:Raw):Boolean
}
