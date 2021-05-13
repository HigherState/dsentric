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
trait MatchCodec[T] extends DirectCodec[T] with DValueCodec[T] {

  def unapply(a:Raw):Option[T] =
    if (isMatch(a)) Some(a.asInstanceOf[T])
    else None

  protected def isMatch(a:Raw):Boolean
}
