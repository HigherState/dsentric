package dsentric.filter

//import com.github.ghik.silencer.silent
import dsentric.{DArray, DObject, Data, Path, PathLensOps}
import dsentric.codecs.{DCollectionCodec, DataCodec}
import dsentric.contracts.{DynamicProperty, ExpectedProperty, MaybeProperty, Property}
import dsentric.operators.Optionable

import scala.annotation.nowarn
import scala.util.matching.Regex

sealed trait PropertyFilterOps[T] extends Any {
  def prop: Property[_, T]
  protected def nest(value: Any): DFilter =
    new DFilter(PathLensOps.pathToMap(prop._path, value))

}

trait DFilterSyntax {

  implicit def toValueFilterOps[T](prop: Property[_, T]): ValueFilterOps[T] =
    new ValueFilterOps(prop)

  implicit def toMaybeFilterOps[T](prop: MaybeProperty[_, T]): ExistsFilterOps[T] =
    new ExistsFilterOps(prop)

  implicit def toEmptyFilterOps[T](prop: DynamicProperty[_, T]): ExistsFilterOps[T] =
    new ExistsFilterOps(prop)

  implicit def toNumericFilterOps[T >: dsentric.operators.Numeric](prop: Property[_, T]): NumericFilterOps[T] =
    new NumericFilterOps(prop)

  implicit def toStringFilterOps[T >: Optionable[String]](prop: Property[_, T]): StringFilterOps[T] =
    new StringFilterOps(prop)

  implicit def toIterableFilterOps[R <: DObject, T, C[_] <: Iterable[_]](
    prop: Property[R, C[T]]
  ): IterableFilterOps[R, T, C] =
    new IterableFilterOps(prop)

  implicit def toDArrayFilterOps[R <: DObject](prop: ExpectedProperty[R, DArray]): DArrayFilterOps[R] =
    new DArrayFilterOps(prop)

}

final class IterableFilterOps[R <: DObject, T, C[_] <: Iterable[_]](val prop: Property[R, C[T]])
    extends AnyVal
    with PropertyFilterOps[C[T]] {

  def $elemMatch(f: Property[R, T] => DFilter): DFilter = {
    val codec = prop._codec.asInstanceOf[DCollectionCodec[C[T], T]].valueCodec
    nest(Map("$elemMatch" -> f(DynamicProperty(codec, "", Path.empty, prop._root)).value))
  }
}

final class DArrayFilterOps[R <: DObject](val prop: ExpectedProperty[R, DArray])
    extends AnyVal
    with PropertyFilterOps[DArray] {

  def $elemMatch(f: Property[R, Data] => DFilter): DFilter =
    nest(Map("$elemMatch" -> f(DynamicProperty(DataCodec, "", Path.empty, prop._root)).value))

}
final class ValueFilterOps[T](val prop: Property[_, T]) extends AnyVal with PropertyFilterOps[T] {

  def $eq(value: T): DFilter    =
    if (prop._path.isEmpty)
      new DFilter(Map("$eq" -> prop._codec(value)))
    else
      nest(prop._codec(value))
  //Currently not supporting chaining of $ne in an && for the same field
  def $ne(value: T): DFilter    =
    nest(Map("$ne" -> prop._codec(value)))
  def $in(values: T*): DFilter  =
    nest(Map("$in" -> values.map(prop._codec(_)).toVector))
  def $nin(values: T*): DFilter =
    nest(Map("$nin" -> values.map(prop._codec(_)).toVector))

}

final class ExistsFilterOps[T](val prop: Property[_, T]) extends AnyVal with PropertyFilterOps[T] {
  def $exists(value: Boolean): DFilter = nest(Map("$exists" -> value))
}

final class NumericFilterOps[T >: dsentric.operators.Numeric](val prop: Property[_, T])
    extends AnyVal
    with PropertyFilterOps[T] {

  def $lt(value: Double): DFilter = nest(Map("$lt" -> value))
  def $lt(value: Long): DFilter   = nest(Map("$lt" -> value))

  def $gt(value: Double): DFilter = nest(Map("$gt" -> value))
  def $gt(value: Long): DFilter   = nest(Map("$gt" -> value))

  def $lte(value: Double): DFilter = nest(Map("$lte" -> value))
  def $lte(value: Long): DFilter   = nest(Map("$lte" -> value))

  def $gte(value: Double): DFilter = nest(Map("$gte" -> value))
  def $gte(value: Long): DFilter   = nest(Map("$gte" -> value))
}

final class StringFilterOps[T >: Optionable[String]](val prop: Property[_, T])
    extends AnyVal
    with PropertyFilterOps[T] {

  def $regex(value: String): DFilter                  = nest(Map("$regex" -> value))
  @nowarn("msg=possible missing interpolator")
  def $regex(value: String, options: String): DFilter =
    nest(Map("$regex" -> value, "$options" -> options))
  def $regex(r: Regex): DFilter                       = nest(Map("$regex" -> r.regex))
  def $like(value: String): DFilter                   = nest(Map("$like" -> value))

}

final class CaseInsensitiveFilterOps[T](val prop: Property[_, T]) extends PropertyFilterOps[T] {

  def $eq(value: T): DFilter =
    nest(Map("$regex" -> ("^" + DFilterOps.string2RegexEscapedString(value.toString) + "$"), "$options" -> "i"))
}

object DFilterSyntax extends DFilterSyntax
