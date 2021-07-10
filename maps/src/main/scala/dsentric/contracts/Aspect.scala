package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric.{DObject, Path}

class Aspect[D <: DObject](val _root: ContractFor[D], val _fields: Map[String, Property[D, _]])
    extends BaseContract[D]
    with ContractLens[D] {
  def _path: Path = Path.empty
}

object Aspect {

  def apply[D <: DObject](
    c: ContractFor[D]
  )(f: PartialFunction[Property[D, _], Option[AspectProperty[D, _]]]): Aspect[D] = {
    def reparent[B <: BaseContract[D]](b: B, fields: Map[String, AspectProperty[D, _]]): B = {
      fields.foreach(_._2.__setParent(b))
      b
    }
    def mapValueProperty[T]: Function[ValueProperty[D, T], ValueAspectProperty[D, T]]           = {
      case e: ExpectedProperty[D, T]      =>
        ExpectedAspectProperty(e._key, e._path, e._codec, e._dataOperators)
      case e: MaybeExpectedProperty[D, T] =>
        ExpectedAspectProperty(e._key, e._path, e._codec, e._dataOperators)
      case m: MaybeProperty[D, T]         =>
        MaybeAspectProperty(m._key, m._path, m._codec, m._dataOperators)
      case d: DefaultProperty[D, T]       =>
        DefaultAspectProperty(d._key, d._path, d._default, d._codec, d._dataOperators)
      case d: MaybeDefaultProperty[D, T]  =>
        DefaultAspectProperty(d._key, d._path, d._default, d._codec, d._dataOperators)
      case v: ValueAspectProperty[D, T]   =>
        v
    }
    def transformFields(fields: Map[String, Property[D, _]]): Map[String, AspectProperty[D, _]] =
      fields.flatMap { pair =>
        f.lift(pair._2) -> pair._2 match {
          case (None, o: ExpectedObjectProperty[D])                           =>
            val fields         = transformFields(o._fields)
            val aspectProperty = ExpectedObjectAspectProperty(o._key, o._path, fields, o._codec, o._dataOperators)
            Some(pair._1 -> reparent(aspectProperty, fields))
          case (None, o: MaybeExpectedObjectProperty[D])                      =>
            val fields         = transformFields(o._fields)
            val aspectProperty = ExpectedObjectAspectProperty(o._key, o._path, fields, o._codec, o._dataOperators)
            Some(pair._1 -> reparent(aspectProperty, fields))
          case (None, o: MaybeObjectProperty[D])                              =>
            val fields         = transformFields(o._fields)
            val aspectProperty = MaybeObjectAspectProperty(o._key, o._path, fields, o._codec, o._dataOperators)
            Some(pair._1 -> reparent(aspectProperty, fields))
          case (None, p: ValueProperty[D, _])                                 =>
            Some(pair._1 -> mapValueProperty(p))
          case (Some(Some(a: ObjectAspectProperty[D])), o: ObjectProperty[D]) =>
            val fields = transformFields(o._fields)
            a.__setFields(fields)
            Some(pair._1 -> reparent(a, fields))
          case (Some(maybeProperty), _)                                       =>
            maybeProperty.map(pair._1 -> _)
          case (None, _)                                                      =>
            None
        }
      }

    val fields = transformFields(c._fields)
    new Aspect(c, fields)
  }
}

trait DAspectSyntax {

  implicit def toExpectedPropertyAspectOps[D <: DObject, T](
    p: ExpectedProperty[D, T]
  ): ExpectedPropertyAspectOps[D, T] =
    new ExpectedPropertyAspectOps(p)

  implicit def toMaybeExpectedPropertyAspectOps[D <: DObject, T](
    p: MaybeExpectedProperty[D, T]
  ): MaybeExpectedPropertyAspectOps[D, T] =
    new MaybeExpectedPropertyAspectOps(p)

  implicit def toMaybePropertyAspectOps[D <: DObject, T](p: MaybeProperty[D, T]): MaybePropertyAspectOps[D, T] =
    new MaybePropertyAspectOps(p)

  implicit def toDefaultPropertyAspectOps[D <: DObject, T](p: DefaultProperty[D, T]): DefaultPropertyAspectOps[D, T] =
    new DefaultPropertyAspectOps(p)

  implicit def toMaybeDefaultPropertyAspectOps[D <: DObject, T](
    p: MaybeDefaultProperty[D, T]
  ): MaybeDefaultPropertyAspectOps[D, T] =
    new MaybeDefaultPropertyAspectOps(p)

  implicit def toExpectedObjectAspectOps[D <: DObject](p: ExpectedObjectProperty[D]): ExpectedObjectAspectOps[D] =
    new ExpectedObjectAspectOps(p)

  implicit def toMaybeExpectedObjectAspectOps[D <: DObject](
    p: MaybeExpectedObjectProperty[D]
  ): MaybeExpectedObjectAspectOps[D] =
    new MaybeExpectedObjectAspectOps(p)

  implicit def toMaybeObjectAspectOps[D <: DObject](p: MaybeObjectProperty[D]): MaybeObjectAspectOps[D] =
    new MaybeObjectAspectOps(p)
}

object DAspectSyntax extends DAspectSyntax

final class ExpectedPropertyAspectOps[D <: DObject, T](val p: ExpectedProperty[D, T]) extends AnyVal {
  def $asMaybe(dataOperators: DataOperator[Option[T]]*): MaybeAspectProperty[D, T] =
    MaybeAspectProperty(p._key, p._path, p._codec, dataOperators.toList)

  def $asDefault(default: T, dataOperators: DataOperator[Option[T]]*): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, default, p._codec, dataOperators.toList)

  def $appendDataOperators(dataOperators: DataOperator[T]*): ExpectedAspectProperty[D, T] =
    ExpectedAspectProperty(p._key, p._path, p._codec, p._dataOperators ++ dataOperators)

  def $setDataOperators(dataOperators: DataOperator[T]*): ExpectedAspectProperty[D, T] =
    ExpectedAspectProperty(p._key, p._path, p._codec, dataOperators.toList)
}

final class MaybeExpectedPropertyAspectOps[D <: DObject, T](val p: MaybeExpectedProperty[D, T]) extends AnyVal {
  def $asMaybe(dataOperators: DataOperator[Option[T]]*): MaybeAspectProperty[D, T] =
    MaybeAspectProperty(p._key, p._path, p._codec, dataOperators.toList)

  def $asDefault(default: T, dataOperators: DataOperator[Option[T]]*): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, default, p._codec, dataOperators.toList)

  def $appendDataOperators(dataOperators: DataOperator[T]*): ExpectedAspectProperty[D, T] =
    ExpectedAspectProperty(p._key, p._path, p._codec, p._dataOperators ++ dataOperators)

  def $setDataOperators(dataOperators: DataOperator[T]*): ExpectedAspectProperty[D, T] =
    ExpectedAspectProperty(p._key, p._path, p._codec, dataOperators.toList)
}

final class MaybePropertyAspectOps[D <: DObject, T](val p: MaybeProperty[D, T]) extends AnyVal {
  def $asExpected(dataOperators: DataOperator[T]*): ExpectedAspectProperty[D, T] =
    ExpectedAspectProperty(p._key, p._path, p._codec, dataOperators.toList)

  def $asDefault(default: T, dataOperators: DataOperator[Option[T]]*): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, default, p._codec, dataOperators.toList)

  def $appendDataOperators(dataOperators: DataOperator[Option[T]]*): MaybeAspectProperty[D, T] =
    MaybeAspectProperty(p._key, p._path, p._codec, p._dataOperators ++ dataOperators)

  def $setDataOperators(dataOperators: DataOperator[Option[T]]*): MaybeAspectProperty[D, T] =
    MaybeAspectProperty(p._key, p._path, p._codec, dataOperators.toList)
}

final class DefaultPropertyAspectOps[D <: DObject, T](val p: DefaultProperty[D, T]) extends AnyVal {
  def $asExpected(dataOperators: DataOperator[T]*): ExpectedAspectProperty[D, T] =
    ExpectedAspectProperty(p._key, p._path, p._codec, dataOperators.toList)

  def $asMaybe(dataOperators: DataOperator[Option[T]]*): MaybeAspectProperty[D, T] =
    MaybeAspectProperty[D, T](p._key, p._path, p._codec, dataOperators.toList)

  def $changeDefault(default: T): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, default, p._codec, p._dataOperators)

  def $appendDataOperators(dataOperators: DataOperator[Option[T]]*): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, p._default, p._codec, p._dataOperators ++ dataOperators)

  def $setDataOperators(dataOperators: DataOperator[Option[T]]*): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, p._default, p._codec, dataOperators.toList)
}

final class MaybeDefaultPropertyAspectOps[D <: DObject, T](val p: MaybeDefaultProperty[D, T]) extends AnyVal {
  def $asExpected(dataOperators: DataOperator[T]*): ExpectedAspectProperty[D, T] =
    ExpectedAspectProperty(p._key, p._path, p._codec, dataOperators.toList)

  def $asMaybe(dataOperators: DataOperator[Option[T]]*): MaybeAspectProperty[D, T] =
    MaybeAspectProperty[D, T](p._key, p._path, p._codec, dataOperators.toList)

  def $changeDefault(default: T): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, default, p._codec, p._dataOperators)

  def $appendDataOperators(dataOperators: DataOperator[Option[T]]*): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, p._default, p._codec, p._dataOperators ++ dataOperators)

  def $setDataOperators(dataOperators: DataOperator[Option[T]]*): DefaultAspectProperty[D, T] =
    DefaultAspectProperty(p._key, p._path, p._default, p._codec, dataOperators.toList)
}

final class ExpectedObjectAspectOps[D <: DObject](val p: ExpectedObjectProperty[D]) extends AnyVal {
  def $asMaybe(dataOperators: DataOperator[Option[DObject]]*): MaybeObjectAspectProperty[D] =
    MaybeObjectAspectProperty[D](p._key, p._path, p._codec, dataOperators.toList)

  def $appendDataOperators(dataOperators: DataOperator[DObject]*): ExpectedObjectAspectProperty[D] =
    ExpectedObjectAspectProperty(p._key, p._path, p._codec, p._dataOperators ++ dataOperators)

  def $setDataOperators(dataOperators: DataOperator[DObject]*): ExpectedObjectAspectProperty[D] =
    ExpectedObjectAspectProperty(p._key, p._path, p._codec, dataOperators.toList)
}

final class MaybeExpectedObjectAspectOps[D <: DObject](val p: MaybeExpectedObjectProperty[D]) extends AnyVal {
  def $asMaybe(dataOperators: DataOperator[Option[DObject]]*): MaybeObjectAspectProperty[D] =
    MaybeObjectAspectProperty[D](p._key, p._path, p._codec, dataOperators.toList)

  def $appendDataOperators(dataOperators: DataOperator[DObject]*): ExpectedObjectAspectProperty[D] =
    ExpectedObjectAspectProperty(p._key, p._path, p._codec, p._dataOperators ++ dataOperators)

  def $setDataOperators(dataOperators: DataOperator[DObject]*): ExpectedObjectAspectProperty[D] =
    ExpectedObjectAspectProperty(p._key, p._path, p._codec, dataOperators.toList)
}

final class MaybeObjectAspectOps[D <: DObject](val p: MaybeObjectProperty[D]) extends AnyVal {
  def $asExpected(dataOperators: DataOperator[DObject]*): ExpectedObjectAspectProperty[D] =
    ExpectedObjectAspectProperty[D](p._key, p._path, p._codec, dataOperators.toList)

  def $appendDataOperators(dataOperators: DataOperator[Option[DObject]]*): MaybeObjectAspectProperty[D] =
    MaybeObjectAspectProperty(p._key, p._path, p._codec, p._dataOperators ++ dataOperators)

  def $setDataOperators(dataOperators: DataOperator[Option[DObject]]*): MaybeObjectAspectProperty[D] =
    MaybeObjectAspectProperty(p._key, p._path, p._codec, dataOperators.toList)
}
