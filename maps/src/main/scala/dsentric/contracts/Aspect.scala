package dsentric.contracts

import dsentric.codecs.{DCodec, DStringCodec}
import dsentric.operators.{DataOperator, Expected, Optional}
import dsentric.DObject

class AspectFor[D <: DObject, D2 <: D](_source: ContractFor[D])(
  f: PartialFunction[Property[D, _], Option[AspectProperty[D2, _]]] = PartialFunction.empty
) extends ContractLike[D2] {

  //TODO: name override
  //TODO: DataOperators
  def \\(property: ObjectProperty[D], merged: ObjectProperty[D]*)(
    f: PartialFunction[Property[D, _], Option[AspectProperty[D2, _]]] = PartialFunction.empty
  ): ExpectedObjectAspectProperty[D2] = {
    val fields           = Aspect.getFields(property)(f) ++ merged.flatMap(Aspect.getFields(_)(f))
    val expectedProperty = new ExpectedObjectAspectProperty[D2](property._codec, Nil)
    fields.foreach(_._2.__setParent(expectedProperty))
    expectedProperty.__setFields(fields)
    expectedProperty.__setParent(this)
    expectedProperty
  }

  //TODO support pulling from property if has withAdditional
  def \\\[Key, Value](property: ObjectProperty[D], merged: ObjectProperty[D]*)(
    f: PartialFunction[Property[D, _], Option[AspectProperty[D2, _]]] = PartialFunction.empty
  )(implicit K: DStringCodec[Key], V: DCodec[Value]): ExpectedObjectAspectPropertyWithAdditional[D2, Key, Value] = {
    val fields           = Aspect.getFields(property)(f) ++ merged.flatMap(Aspect.getFields(_)(f))
    val expectedProperty =
      new ExpectedObjectAspectPropertyWithAdditional[D2, Key, Value](property._codec, Nil, Nil, K, V)
    fields.foreach(_._2.__setParent(expectedProperty))
    expectedProperty.__setFields(fields)
    expectedProperty.__setParent(this)
    expectedProperty
  }

  def \\?(property: ObjectProperty[D], merged: ObjectProperty[D]*)(
    f: PartialFunction[Property[D, _], Option[AspectProperty[D2, _]]] = PartialFunction.empty
  ): MaybeObjectAspectProperty[D2] = {
    val fields        = Aspect.getFields(property)(f) ++ merged.flatMap(Aspect.getFields(_)(f))
    val maybeProperty = new MaybeObjectAspectProperty[D2](property._codec, Nil)
    fields.foreach(_._2.__setParent(maybeProperty))
    maybeProperty.__setFields(fields)
    maybeProperty.__setParent(this)
    maybeProperty
  }

  //TODO support pulling from property if has withAdditional
  def \\\?[Key, Value](property: ObjectProperty[D], merged: ObjectProperty[D]*)(
    f: PartialFunction[Property[D, _], Option[AspectProperty[D2, _]]]
  )(implicit K: DStringCodec[Key], V: DCodec[Value]): MaybeObjectAspectPropertyWithAdditional[D2, Key, Value] = {
    val fields        = Aspect.getFields(property)(f) ++ merged.flatMap(Aspect.getFields(_)(f))
    val maybeProperty = new MaybeObjectAspectPropertyWithAdditional[D2, Key, Value](property._codec, Nil, Nil, K, V)
    fields.foreach(_._2.__setParent(maybeProperty))
    maybeProperty.__setFields(fields)
    maybeProperty.__setParent(this)
    maybeProperty
  }

  override def _root: ContractFor[D2] = _source.asInstanceOf[ContractFor[D2]]

  private[contracts] var __fields: Map[String, Property[D2, _]] = _
  @volatile
  private var _bitmap0: Boolean                                 = false

  final def _fields: Map[String, Property[D2, _]] =
    if (_bitmap0) __fields
    else {
      this.synchronized {
        //Contract resolved properties should override aspect transformed properties
        val aspectFields =
          Aspect.getFields[D, D2](_source)(f)
        aspectFields.foreach(_._2.__setParent(this))

        __fields = aspectFields ++
          this.getClass.getMethods.flatMap { m =>
            if (
              classOf[Property[D2, _]].isAssignableFrom(
                m.getReturnType
              ) && m.getTypeParameters.isEmpty && m.getParameterTypes.isEmpty
            )
              m.invoke(this) match {
                case prop: PropertyResolver[D2, Any] @unchecked =>
                  Some(prop.__nameOverride.getOrElse(m.getName) -> prop)
                case prop: ObjectAspectProperty[D2] @unchecked  =>
                  prop.__setKey(m.getName)
                  Some(prop._key -> prop)
                case _                                          =>
                  None
              }
            else
              None
          }.toMap
        _bitmap0 = true
      }
      __fields
    }
}

class Aspect[D <: DObject](_source: ContractFor[D])(
  f: PartialFunction[Property[D, _], Option[AspectProperty[D, _]]] = PartialFunction.empty
) extends AspectFor[D, D](_source)(f)

object Aspect {

  def apply[D <: DObject](_root: ContractFor[D])(
    f: PartialFunction[Property[D, _], Option[AspectProperty[D, _]]]
  ): Aspect[D] =
    new Aspect[D](_root)(f)

  private[contracts] def getFields[D <: DObject, D2 <: D](
    contract: BaseContract[D]
  )(f: PartialFunction[Property[D, _], Option[AspectProperty[D2, _]]]): Map[String, AspectProperty[D2, _]] = {

    def reparent[B <: BaseContract[D2]](b: B, fields: Map[String, AspectProperty[D2, _]]): B = {
      fields.foreach(_._2.__setParent(b))
      b
    }
    def mapValueProperty[T]: Function[ValueProperty[D, T], ValueAspectProperty[D2, T]]           = {
      case e: ExpectedProperty[D, T]      =>
        ExpectedAspectProperty(e._key, e._codec, e._dataOperators)
      case e: MaybeExpectedProperty[D, T] =>
        ExpectedAspectProperty(e._key, e._codec, e._dataOperators)
      case m: MaybeProperty[D, T]         =>
        MaybeAspectProperty(m._key, m._codec, m._dataOperators)
      case d: DefaultProperty[D, T]       =>
        DefaultAspectProperty(d._key, d._default, d._codec, d._dataOperators)
      case d: MaybeDefaultProperty[D, T]  =>
        DefaultAspectProperty(d._key, d._default, d._codec, d._dataOperators)
      case _: ValueAspectProperty[D, T]   =>
        ???
    }
    def transformFields(fields: Map[String, Property[D, _]]): Map[String, AspectProperty[D2, _]] =
      fields.flatMap { pair =>
        f.lift(pair._2) -> pair._2 match {
          case (None, o: ExpectedObjectProperty[D])                            =>
            val fields         = transformFields(o._fields)
            val aspectProperty = ExpectedObjectAspectProperty(o, fields, o._dataOperators)
            Some(pair._1 -> reparent(aspectProperty, fields))
          case (None, o: MaybeExpectedObjectProperty[D])                       =>
            val fields         = transformFields(o._fields)
            val aspectProperty = ExpectedObjectAspectProperty(o, fields, o._dataOperators)
            Some(pair._1 -> reparent(aspectProperty, fields))
          case (None, o: MaybeObjectProperty[D])                               =>
            val fields         = transformFields(o._fields)
            val aspectProperty = MaybeObjectAspectProperty(o, fields, o._dataOperators)
            Some(pair._1 -> reparent(aspectProperty, fields))
          case (None, p: ValueProperty[D, _])                                  =>
            Some(pair._1 -> mapValueProperty(p))
          case (Some(Some(a: ObjectAspectProperty[D2])), o: ObjectProperty[D]) =>
            val fields = transformFields(o._fields)
            a.__setFields(fields)
            Some(pair._1 -> reparent(a, fields))
          case (Some(maybeProperty), _)                                        =>
            maybeProperty.map(pair._1 -> _)
          case (None, _)                                                       =>
            None
        }
      }

    val finalFields = transformFields(contract._fields)
    finalFields
  }
}

trait DAspectSyntax {

  type AspectPropertyFunction[D <: DObject, D2 <: DObject] =
    PartialFunction[Property[D, _], Option[AspectProperty[D2, _]]]

  implicit def toPropertyAspectOps[D <: DObject](p: Property[D, _]): PropertyAspectOps[D, _] =
    new PropertyAspectOps(p)

  implicit def toObjectPropertyAspectOps[D <: DObject](p: ObjectProperty[D]): ObjectPropertyAspectOps[D] =
    new ObjectPropertyAspectOps(p)

  implicit def toValuePropertyAspectOps[D <: DObject, T](p: ValueProperty[D, T]): ValuePropertyAspectOps[D, T] =
    new ValuePropertyAspectOps(p)

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

final class PropertyAspectOps[D <: DObject, _](val p: Property[D, _]) extends AnyVal {
  def $asExpected[D2 <: D](): AspectProperty[D2, _] =
    p match {
      case o: ObjectProperty[D]   =>
        val dataOperators =
          o._dataOperators
            .collect { case d: DataOperator[DObject] with Expected => d }
        new ObjectPropertyAspectOps[D](o).$asExpected[D2](dataOperators)
      case v: ValueProperty[D, _] =>
        val dataOperators =
          v._dataOperators
            .collect { case d: DataOperator[_] with Expected => d }
        new ValuePropertyAspectOps[D, Any](v.asInstanceOf[ValueProperty[D, Any]]).$asExpected[D2](dataOperators)
      case _                      =>
        ???
    }

  def $asMaybe[D2 <: D](): AspectProperty[D2, _] =
    p match {
      case o: ObjectProperty[D]   =>
        val dataOperators =
          o._dataOperators
            .collect { case d: DataOperator[DObject] with Optional => d }
        new ObjectPropertyAspectOps[D](o).$asMaybe[D2](dataOperators)
      case v: ValueProperty[D, _] =>
        val dataOperators =
          v._dataOperators
            .collect { case d: DataOperator[_] with Optional => d }
        new ValuePropertyAspectOps[D, Any](v.asInstanceOf[ValueProperty[D, Any]]).$asMaybe[D2](dataOperators)
      case _                      =>
        ???
    }

  def $asExpected[D2 <: D](replaceDataOperators: List[DataOperator[DObject] with Expected]): AspectProperty[D2, _] =
    p match {
      case o: ObjectProperty[D]   =>
        new ObjectPropertyAspectOps[D](o).$asExpected[D2](replaceDataOperators)
      case v: ValueProperty[D, _] =>
        new ValuePropertyAspectOps[D, Any](v.asInstanceOf[ValueProperty[D, Any]]).$asExpected[D2](replaceDataOperators)
      case _                      =>
        ???
    }

  def $asMaybe[D2 <: D](replaceDataOperators: List[DataOperator[DObject] with Optional]): AspectProperty[D2, _] =
    p match {
      case o: ObjectProperty[D]   =>
        new ObjectPropertyAspectOps[D](o).$asMaybe[D2](replaceDataOperators)
      case v: ValueProperty[D, _] =>
        new ValuePropertyAspectOps[D, Any](v.asInstanceOf[ValueProperty[D, Any]]).$asMaybe[D2](replaceDataOperators)
      case _                      =>
        ???
    }

}

final class ObjectPropertyAspectOps[D <: DObject](val p: ObjectProperty[D]) extends AnyVal {

  def $asExpected[D2 <: D](): ExpectedObjectAspectProperty[D2] = {
    val dataOperators =
      p._dataOperators
        .collect { case d: DataOperator[DObject] with Expected => d }
    ExpectedObjectAspectProperty[D2](p, dataOperators)
  }

  def $asExpected[D2 <: D](
    replaceDataOperators: List[DataOperator[DObject] with Expected]
  ): ExpectedObjectAspectProperty[D2] =
    ExpectedObjectAspectProperty[D2](p, replaceDataOperators)

  def $asExpected[D2 <: D](
    appendDataOperator: DataOperator[DObject] with Expected,
    tail: DataOperator[DObject] with Expected*
  ): ExpectedObjectAspectProperty[D2] = {
    val dataOperators =
      p._dataOperators
        .collect { case d: DataOperator[DObject] with Expected => d } ::: appendDataOperator :: tail.toList
    ExpectedObjectAspectProperty[D2](p, dataOperators)
  }

  def $asMaybe[D2 <: D](): MaybeObjectAspectProperty[D2] = {
    val dataOperators =
      p._dataOperators
        .collect { case d: DataOperator[DObject] with Optional => d }
    MaybeObjectAspectProperty[D2](p, dataOperators)
  }

  def $asMaybe[D2 <: D](
    replaceDataOperators: List[DataOperator[DObject] with Optional] = Nil
  ): MaybeObjectAspectProperty[D2] =
    MaybeObjectAspectProperty[D2](p, replaceDataOperators)

  def $asMaybe[D2 <: D](
    appendDataOperator: DataOperator[DObject] with Optional,
    tail: DataOperator[DObject] with Optional*
  ): MaybeObjectAspectProperty[D2] = {
    val dataOperators =
      p._dataOperators
        .collect { case d: DataOperator[DObject] with Optional => d } ::: appendDataOperator :: tail.toList
    MaybeObjectAspectProperty[D2](p, dataOperators)
  }

}

final class ValuePropertyAspectOps[D <: DObject, T](val p: ValueProperty[D, T]) extends AnyVal {

  def $asExpected[D2 <: D](): ExpectedAspectProperty[D2, T] = {
    val dataOperators =
      p._dataOperators
        .collect { case d: DataOperator[T] with Expected => d }
    ExpectedAspectProperty(p._key, p._codec, dataOperators)
  }

  def $asExpected[D2 <: D](replaceDataOperators: List[DataOperator[T] with Expected]): ExpectedAspectProperty[D2, T] =
    ExpectedAspectProperty(p._key, p._codec, replaceDataOperators)

  def $asExpected[D2 <: D](
    appendDataOperators: DataOperator[T] with Expected,
    tail: DataOperator[T] with Expected*
  ): ExpectedAspectProperty[D2, T] = {
    val dataOperators =
      p._dataOperators
        .collect { case d: DataOperator[T] with Expected => d } ::: appendDataOperators :: tail.toList
    ExpectedAspectProperty(p._key, p._codec, dataOperators)
  }

  def $asMaybe[D2 <: D](): MaybeAspectProperty[D2, T] = {
    val dataOperators =
      p._dataOperators
        .collect { case d: DataOperator[T] with Optional => d }
    MaybeAspectProperty(p._key, p._codec, dataOperators)
  }

  def $asMaybe[D2 <: D](replaceDataOperators: List[DataOperator[T] with Optional] = Nil): MaybeAspectProperty[D2, T] =
    MaybeAspectProperty(p._key, p._codec, replaceDataOperators)

  def $asMaybe[D2 <: D](
    appendDataOperators: DataOperator[T] with Optional,
    tail: DataOperator[T] with Optional*
  ): MaybeAspectProperty[D2, T] = {
    val dataOperators =
      p._dataOperators
        .collect { case d: DataOperator[T] with Optional => d } ::: appendDataOperators :: tail.toList
    MaybeAspectProperty(p._key, p._codec, dataOperators)
  }

  def $asDefault[D2 <: D](default: T): DefaultAspectProperty[D2, T] = {
    val operators =
      p._dataOperators
        .collect { case d: DataOperator[T] with Optional => d }
    DefaultAspectProperty(p._key, default, p._codec, operators)
  }

  def $asDefault[D2 <: D](
    default: T,
    replaceDataOperators: List[DataOperator[T] with Optional]
  ): DefaultAspectProperty[D2, T] =
    DefaultAspectProperty(p._key, default, p._codec, replaceDataOperators)

  def $asDefault[D2 <: D](
    default: T,
    appendDataOperators: DataOperator[T] with Optional,
    tail: DataOperator[T] with Optional*
  ): DefaultAspectProperty[D2, T] = {
    val operators =
      p._dataOperators
        .collect { case d: DataOperator[T] with Optional => d } ::: appendDataOperators :: tail.toList
    DefaultAspectProperty(p._key, default, p._codec, operators)
  }
}

final class ExpectedPropertyAspectOps[D <: DObject, T](val p: ExpectedProperty[D, T]) extends AnyVal {
  def $appendDataOperators[D2 <: D](dataOperators: DataOperator[T] with Expected*): ExpectedAspectProperty[D2, T] =
    ExpectedAspectProperty(p._key, p._codec, p._dataOperators ++ dataOperators)

  def $replaceDataOperators[D2 <: D](dataOperators: DataOperator[T] with Expected*): ExpectedAspectProperty[D2, T] =
    ExpectedAspectProperty(p._key, p._codec, dataOperators.toList)
}

final class MaybeExpectedPropertyAspectOps[D <: DObject, T](val p: MaybeExpectedProperty[D, T]) extends AnyVal {
  def $appendDataOperators[D2 <: D](dataOperators: DataOperator[T] with Expected*): ExpectedAspectProperty[D2, T] =
    ExpectedAspectProperty(p._key, p._codec, p._dataOperators ++ dataOperators)

  def $replaceDataOperators[D2 <: D](dataOperators: DataOperator[T] with Expected*): ExpectedAspectProperty[D2, T] =
    ExpectedAspectProperty(p._key, p._codec, dataOperators.toList)
}

final class MaybePropertyAspectOps[D <: DObject, T](val p: MaybeProperty[D, T]) extends AnyVal {
  def $appendDataOperators[D2 <: D](dataOperators: DataOperator[T] with Optional*): MaybeAspectProperty[D2, T] =
    MaybeAspectProperty(p._key, p._codec, p._dataOperators ++ dataOperators)

  def $replaceDataOperators[D2 <: D](dataOperators: DataOperator[T] with Optional*): MaybeAspectProperty[D2, T] =
    MaybeAspectProperty(p._key, p._codec, dataOperators.toList)
}

final class DefaultPropertyAspectOps[D <: DObject, T](val p: DefaultProperty[D, T]) extends AnyVal {

  def $changeDefault[D2 <: D](default: T): DefaultAspectProperty[D2, T] =
    DefaultAspectProperty(p._key, default, p._codec, p._dataOperators)

  def $appendDataOperators[D2 <: D](dataOperators: DataOperator[T] with Optional*): DefaultAspectProperty[D2, T] =
    DefaultAspectProperty(p._key, p._default, p._codec, p._dataOperators ++ dataOperators)

  def $replaceDataOperators[D2 <: D](dataOperators: DataOperator[T] with Optional*): DefaultAspectProperty[D2, T] =
    DefaultAspectProperty(p._key, p._default, p._codec, dataOperators.toList)
}

final class MaybeDefaultPropertyAspectOps[D <: DObject, T](val p: MaybeDefaultProperty[D, T]) extends AnyVal {
  def $changeDefault[D2 <: D](default: T): DefaultAspectProperty[D2, T] =
    DefaultAspectProperty(p._key, default, p._codec, p._dataOperators)

  def $appendDataOperators[D2 <: D](dataOperators: DataOperator[T] with Optional*): DefaultAspectProperty[D2, T] =
    DefaultAspectProperty(p._key, p._default, p._codec, p._dataOperators ++ dataOperators)

  def $replaceDataOperators[D2 <: D](dataOperators: DataOperator[T] with Optional*): DefaultAspectProperty[D2, T] =
    DefaultAspectProperty(p._key, p._default, p._codec, dataOperators.toList)
}

final class ExpectedObjectAspectOps[D <: DObject](val p: ExpectedObjectProperty[D]) extends AnyVal {

  def $appendDataOperators[D2 <: D](
    dataOperators: DataOperator[DObject] with Expected*
  ): ExpectedObjectAspectProperty[D2] =
    ExpectedObjectAspectProperty(p, p._dataOperators ++ dataOperators)

  def $replaceDataOperators[D2 <: D](
    dataOperators: DataOperator[DObject] with Expected*
  ): ExpectedObjectAspectProperty[D2] =
    ExpectedObjectAspectProperty(p, dataOperators.toList)
}

final class MaybeExpectedObjectAspectOps[D <: DObject](val p: MaybeExpectedObjectProperty[D]) extends AnyVal {
  def $appendDataOperators[D2 <: D](
    dataOperators: DataOperator[DObject] with Expected*
  ): ExpectedObjectAspectProperty[D2] =
    ExpectedObjectAspectProperty(p, p._dataOperators ++ dataOperators)

  def $replaceDataOperators[D2 <: D](
    dataOperators: DataOperator[DObject] with Expected*
  ): ExpectedObjectAspectProperty[D2] =
    ExpectedObjectAspectProperty(p, dataOperators.toList)
}

final class MaybeObjectAspectOps[D <: DObject](val p: MaybeObjectProperty[D]) extends AnyVal {
  def $appendDataOperators[D2 <: D](
    dataOperators: DataOperator[DObject] with Optional*
  ): MaybeObjectAspectProperty[D2] =
    MaybeObjectAspectProperty(p, p._dataOperators ++ dataOperators)

  def $replaceDataOperators[D2 <: D](dataOperators: DataOperator[DObject] with Optional*): MaybeObjectAspectProperty[D2] =
    MaybeObjectAspectProperty(p, dataOperators.toList)
}
