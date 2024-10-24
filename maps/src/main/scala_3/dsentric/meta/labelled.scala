package dsentric.meta

import scala.deriving._
import scala.compiletime._

object labelled {
  opaque type FieldType[K, +V] <: V = V

  type ->>[K, +V] = FieldType[K, V]

  def field[K]: FieldBuilder[K] = new FieldBuilder(true)

  class FieldBuilder[K](private val dummy: Boolean) extends AnyVal {
    def apply[V](v: V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
}

trait DepFn0 {
  type Out

  def apply(): Out
}

trait Labelling[T] extends DepFn0 with Serializable { type Out <: Tuple }

object Labelling {
  type Aux[T, Out0] = Labelling[T] { type Out = Out0 }

  def apply[T](implicit lab: Labelling[T]): Aux[T, lab.Out] = lab

  inline given [T](using m: Mirror.Of[T]): Labelling.Aux[T, m.MirroredElemLabels] = {
    val tuple = constValueTuple[m.MirroredElemLabels]
    new InnerLabel[m.MirroredElemLabels, T](tuple)

  }
  private class InnerLabel[A <: Tuple, T](tuple:A) extends Labelling[T] {
    override type Out = A
    override def apply(): A = tuple
  }
}

trait LabelledType[T] extends DepFn1[T] with Serializable { type Out <: Tuple }

object LabelledType {
  import labelled.{FieldType, field}

  type Aux[T, Out0] = LabelledType[T] { type Out = Out0 }

  def apply[T](implicit lab: LabelledType[T]): Aux[T, lab.Out] = lab

  type FieldTypes[Labels <: Tuple, Types <: Tuple] <: Tuple =
    (Labels, Types) match {
      case (labelHead *: labelTail, typeHead *: typeTail) =>
        FieldType[labelHead, typeHead] *: FieldTypes[labelTail, typeTail]
      case _                                              =>
        EmptyTuple
    }

  def fieldTypes[Labels <: Tuple, Types <: Tuple](labels: Labels, types: Types): FieldTypes[Labels, Types] =
    (labels, types) match {
      case (labelHead *: labelTail, typeHead *: typeTail) =>
        (field(typeHead) *: fieldTypes(labelTail, typeTail)).asInstanceOf[FieldTypes[Labels, Types]]
      case _                                              =>
        EmptyTuple.asInstanceOf[FieldTypes[Labels, Types]]
    }

  inline given [T <: Product](using m: Mirror.ProductOf[T]): LabelledType.Aux[T, FieldTypes[m.MirroredElemLabels, m.MirroredElemTypes]] = {
    def f(t:T) =
      fieldTypes[m.MirroredElemLabels, m.MirroredElemTypes](
        constValueTuple[m.MirroredElemLabels],
        Tuple.fromProductTyped(t)
      )
    new InnerLabelledType[T,m.MirroredElemLabels, m.MirroredElemTypes](f)
  }

  private class InnerLabelledType[T <: Product, EL <: Tuple, ET <: Tuple](f:T => FieldTypes[EL, ET]) extends LabelledType[T] {
    override type Out = FieldTypes[EL, ET]
    override def apply(t: T): Out = f(t)
  }
}