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

    new Labelling[T] {
      override type Out = m.MirroredElemLabels
      override def apply(): Out = tuple
    }
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

  /*def fieldTypes[Labels <: Tuple, Types <: Tuple](types: Types): FieldTypes[Labels, Types] =
    erasedValue[Labels] match {
      case _: EmptyTuple => EmptyTuple.asInstanceOf[FieldTypes[Labels, Types]]
      case _: (hl *: tl) =>
        types match {
          case _: EmptyTuple => EmptyTuple.asInstanceOf[FieldTypes[Labels, Types]]
          case tt: (_ *: tt) => (field[hl](tt.head) *: fieldTypes[tl, tt](tt.tail)).asInstanceOf[FieldTypes[Labels, Types]]
        }
    }*/

  def fieldTypes[Labels <: Tuple, Types <: Tuple](labels: Labels, types: Types): FieldTypes[Labels, Types] =
    (labels, types) match {
      case (labelHead *: labelTail, typeHead *: typeTail) =>
        (field(typeHead) *: fieldTypes(labelTail, typeTail)).asInstanceOf[FieldTypes[Labels, Types]]
      case _                                              =>
        EmptyTuple.asInstanceOf[FieldTypes[Labels, Types]]
    }

  inline given [T <: Product](using m: Mirror.ProductOf[T]): LabelledType.Aux[T, FieldTypes[m.MirroredElemLabels, m.MirroredElemTypes]] =
    new LabelledType[T] {
      override type Out = FieldTypes[m.MirroredElemLabels, m.MirroredElemTypes]
      override def apply(t: T): Out =
        fieldTypes[m.MirroredElemLabels, m.MirroredElemTypes](
          constValueTuple[m.MirroredElemLabels],
          Tuple.fromProductTyped(t)
        )
    }
}