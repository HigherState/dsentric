package dsentric.codecs

import cats.data.NonEmptyList
import com.github.ghik.silencer.silent
import dsentric._
import dsentric.contracts.{Contract, ContractLike}
import dsentric.schema._
import shapeless.{::, labelled, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.UnaryTCConstraint.*->*
import shapeless.labelled.FieldType
import shapeless.ops.hlist.ToTraversable

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable

sealed trait DCodec[T] {
  def apply(t: T): Raw
  def unapply(a: Raw): Option[T]

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

  def typeDefinition: TypeDefinition

  def containsContractCodec: Boolean
  @inline
  protected def deltaNull(deltaValue: Raw): Boolean =
    deltaValue.isInstanceOf[DNull.type]
}

trait DValueCodec[T]  extends DCodec[T]      {
  def apply(t: T): RawValue
  def containsContractCodec: Boolean = false
}
//Required for handling bridge generation of value classes
trait DValueBridge[T] extends DValueCodec[T] {
  def bridge(t: T): Data

  def apply(t: T): RawValue =
    bridge(t).value
}

trait DStringCodec[T] extends DValueCodec[T] {
  def apply(t: T): String

  def unapply(a: Raw): Option[T] =
    a match {
      case s: String =>
        fromString(s)
      case _         =>
        None
    }

  def fromString(s: String): Option[T]

  def typeDefinition: StringDefinition
}

trait DMapCodec[M, K, T] extends DCodec[M] {

  def keyCodec: DStringCodec[K]
  def valueCodec: DCodec[T]

  def build(m: Map[K, T]): Option[M]

  def extract(m: M): Map[K, T]

  def apply(t: M): RawObject         =
    extract(t).map(p => keyCodec(p._1) -> valueCodec(p._2))

  def unapply(a: Raw): Option[M]     =
    a match {
      case s: RawObject @unchecked =>
        s.map(p => keyCodec.unapply(p._1) -> valueCodec.unapply(p._2))
          .foldLeft[Option[mutable.Builder[(K, T), Map[K, T]]]](Some(Map.newBuilder[K, T])) {
            case (Some(mb), (Some(k), Some(v))) =>
              Some(mb.addOne(k -> v))
            case _                              =>
              None
          }
          .flatMap(mb => build(mb.result()))
      case _                       =>
        None
    }
  def containsContractCodec: Boolean =
    valueCodec.containsContractCodec
}

trait DCollectionCodec[S, T] extends DCodec[S] {
  def valueCodec: DCodec[T]

  def build(t: Vector[T]): Option[S]

  def extract(s: S): Vector[T]

  def apply(t: S): RawArray =
    valueCodec match {
      case _: DirectCodec[T] @unchecked =>
        extract(t)
      case _                 =>
        extract(t).map(valueCodec.apply)
    }

  def unapply(a: Raw): Option[S] =
    a match {
      case s: RawArray @unchecked =>
        s.map(valueCodec.unapply)
          .foldLeft[Option[VectorBuilder[T]]](Some(new VectorBuilder[T])) {
            case (Some(vb), Some(t)) => Some(vb += t)
            case _                   => None
          }
          .flatMap(vb => build(vb.result()))
      case _                      =>
        None
    }

  def containsContractCodec: Boolean =
    valueCodec.containsContractCodec
}

/**
 * Converting from or too a value class
 * @param from
 * @param to
 * @param typeDefinitionOverride
 * @param D
 * @tparam S
 * @tparam T
 */
final case class DValueClassCodec[S, T](
  from: S => T,
  to: T => Option[S],
  typeDefinitionOverride: Option[TypeDefinition] = None
)(implicit D: DCodec[T])
    extends DCodec[S] {
  def unapply(a: Raw): Option[S] =
    D.unapply(a)
      .flatMap(to)

  def apply(s: S): Raw =
    D(from(s))

  def internalCodec: DCodec[T] = D

  def typeDefinition: TypeDefinition =
    typeDefinitionOverride.getOrElse(D.typeDefinition)

  def containsContractCodec: Boolean = D.containsContractCodec
}

/**
 * For cases where D is simply a new Box around the RawObject
 * @param contract
 * @param cstr
 * @tparam D
 */
final case class DContractCodec[D <: DObject](contract: ContractLike[D], cstr: RawObject => D) extends DCodec[D] {
  def containsContractCodec: Boolean = true

  def unapply(a: Raw): Option[D] =
    a match {
      case m: RawObject @unchecked =>
        Some(cstr(m))
      case _                       =>
        None
    }

  def apply(t: D): RawObject =
    t.value

  lazy val typeDefinition: TypeDefinition =
    Definition.nestedContractObjectDefinition(contract)
}

sealed trait DParameter[H <: HList] {
  def encode(t: H): RawObject

  //Decode will return either a list of failed key and option DCodecs (None for missing field), or the RawObject with the parameter fields removed
  //And a function which takes this raw object and produces an argument List.
  def decode(r: RawObject): (RawObject, Either[NonEmptyList[(String, Option[(Raw, DCodec[_])])], RawObject => H])

  def propertyDefinition: Set[PropertyDefinition]
}
object DParameter                   {
  implicit val hnilParameter: DParameter[HNil] =
    new DParameter[HNil] {
      private val result: Either[NonEmptyList[(String, Option[(Raw, DCodec[_])])], RawObject => HNil] =
        Right(_ => HNil)

      def encode(t: HNil): RawObject =
        RawObject.empty

      def decode(r: RawObject): (RawObject, Either[NonEmptyList[(String, Option[(Raw, DCodec[_])])], RawObject => HNil]) =
        r -> result

      def propertyDefinition: Set[PropertyDefinition] =
        Set.empty
    }

  implicit def rawObjectValueParameter[K <: Symbol, T <: HList](implicit
    tEncoder: DParameter[T]
  ): DParameter[FieldType[K, RawObject] :: T] =
    new DParameter[FieldType[K, RawObject] :: T] {
      def encode(t: FieldType[K, RawObject] :: T): RawObject =
        t.head ++ tEncoder.encode(t.tail)

      def decode(
        r: RawObject
      ): (RawObject, Either[NonEmptyList[(String, Option[(Raw, DCodec[_])])], RawObject => FieldType[K, RawObject] :: T]) = {
        val (r2, tail) = tEncoder.decode(r)
        r2 -> tail.map(function => (rf: RawObject) => labelled.field[K](rf) :: function(rf))
      }

      def propertyDefinition: Set[PropertyDefinition] =
        tEncoder.propertyDefinition
    }

  implicit def hListParameter[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[DCodec[H]],
    tEncoder: DParameter[T]
  ): DParameter[FieldType[K, H] :: T] = new DParameter[FieldType[K, H] :: T] {
    val fieldName: String                                                                                  = witness.value.name
    private def failed(maybeRaw: Option[Raw]): Either[NonEmptyList[(String, Option[(Raw, DCodec[_])])], H] =
      Left(NonEmptyList(fieldName -> maybeRaw.map(_ -> hEncoder.value), Nil))

    def encode(t: FieldType[K, H] :: T): RawObject                                                         =
      tEncoder.encode(t.tail) +
        (fieldName -> hEncoder.value.apply(t.head))

    def decode(
      r: RawObject
    ): (RawObject, Either[NonEmptyList[(String, Option[(Raw, DCodec[_])])], RawObject => FieldType[K, H] :: T]) = {
      import cats.implicits._
      val result     =
        r.get(fieldName).fold(failed(None)) { raw =>
          hEncoder.value.unapply(raw).fold(failed(Some(raw)))(Right.apply)
        }
      val (r2, tail) = tEncoder.decode(r)
      (r2 - fieldName) ->
        (result, tail).parMapN((head, function) => (rf: RawObject) => labelled.field[K](head) :: function(rf))
    }

    def propertyDefinition: Set[PropertyDefinition] =
      tEncoder.propertyDefinition + PropertyDefinition(fieldName, hEncoder.value.typeDefinition, Nil, None, true, None)
  }
}

/**
 * There is an assumption that the Parameter Codecs will be simple Value DCodecs
 * Also there is currently no support for Option
 * @param contract
 * @param generic
 * @param hCodec
 * @tparam D
 * @tparam H
 */
final case class DParameterisedContractCodec[D <: DObject, H <: HList](contract: ContractLike[D])(implicit
  generic: LabelledGeneric.Aux[D, H],
  hCodec: Lazy[DParameter[H]]
) extends DCodec[D] {
  def unapply(raw: Raw): Option[D] =
    raw match {
      case rawObject: RawObject @unchecked =>
        val (r2, tail) = hCodec.value.decode(rawObject)
        tail.map(f => generic.from(f(r2))).toOption
      case _                               =>
        None
    }

  def extractParameters(
    rawObject: RawObject
  ): (RawObject, Either[NonEmptyList[(String, Option[(Raw, DCodec[_])])], RawObject => D]) = {
    val (valueObject, result) = hCodec.value.decode(rawObject)
    valueObject -> result.map(f => (rawObject: RawObject) => generic.from(f(rawObject)))
  }

  def apply(t: D): RawObject =
    hCodec.value.encode(generic.to(t))

  lazy val typeDefinition: TypeDefinition = {
    val contractDefinition = Definition.nestedContractObjectDefinition(contract)
    contractDefinition.copy(properties = contractDefinition.properties ++ hCodec.value.propertyDefinition)
  }

  def containsContractCodec: Boolean = true
}

/**
 * May want to generalise away from being Contract when we have Case Class DCodecs
 * @param contract
 * @param cstr
 * @param dstr
 * @param build
 * @param extract
 * @tparam S
 * @tparam D
 */
final case class DKeyContractCollectionCodec[S, D <: DObject](
  contract: ContractLike[D],
  cstr: (String, RawObject) => Option[D],
  dstr: D => (String, RawObject),
  build: Vector[D] => Option[S],
  extract: S => Vector[D]
) extends DCodec[S] {
  def unapply(a: Raw): Option[S] =
    a match {
      case m: RawObject @unchecked =>
        m.view
          .foldLeft(Option(Vector.newBuilder[D])) {
            case (Some(b), (key, value: RawObject @unchecked)) =>
              cstr(key, value).map(b.addOne)
            case _                                             =>
              None
          }
          .flatMap(b => build(b.result()))
      case _                       =>
        None
    }

  def apply(s: S): RawObject =
    extract(s).iterator.map { d =>
      dstr(d)
    }.toMap

  /**
   * Unable to extract property definition to place into object
   * @return
   */
  def typeDefinition: TypeDefinition =
    ObjectDefinition(additionalProperties = Right(Definition.nestedContractObjectDefinition(contract)))

  def containsContractCodec: Boolean = true
}

object DContractCodec {
  val dObjectCstr: RawObject => DObject = new DObjectInst(_)

  def apply(contract: Contract): DContractCodec[DObject] =
    DContractCodec(contract, dObjectCstr)
}

final case class DTypeContractCodec[D <: DObject](typeDefinition: TypeDefinition, cstr: RawObject => D)(
  val contracts: PartialFunction[D, ContractLike[D]]
) extends DCodec[D] {

  def unapply(a: Raw): Option[D] =
    a match {
      case m: RawObject @unchecked =>
        val d = cstr(m)
        contracts
          .lift(d)
          .map(_ => d)
      case _                       =>
        None
    }

  def containsContractCodec: Boolean =
    true

  def apply(t: D): RawObject =
    t.value
}

object DTypeContractCodec {
  def apply(typeDefinition: TypeDefinition)(
    contracts: PartialFunction[DObject, Contract]
  ): DTypeContractCodec[DObject] =
    DTypeContractCodec(typeDefinition, DContractCodec.dObjectCstr)(contracts)

  def apply(contracts: PartialFunction[DObject, Contract]): DTypeContractCodec[DObject] =
    DTypeContractCodec(ObjectDefinition.empty, DContractCodec.dObjectCstr)(contracts)
}
@silent
abstract class DProductCodec[T, E <: HList, H <: HList: *->*[DCodec]#λ](val codecs: H)(implicit
  T: ToTraversable.Aux[H, Array, DCodec[_]]
) extends DCodec[T] {

  def apply(t: T): RawArray

  def unapply(a: Raw): Option[T] =
    a match {
      case rawArray: RawArray if rawArray.size == codecsArray.length =>
        rawArray
          .zip(codecsArray)
          .foldRight[Option[HList]](Some(HNil)) { (p, h) =>
            h.flatMap { hlist =>
              p._2.unapply(p._1).map(_ :: hlist)
            }
          }
          .flatMap(h => build(h.asInstanceOf[E]))
      case _                                                         =>
        None
    }

  def build(e: E): Option[T]

  val codecsArray: Array[DCodec[_]] =
    codecs.toArray

  def containsContractCodec: Boolean =
    codecsArray.exists(_.containsContractCodec)

  def typeDefinition: ArrayDefinition =
    ArrayDefinition(codecsArray.map(_.typeDefinition).toVector)
}
@silent
abstract class DCoproductCodec[T, H <: HList: *->*[DCodec]#λ](val codecs: H)(implicit
  T: ToTraversable.Aux[H, List, DCodec[_]]
) extends DCodec[T] {
  val codecsList: List[DCodec[_]] = codecs.toList

  def lift[A](a: A, codec: DCodec[A]): Option[T]

  def containsContractCodec: Boolean =
    codecsList.exists(_.containsContractCodec)
}

object DataCodec extends DValueCodec[Data] {
  def apply(t: Data): Raw =
    t.value

  def unapply(a: Raw): Option[Data] =
    a match {
      case a: RawObject @unchecked =>
        Some(new DObjectInst(a))
      case v: RawArray @unchecked  =>
        Some(new DArray(v))
      case DNull                   =>
        Some(DNull)
      case j                       =>
        Some(new DValue(j))
    }

  def typeDefinition: TypeDefinition =
    TypeDefinition.anyDefinition
}

object DValueCodec {

  def literal[T](t: T)(implicit D: DValueCodec[T]): DValueCodec[T] =
    new DValueCodec[T] {
      private val rawT: Raw              = D.apply(t)
      override def apply(t: T): RawValue =
        rawT

      def unapply(a: Raw): Option[T] =
        if (a == rawT) Some(t)
        else None

      def typeDefinition: TypeDefinition = D.typeDefinition match {
        case s: StringDefinition  => s.copy(enum = List(rawT))
        case i: IntegerDefinition => i.copy(enum = List(rawT))
        case n: NumberDefinition  => n.copy(enum = List(rawT))
        case t                    => t
      }
    }
}
