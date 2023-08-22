package dsentric

import dsentric.codecs.{DCodec, DataCodec}
import dsentric.contracts.{OmitPathSetter, PathSetter, SelectPathSetter, ValidPathSetter}
import dsentric.failure.ValidResult
import dsentric.filter.DFilter

import scala.collection.{mutable, Iterable, IterableFactory, IterableOps}

trait Data extends Any {

  def value: Raw

  def isNull: Boolean = value match {
    case null    => true
    case x: Data => x.isNull
    case _       => false
  }

  def render(implicit R: Renderer): String =
    R.print(value)

  def asObject: Option[DObject] =
    value match {
      case m: RawObject @unchecked =>
        Some(new DObjectInst(m))
      case _                       =>
        None
    }

  def asArray: Option[DArray] =
    value match {
      case m: RawArray @unchecked =>
        Some(new DArray(m))
      case _                      =>
        None
    }

  def nestedContains[T](t: T)(implicit D: DCodec[T]): Boolean =
    DataOps.valueContains(value, t, D)

  def nestedValueMap[T, U](pf: PartialFunction[T, U])(implicit D1: DCodec[T], D2: DCodec[U]): Data =
    new DValue(DataOps.nestedValueMap(value, pf))

  def nestedKeyValueMap[T, U](
    pf: PartialFunction[(String, T), Option[(String, U)]]
  )(implicit D1: DCodec[T], D2: DCodec[U]): Data =
    new DValue(DataOps.nestedKeyValueMap(value, pf).asInstanceOf[RawObject])

  def nestedKeyMap(pf: PartialFunction[String, Option[String]]): Data =
    new DValue(DataOps.nestedKeyMap(value, pf).asInstanceOf[RawObject])

  def decode[T](implicit D: DCodec[T]): Option[T] =
    D.unapply(value)

  override def toString: String =
    SimpleRenderer.print(value)
}

class DValue private[dsentric] (val value: Raw) extends AnyVal with Data

trait DObjectOps[+C <: DObjectOps[C]] extends Any with Data with IterableOps[(String, Data), Iterable, C] {

  def value: RawObject

  protected def wrap(value: RawObject): C

//  protected[this] def newBuilder: mutable.Builder[(String, Data), This] =
//    new mutable.Builder[(String, Data), This] {
//      private var elems:List[(String, Any)] = Nil
//      def +=(elem: (String, Data)): this.type = {
//        elems = elem._1 -> elem._2.value :: elems
//        this
//      }
//
//      def clear(): Unit = {
//        elems = Nil
//      }
//
//      def result(): This =
//        wrap(elems.toMap)
//    }

  override protected def fromSpecific(coll: IterableOnce[(String, Data)]): C =
    wrap(coll.iterator.map(p => p._1 -> p._2.value).toMap)

  override def iterableFactory: IterableFactory[Iterable]                    = value.iterableFactory

  override protected def newSpecificBuilder: mutable.Builder[(String, Data), C] =
    new mutable.Builder[(String, Data), C] {
      private val builder = Map.newBuilder[String, Raw]
      def clear(): Unit   = builder.clear()

      def result(): C = wrap(builder.result())

      def addOne(elem: (String, Data)): this.type = {
        builder.addOne(elem._1 -> elem._2.value)
        this
      }
    }

  def iterator: Iterator[(String, Data)] =
    value.iterator.map(p => p._1 -> ForceWrapper.data(p._2))

  def seq: Iterator[(String, Data)]      =
    iterator

  private[dsentric] def internalWrap(value: RawObject): C =
    wrap(value)

  def +(kv: (String, Data)): C                 =
    wrap(value + (kv._1 -> kv._2.value))

  def ++(kvs: IterableOnce[(String, Data)]): C =
    wrap(value ++ kvs.iterator.map(kv => kv._1 -> kv._2.value))

  def -(key: String): C                        =
    wrap(value - key)

  def --(keys: IterableOnce[String]): C =
    wrap(value -- keys)

  def \[T](path: Path)(implicit D: DCodec[T]): Option[T] =
    PathLensOps
      .traverse(value, path)
      .collect { case D(t) => t }

  def +\(v: (Path, Data)): C                             =
    wrap(PathLensOps.set(value, v._1, v._2.value))

  def ++\(paths: IterableOnce[(Path, Data)]): C =
    wrap(paths.iterator.foldLeft(value) { (v, e) =>
      PathLensOps.set(v, e._1, e._2.value)
    })

  def -\(path: Path): C =
    wrap(PathLensOps.drop(value, path).getOrElse(value))

  def --\(paths: IterableOnce[Path]): C =
    wrap(paths.iterator.foldLeft(value)((v, p) => PathLensOps.drop(v, p).getOrElse(v)))

  def toObject: DObject =
    new DObjectInst(value)

  def get(key: String): Option[Data] =
    value.get(key).map(ForceWrapper.data)

  def get(path: Path): Option[Data] =
    PathLensOps.traverse(value, path).map(ForceWrapper.data)

  override def size: Int =
    value.size

  override def isEmpty: Boolean =
    value.isEmpty

  def keys: Iterable[String] =
    value.keys

  def keySet: Set[String] =
    value.keySet

  def contains(key: String): Boolean =
    value.contains(key)

  def contains(path: Path): Boolean              =
    PathLensOps
      .traverse(value, path)
      .nonEmpty
  //All projected paths must be found
  def contains(projection: DProjection): Boolean =
    RawObjectOps.contains(value, projection.value, false)

  def contains(obj: DObject): Boolean =
    RawObjectOps.contains(value, obj.value, true)

  def intersects(projection: DProjection): Boolean =
    RawObjectOps.intersects(value, projection.value, false)

  def intersects(obj: DObject): Boolean =
    RawObjectOps.intersects(value, obj.value, false)

  def modify[D >: C <: DObject](f: PathSetter[D]): C =
    internalWrap(f(this.asInstanceOf[D]).value)

  def modify[D >: C <: DObject](f: ValidPathSetter[D]): ValidResult[C] =
    f(this.asInstanceOf[D]).map(d => internalWrap(d.asInstanceOf[DObject].value))

  def keyProduct[A](target: DObject)(f: (String, Option[Data], Option[Data]) => A): Iterable[A] = {
    val k = keys ++ target.keys
    k.map { key =>
      f(key, get(key), target.get(key))
    }
  }

  override def toIterable: Iterable[(String, Data)] =
    value.view.mapValues(ForceWrapper.data)

  def values: Iterable[Data] =
    value.values.map(ForceWrapper.data)

  def traverseConcat(additional: DObject): C =
    wrap(RawObjectOps.traverseConcat(this.value, additional.value))

  def removingTraverseConcat[T](additional: DObject, removeValue: T)(implicit D: DCodec[T]): C =
    wrap(RawObjectOps.removingTraverseConcat(this.value, additional.value, D.apply(removeValue)))

  def applyDelta(delta: Delta): C =
    wrap(RawObjectOps.deltaTraverseConcat(this.value, delta.value))

  def reduce: Option[C] =
    RawObjectOps.reduceMap(this.value, DNull).map(wrap)

  //Returns the differences in the compare object
  def diff(compare: DObject): DObject        =
    RawObjectOps.rightDifference(value -> compare.value).fold(DObject.empty)(new DObjectInst(_))

  def deltaDiff(delta: Delta): Option[Delta] =
    RawObjectOps.differenceDelta(this.value -> delta.value).map(new DeltaInst(_))

  def toQuery: DFilter                       =
    DFilter(value)

  def filterKeys(p: String => Boolean): C =
    wrap(value.view.filterKeys(p).toMap)

  override def nestedValueMap[T, U](pf: PartialFunction[T, U])(implicit D1: DCodec[T], D2: DCodec[U]): C =
    wrap(DataOps.nestedValueMap(value, pf).asInstanceOf[RawObject])

  override def nestedKeyValueMap[T, U](
    pf: PartialFunction[(String, T), Option[(String, U)]]
  )(implicit D1: DCodec[T], D2: DCodec[U]): C =
    wrap(DataOps.nestedKeyValueMap(value, pf).asInstanceOf[RawObject])

  override def nestedKeyMap(pf: PartialFunction[String, Option[String]]): C =
    wrap(DataOps.nestedKeyMap(value, pf).asInstanceOf[RawObject])
}

trait DObject extends Any with DObjectOps[DObject] {
  override protected def coll: this.type = this
}

/**
 * Deltas are collection of changes to apply to a DObject
 * The are neither Commutative or Associative
 * Deltas should never be combined to give a new Delta as information can be lost
 */
trait Delta extends Any with DObjectOps[Delta] {
  override protected def coll: this.type = this

  override def toString: String =
    s"Delta :- ${super.toString}"
}

final class DObjectInst private[dsentric] (val value: RawObject) extends AnyVal with DObject {
  @inline
  protected def wrap(value: RawObject): DObject =
    new DObjectInst(value)

}

final class DeltaInst private[dsentric] (val value: RawObject) extends AnyVal with Delta {

  protected def wrap(value: RawObject): Delta = new DeltaInst(value)

}

sealed trait ProjectionWildcard {
  def matches(key: String): Boolean
}
case object NoWildcard                      extends ProjectionWildcard {
  def matches(key: String): Boolean = false
}
final case class DefinedWildcard(s: String) extends ProjectionWildcard {
  def matches(key: String): Boolean = s == key
}

final class DProjection private[dsentric] (val wildCard: ProjectionWildcard, val value: RawObject)
    extends DObject
    with DObjectOps[DProjection] {
  projection =>

  protected def wrap(value: RawObject) = new DProjection(wildCard, value)

  def &(key: String): DProjection =
    wrap(value + (key -> 1L))

  //TODO BUG should concat not replace.
  def &(path: Path): DProjection  =
    wrap(value ++ PathLensOps.pathToMap(path, 1L))

  //Nest projection into a new object under the given key
  def nest(key: String): DProjection =
    wrap(Map(key -> value))

  def nest(path: Path): DProjection  =
    wrap(PathLensOps.pathToMap(path, value))

  /**
   * Extracts a possible sub projection as the key value
   * @param key
   * @return Left true, if key is simply selected, false if not selected at all and Right if
   *         the selection is a projection.
   */
  def unnest(key: String): Either[Boolean, DProjection] =
    value.get(key) match {
      case Some(m: RawObject @unchecked) => Right(wrap(m))
      case Some(1L)                      => Left(true)
      case None                          =>
        resolveWildCard(value).map(wrap)
      case _                             => Left(false)
    }

  /**
   * Extracts a possible sub projection for the path
   * @param path
   * @return Left true, the path and any sub paths are all selected, false if path is not selected,
   *         Right if the path leads to a projection.
   */
  def unnest(path: Path): Either[Boolean, DProjection] =
    selectsTraverse(value, path).map(wrap)

  def &(d: DProjection): DProjection = {
    val wc = wildCard -> d.wildCard match {
      case (d: DefinedWildcard, _) => d
      case (_, d: DefinedWildcard) => d
      case _                       => NoWildcard
    }
    new DProjection(wc, RawObjectOps.traverseConcat(value, d.value))
  }

  def select[D <: DObject]: PathSetter[D] =
    SelectPathSetter(this)

  def omit[D <: DObject]: PathSetter[D] =
    OmitPathSetter(this)

  /*
  Value on the left hand side is the selection by the projection, the value on the right has the values exluded
   */
  def partition[D <: DObject](obj: D): (D, D) =
    this.select(obj) -> this.omit(obj)

  def partition(obj: Delta): (Delta, Delta) =
    (this.select[DObject] |>> obj) -> (this.omit[DObject] |>> obj)

  def toPaths: Set[Path] =
    getPaths(value, Path.empty).getOrElse(Set.empty)

  def toDObject: DObject =
    new DObjectInst(value)

  /**
   * Returns true if the key is expressed in the projection, similar to contains, however
   * takes into account wildcard and noop values
   * @param key
   * @return
   */
  def selects(key: String): Boolean =
    value.get(key) match {
      case None                          =>
        resolveWildCard(value).fold(identity, _ => true)
      case Some(1L)                      => true
      case Some(_: RawObject @unchecked) => true
      case _                             => false
    }

  /**
   * Returns true if the key is expressed in the projection, similar to contains, however
   * takes into account wildcard and noop values
   *
   * @param key
   * @return
   */
  def selects(path: Path): Boolean =
    selectsNested(selects)(value, path)

  /**
   * Returns true if the key is expressed in the projection as a leaf key pair only
   * @param key
   * @return
   */
  def selectsValue(key: String): Boolean =
    value.get(key) match {
      case None     =>
        resolveWildCard(value).fold(identity, _ => false)
      case Some(1L) => true
      case _        => false
    }

  /**
   * Returns true if the whole path is expressed in the projection as a leaf key pair only
   * @param key
   * @return
   */
  def selectsValue(path: Path): Boolean =
    selectsNested(selectsValue)(value, path)

  private def selectsNested(tailF: String => Boolean)(projection: RawObject, path: Path): Boolean =
    path match {
      case PathKey(key, PathEnd) =>
        tailF(key)
      case PathKey(key, tail)    =>
        projection.get(key) match {
          case None                          =>
            resolveWildCard(projection).fold(identity, selectsNested(tailF)(_, tail))
          case Some(1L)                      => true
          case Some(m: RawObject @unchecked) =>
            selectsNested(tailF)(m, tail)
          case _                             =>
            false
        }
      case _                     => false
    }

  private def selectsTraverse(projection: RawObject, path: Path): Either[Boolean, RawObject] =
    path match {
      case PathKey(key, PathEnd) =>
        projection.get(key) match {
          case None                          => resolveWildCard(projection)
          case Some(1L)                      => Left(true)
          case Some(m: RawObject @unchecked) => Right(m)
          case _                             => Left(false)
        }
      case PathKey(key, tail)    =>
        projection.get(key) match {
          case None                          =>
            resolveWildCard(projection)
              .flatMap(selectsTraverse(_, tail))
          case Some(1L)                      =>
            Left(true)
          case Some(m: RawObject @unchecked) =>
            selectsTraverse(m, tail)
          case _                             =>
            Left(false)
        }
      case _                     =>
        Left(false)
    }

  private def resolveWildCard(layer: RawObject): Either[Boolean, RawObject] =
    wildCard match {
      case DefinedWildcard(s) =>
        layer.get(s) match {
          case Some(1L)                      => Left(true)
          case Some(m: RawObject @unchecked) => Right(m)
          case _                             => Left(false)
        }
      case _                  =>
        Left(false)
    }

  private def getPaths(projection: RawObject, segments: Path): Option[Set[Path]] = {
    val pairs = projection.flatMap {
      case (key, 1)                       =>
        Some(Set(segments \ key))
      case (key, j: RawObject @unchecked) =>
        getPaths(j, segments \ key)
      case _                              =>
        None
    }.toList
    if (pairs.length < projection.size) None
    else Some(pairs.reduce(_ ++ _))
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case c: DProjection =>
        c.wildCard == this.wildCard && c.value == this.value
      case _              =>
        false
    }

  override def hashCode(): Int =
    value.hashCode()
}

class DArray(val value: RawArray) extends AnyVal with Data {

  def toObjects: Iterator[DObject] =
    value.iterator
      .collect { case m: RawObject @unchecked =>
        new DObjectInst(m)
      }

  def toValues[T](implicit D: DCodec[T]): Iterator[T] =
    value.iterator.flatMap(D.unapply)

  def toDataValues: Iterator[Data] =
    toValues(DataCodec)

  override def nestedValueMap[T, U](pf: PartialFunction[T, U])(implicit D1: DCodec[T], D2: DCodec[U]): DArray =
    new DArray(DataOps.nestedValueMap(value, pf).asInstanceOf[Vector[Any]])

  def intersect(dArray: DArray): DArray =
    new DArray(value.intersect(dArray.value))

  def nonEmpty: Boolean =
    value.nonEmpty

  def intersects(dArray: DArray): Boolean =
    intersect(dArray).nonEmpty

  def contains(data: Data): Boolean =
    value.contains(data.value)
}

sealed trait DNullable[+T] extends IterableOnce[T] with Product with Serializable {
  def isNull: Boolean
  def toOption: Option[T]
  def map[S](f: T => S): DNullable[S] =
    flatMap(t => DSome(f(t)))
  def flatMap[S](f: T => DNullable[S]): DNullable[S]
  def getOrElse[S >: T](default: => S): S
}

case object DNull extends Data with DNullable[Nothing] {
  override def value: Data      = this
  override def toString: String = "null"
  override def isNull: Boolean  = true

  def toOption: Option[Nothing] = None

  def flatMap[S](f: Nothing => DNullable[S]): DNullable[S] = DNull

  def getOrElse[S >: Nothing](default: => S): S = default

  def iterator: Iterator[Nothing] =
    Iterator.empty
}

final case class DSome[T](t: T) extends DNullable[T] {
  override def isNull: Boolean = false

  def toOption: Option[T] = Some(t)

  def flatMap[S](f: T => DNullable[S]): DNullable[S] =
    f(t)

  def getOrElse[S >: T](default: => S): S =
    t

  def iterator: Iterator[T] =
    Iterator(t)
}

object Data {
  def apply[T](value: T)(implicit codec: DCodec[T]): Data =
    codec.apply(value) match {
      case obj: RawObject @unchecked =>
        new DObjectInst(obj)
      case arr: RawArray @unchecked  =>
        new DArray(arr)
      case DNull                     => DNull
      case a                         => new DValue(a)
    }
}

object DObject {

  val empty: DObject =
    new DObjectInst(Map.empty[String, Any])

  def apply(values: (String, Data)*): DObject          =
    new DObjectInst(values.iterator.map(p => p._1 -> p._2.value).toMap)

  def apply(values: Iterator[(String, Data)]): DObject =
    new DObjectInst(values.map(p => p._1 -> p._2.value).toMap)

  def apply(map: Map[String, Data]): DObject           =
    new DObjectInst(map.view.mapValues(_.value).toMap)
}

object DArray {

  val empty = new DArray(Vector.empty)

  def apply(values: Iterator[Data]): DArray =
    new DArray(values.map(_.value).toVector)

  def apply[T](values: T*)(implicit codec: DCodec[T]) =
    new DArray(values.map(codec.apply).toVector)
}

object DProjection {

  val empty = new DProjection(NoWildcard, Map.empty)

  def empty(wildCard: String): DProjection = new DProjection(DefinedWildcard(wildCard), Map.empty)

  def apply(paths: Path*): DProjection =
    new DProjection(
      NoWildcard,
      paths
        .map(p => PathLensOps.pathToMap(p, 1L))
        .foldLeft(Map.empty[String, Any])(RawObjectOps.traverseConcat)
    )

  def apply(wildCard: ProjectionWildcard, paths: Path*): DProjection =
    new DProjection(
      wildCard,
      paths
        .map(p => PathLensOps.pathToMap(p, 1L))
        .foldLeft(Map.empty[String, Any])(RawObjectOps.traverseConcat)
    )

  def fromKeys(keys: String*): DProjection                               =
    new DProjection(NoWildcard, keys.map(_ -> 1L).toMap)

  def fromKeys(wildCard: ProjectionWildcard, keys: String*): DProjection =
    new DProjection(wildCard, keys.map(_ -> 1L).toMap)

  def fromObject(d: DObject): DProjection                                =
    new DProjection(NoWildcard, d.value)

  def fromObject(wildCard: ProjectionWildcard, d: DObject): DProjection =
    new DProjection(wildCard, d.value)
}

object Delta {
  val empty: Delta = new DeltaInst(RawObject.empty)

  def apply(values: (String, Data)*): Delta          =
    new DeltaInst(values.iterator.map(p => p._1 -> p._2.value).toMap)

  def apply(values: Iterator[(String, Data)]): Delta =
    new DeltaInst(values.map(p => p._1 -> p._2.value).toMap)

  def fromObject(d: DObject): Delta                  =
    new DeltaInst(d.value)

}

object ForceWrapper {

  def data(value: Any): Data =
    value match {
      case d: Data                 =>
        d
      case m: RawObject @unchecked =>
        dObject(m)
      case v: RawArray @unchecked  =>
        dArray(v)
      case a: Any                  =>
        dValue(a)
    }

  def dObject(value: RawObject): DObject =
    new DObjectInst(value)

  def delta(value: RawObject): Delta =
    new DeltaInst(value)

  def dValue(value: Raw): DValue =
    new DValue(value)

  def dArray(value: RawArray): DArray =
    new DArray(value)

  def dFilter(value: RawObject): DFilter =
    new DFilter(value)

  def dProjection(value: RawObject): DProjection =
    new DProjection(NoWildcard, value)

  def dProjection(wildCard: ProjectionWildcard, value: RawObject): DProjection =
    new DProjection(wildCard, value)
}
