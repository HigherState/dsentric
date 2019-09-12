package dsentric

import scala.util.matching.Regex
import queryTree._
import queryTree.Tree

trait PropertyExtension[T] extends Any {
  def prop:Property[_, T]
  protected def nest(value:Raw):DQuery =
    new DQuery(PathLensOps.pathToMap(prop._path, value))

}

trait Query {

  implicit def valueQuery[T](prop:Property[_, T]):ValueQuery[T] =
    new ValueQuery(prop)

  implicit def maybeQuery[T](prop:Maybe[_, T]):MaybeQuery[T] =
    new MaybeQuery(prop)

  implicit def numericQuery[T >: Numeric](prop:Property[_, T]):NumericQuery[T] =
    new NumericQuery(prop)

  implicit def stringQuery[T >: Optionable[String]](prop:Property[_, T]):StringQuery[T] =
    new StringQuery(prop)

  implicit class ArrayQuery[T](val prop: Expected[_, Vector[T]])(implicit codec: DCodec[T]) extends PropertyExtension[Vector[T]]  {

    def $elemMatch(f:Property[_, T] => DQuery):DQuery =
      nest(Map("$elemMatch" -> f(new EmptyProperty[T]).value))

    def $exists(isTrue:Boolean):DQuery =
      nest(Map("$exists" -> isTrue))
  }

  implicit class MaybeArrayQuery[ T](val prop: Maybe[_, Vector[T]])(implicit codec: DCodec[T]) extends PropertyExtension[Vector[T]]  {

    def $elemMatch(f:Property[_, T] => DQuery):DQuery =
      nest(Map("$elemMatch" -> f(new EmptyProperty[T]).value))

    def $exists(isTrue:Boolean):DQuery =
      nest(Map("$exists" -> isTrue))
  }

  implicit class JArrayQuery(val prop: Expected[_, DArray]) extends PropertyExtension[DArray]  {

    def $elemMatch(f:Property[_, Data] => DQuery):DQuery =
      nest(Map("$elemMatch" -> f(new EmptyProperty[Data]()(DefaultCodecs.dataCodec)).value))

    def $exists(isTrue:Boolean):DQuery =
      nest(Map("$exists" -> isTrue))

  }

  implicit class MaybeJArrayQuery(val prop: Maybe[_, DArray]) extends PropertyExtension[DArray]  {

    def $elemMatch(f:Property[_, Data] => DQuery):DQuery =
      nest(Map("$elemMatch" -> f(new EmptyProperty[Data]()(DefaultCodecs.dataCodec)).value))

    def $exists(isTrue:Boolean):DQuery =
      nest(Map("$exists" -> isTrue))
  }

  implicit class ObjectArrayQuery[D <: DObject, T <: ContractFor[D]](val prop: ExpectedObjectArray[D, T]) {

    def $elemMatch(f:T => DQuery):DQuery =
      nest(Map("$elemMatch" -> f(prop.contract).value))

    def $exists(isTrue:Boolean):DQuery =
      nest(Map("$exists" -> isTrue))

    protected def nest(value:Any):DQuery =
      new DQuery(PathLensOps.pathToMap(prop._path, value))

  }

  implicit class MaybeObjectArrayQuery[T <: Contract](val prop: MaybeObjectArray[_, T]) {

    def $elemMatch(f:T => DQuery):DQuery =
      nest(Map("$elemMatch" -> f(prop.contract).value))

    def $exists(isTrue:Boolean):DQuery =
      nest(Map("$exists" -> isTrue))

    protected def nest(value:Any):DQuery =
      new DQuery(PathLensOps.pathToMap(prop._path, value))

  }

  implicit class DefaultObjectArrayQuery[T <: Contract](val prop: DefaultObjectArray[_, T]) {

    def $elemMatch(f:T => DQuery):DQuery =
      nest(Map("$elemMatch" -> f(prop.contract).value))

    def $exists(isTrue:Boolean):DQuery =
      nest(Map("$exists" -> isTrue))

    protected def nest(value:Any):DQuery =
      new DQuery(PathLensOps.pathToMap(prop._path, value))
  }

}

object Query extends Query {
  private[dsentric] def apply(value:Option[Any], query:RawObject):Boolean = {
    query.forall {
      case ("$and", values:RawArray@unchecked) =>
        values.collect{ case m:RawObject@unchecked => m}.forall(apply(value, _))
      case ("$or", values:RawArray@unchecked) =>
        values.collect{ case m:RawObject@unchecked => m}.exists(apply(value, _))
      case ("$eq", v) =>
        value.contains(v)
      case ("$ne", v) =>
        !value.contains(v)
      case ("$regex" | "$options", _) =>
        query.get("$regex").collect{
          case v:String =>
            val options = query.get("$options").collect{ case o:String => s"(?$o)" }.getOrElse("")
            value.collect { case s:String => (options + v).r.pattern.matcher(s).matches }.getOrElse(false)
        }.getOrElse(false)

      case ("$like", v:String) =>
        value.collect {
          case s:String =>
            ("(?i)" + v.replace("%", ".*")).r.pattern.matcher(s).matches
        }.getOrElse(false)
      case ("$lt", v) =>
        value.exists(x => order(x -> v).contains(-1))
      case ("$gt", v) =>
        value.exists(x => order(x -> v).contains(1))
      case ("$lte", v) =>
        value.exists(x => order(x -> v).exists(r => r <= 0))
      case ("$gte", v) =>
        value.exists(x => order(x -> v).exists(r => r >= 0))
      case ("$in", values:RawArray@unchecked) =>
        value.exists(j => values.contains(j))
      case ("$nin", values:RawArray@unchecked) =>
        !value.exists(j => values.contains(j)) //nin doesnt require existence, as per mongodb
      case ("$exists", v:Boolean) =>
        value.isDefined == v
      case ("$not", v:RawObject@unchecked) =>
        !apply(value, v)
      case ("$elemMatch", v:RawObject@unchecked) =>
        value.collect { case seq:RawArray@unchecked => seq.exists(s => apply(Some(s), v)) }.getOrElse(false)
      case ("$elemMatch", v) =>
        value.collect { case seq:RawArray@unchecked => seq.contains(v) }.getOrElse(false)
      case (key, v:RawObject@unchecked) =>
        apply(value.collect{ case m:RawObject@unchecked => m}.flatMap(_.get(key)), v)
      case (key, v) =>
        value.collect{ case m:RawObject@unchecked => m}.fold(false) { l =>
          l.get(key).contains(v)
        }
    }
  }

  private[dsentric] def apply(value:Any, query:Tree):Boolean = {
    import Dsentric._
    query match {
      case &(trees) =>
        trees.forall(t => this(value, t))
      case |(trees) =>
        trees.exists(t => this(value, t))
      case !!(tree) =>
        !this(value, tree)
      case ?(path, "$eq", v) =>
        search(value -> path).exists(x => order(x -> v).contains(0))
      case ?(path, "$ne", v) =>
        !search(value -> path).exists(x => order(x -> v).contains(0))
      case /(path, regex) =>
        search(value -> path).collect{case s:String => s}.exists(s => regex.pattern.matcher(s).matches)
      case In(path, values) =>
        values.forall{ kv =>
          search(value -> (path \ kv._1)).exists(x => order(x -> kv._2).contains(0))
        }
      case %(path, _, regex) =>
        search(value -> path).collect{case s:String => s}.exists(s => regex.pattern.matcher(s).matches)
      case ?(path, "$lt", v) =>
        search(value -> path).exists(x => order(x -> v).contains(-1))
      case ?(path, "$gt", v) =>
        search(value -> path).exists(x => order(x -> v).contains(1))
      case ?(path, "$lte", v) =>
        search(value -> path).exists(x => order(x -> v).exists(r => r <= 0))
      case ?(path, "$gte", v) =>
        search(value -> path).exists(x => order(x -> v).exists(r => r >= 0))
      case ?(path, "$in", values:RawArray@unchecked) =>
        search(value -> path).exists(j => values.contains(j))
      case ?(path, "$nin", values:RawArray@unchecked) =>
        !search(value -> path).exists(j => values.contains(j))
      case ?(path, "$exists", v:Boolean) =>
        search(value -> path).nonEmpty == v
      case Exists(path, subQuery) =>
        search(value -> path).collect { case v:RawArray => v.exists(s => apply(s, subQuery)) }.getOrElse(false)
      case _ =>
        false
    }
  }

  private def search:Function[(Any, Path), Option[Any]] = {
    case (v, PathEnd) =>
      Some(v)
    case (v:RawObject@unchecked, p) =>
      PathLensOps.traverse(v, p)
    case _ => None
  }

  private val order = DValueOps.order.lift

}

//Handle default?
class ValueQuery[T](val prop: Property[_, T]) extends AnyVal with PropertyExtension[T] {

  def $eq(value:T): DQuery =
    if (prop.isInstanceOf[EmptyProperty[_]])
      new DQuery(Map("$eq" -> prop._codec(value).value))
    else
      nest(prop._codec(value).value)
  //Currently not supporting chaining of $ne in an && for the same field
  def $ne(value:T): DQuery =
    nest(Map("$ne" -> prop._codec(value).value))
  def $in(values:T*): DQuery =
    nest(Map("$in" -> values.map(prop._codec(_).value).toVector))
  def $nin(values:T*): DQuery =
    nest(Map("$nin" -> values.map(prop._codec(_).value).toVector))

}

class MaybeQuery[T](val prop:Maybe[_, T]) extends AnyVal with PropertyExtension[T] {
  def $exists(value:Boolean): DQuery = nest(Map("$exists" -> value))
}

class NumericQuery[T >: Numeric](val prop: Property[_, T]) extends AnyVal with PropertyExtension[T] {

  def $lt(value:Double): DQuery = nest(Map("$lt" -> value))
  def $lt(value:Long): DQuery = nest(Map("$lt" -> value))

  def $gt(value:Double): DQuery = nest(Map("$gt" -> value))
  def $gt(value:Long): DQuery = nest(Map("$gt" -> value))

  def $lte(value:Double): DQuery = nest(Map("$lt" -> value))
  def $lte(value:Long): DQuery = nest(Map("$lt" -> value))

  def $gte(value:Double): DQuery = nest(Map("$gt" -> value))
  def $gte(value:Long): DQuery = nest(Map("$gt" -> value))
}

class StringQuery[T >: Optionable[String]](val prop:Property[_, T]) extends AnyVal with PropertyExtension[T] {

  def $regex(value:String): DQuery = nest(Map("$regex" -> value))
  def $regex(value:String, options:String): DQuery = nest(Map("$regex" -> value, "$options" -> options))
  def $regex(r:Regex): DQuery = nest(Map("$regex" -> r.regex))
  def $like(value:String): DQuery = nest(Map("$like" -> value))

}