package dsentric

import dsentric.codecs.DCodec

/**
  * Created by jamie.pullar on 12/05/2016.
  */
trait DValueOps {

  def order(x:DValue, y:DValue):Option[Int] = {
    order.lift(x.value -> y.value)
  }

  private[dsentric] def order:PartialFunction[(Raw, Raw), Int] = {
    case (x:Double, y:Double) =>
      Ordering.Double.TotalOrdering.compare(x, y)
    case (x:Long, y:Long) =>
      Ordering.Long.compare(x, y)
    case (x:String, y:String) =>
      Ordering.String.compare(x, y)
    case (x:Boolean, y:Boolean) =>
      Ordering.Boolean.compare(x, y)
    case (x:Double, y:Long) =>
      Ordering.Double.TotalOrdering.compare(x, y)
    case (x:Long, y:Double) =>
      Ordering.Double.TotalOrdering.compare(x, y)
  }
}

object DValueOps extends DValueOps

trait DataOps {

  def nestedContains[T](value:Data, t:T)(implicit D:DCodec[T]):Boolean =
    valueContains(value.value, t, D)

  private[dsentric] def valueContains[T](target:Raw, t:T, D:DCodec[T]):Boolean =
    target match {
      case D(v) =>
        v == t
      case m:RawObject@unchecked =>
        m.exists(p => valueContains(p._2, t, D))
      case v:RawArray@unchecked =>
        v.exists(e => valueContains(e, t, D))
      case _ =>
        false
    }

  private[dsentric] def nestedValueMap[T, U](value:Raw, pf:PartialFunction[T, U])(implicit D1:DCodec[T], D2:DCodec[U]):Raw =
    value match {
      case a@D1(v) =>
        pf.lift(v).map(D2(_)).getOrElse(a)
      case m:RawObject@unchecked =>
        m.view.mapValues(nestedValueMap(_,pf)).toMap
      case v:RawArray@unchecked =>
        v.map(nestedValueMap(_,pf))
      case v =>
        v
    }

  private[dsentric] def nestedKeyValueMap[T, U](value:Any, pf:PartialFunction[(String, T), Option[(String, U)]])(implicit D1:DCodec[T], D2:DCodec[U]):Raw =
    value match {
      case m:RawObject@unchecked =>
        m.flatMap { kv =>
          val newPair = D1.unapply(kv._2).flatMap(t => pf.lift(kv._1 -> t))
          newPair.fold[Option[(String, Any)]](Some(kv._1 -> nestedKeyValueMap(kv._2, pf)))(_.map(r => r._1 -> D2(r._2)))
        }
      case v:RawArray@unchecked =>
        v.map(nestedKeyValueMap(_,pf))
      case v =>
        v
    }

  private[dsentric] def nestedKeyMap(value:Any, pf:PartialFunction[String, Option[String]]):Raw =
    value match {
      case m:RawObject@unchecked =>
        m.flatMap { kv =>
          pf.lift(kv._1).fold[Option[(String, Any)]](Some(kv._1 -> nestedKeyMap(kv._2, pf)))(v => v.map(nk => nk -> nestedKeyMap(kv._2, pf)))
        }
      case v:RawArray@unchecked =>
        v.map(nestedKeyMap(_,pf))
      case v =>
        v
    }
}

object DataOps extends DataOps
