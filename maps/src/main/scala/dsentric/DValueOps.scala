package dsentric

/**
  * Created by jamie.pullar on 12/05/2016.
  */
trait DValueOps {

  def order(x:DValue, y:DValue):Option[Int] = {
    order.lift(x.value -> y.value)
  }

  private[dsentric] def order:PartialFunction[(Any, Any), Int] = {
    case (x:Double, y:Double) =>
      Ordering.Double.compare(x, y)
    case (x:Long, y:Long) =>
      Ordering.Long.compare(x, y)
    case (x:String, y:String) =>
      Ordering.String.compare(x, y)
    case (x:Boolean, y:Boolean) =>
      Ordering.Boolean.compare(x, y)
    case (x:Double, y:Long) =>
      Ordering.Double.compare(x, y)
    case (x:Long, y:Double) =>
      Ordering.Double.compare(x, y)
  }
}

object DValueOps extends DValueOps

trait DataOps {

  def nestedContains[T](value:Data, t:T)(implicit D:DCodec[T]):Boolean =
    valueContains(value.value, t, D)

  private[dsentric] def valueContains[T](target:Any, t:T, D:DCodec[T]):Boolean =
    target match {
      case D(v) =>
        v == t
      case m:Map[String, Any]@unchecked =>
        m.exists(p => valueContains(p._2, t, D))
      case v:Vector[Any]@unchecked =>
        v.exists(e => valueContains(e, t, D))
      case _ =>
        false
    }

  private[dsentric] def nestedValueMap[T, U](value:Any, pf:PartialFunction[T, U])(implicit D1:DCodec[T], D2:DCodec[U]):Any =
    value match {
      case a@D1(v) =>
        pf.lift(v).map(D2(_).value).getOrElse(a)
      case m:Map[String, Any]@unchecked =>
        m.mapValues(nestedValueMap(_,pf))
      case v:Vector[Any]@unchecked =>
        v.map(nestedValueMap(_,pf))
      case v =>
        v
    }
}

object DataOps extends DataOps
