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

  private[Dsentric] def valueContains[T](target:Any, t:T, D:DCodec[T]):Boolean =
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

//  def nestedMap[T, U](value:Data)(pf:PartialFunction[T, U])(implicit DT:DCodec[T], DU:DCodec[U]):Boolean =
//    valueMap(value.value, pf, DT, DU)
//
//  private[Dsentric] def valueMap[T, U](target:Any, pf:PartialFunction[T, U], DT:DCodec[T], DU:DCodec[U]):Option[U] =
//    target match {
//      case DT(t) =>
//        pf.lift(t)
//      case m:Map[String, Any]@unchecked =>
//        m.exists(p => valueContains(p._2, t, D))
//      case v:Vector[Any]@unchecked =>
//        v.exists(e => valueContains(e, t, D))
//      case _ =>
//        false
//    }
}
