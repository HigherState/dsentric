package dsentric

/**
  * Created by jamie.pullar on 12/05/2016.
  */
trait JsonOps {

  def order(x:Json, y:Json):Option[Int] = {
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

object JsonOps extends JsonOps
