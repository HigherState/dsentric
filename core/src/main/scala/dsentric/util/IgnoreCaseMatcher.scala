package dsentric.util


class StringContextOps(val sc: StringContext) extends AnyVal {
  def i = IgnoreCaseMatcher(sc.raw())
}

case class IgnoreCaseMatcher(value:String) {
  def unapply(s:String):Boolean =
    value.equalsIgnoreCase(s)
}

trait ToStringContextOps {

  implicit def toIgnoreCase(sc:StringContext):StringContextOps =
    new StringContextOps(sc)
}

object ToStringContextOps extends ToStringContextOps