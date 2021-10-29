package dsentric

trait RawOps {

  def reducesEmpty(target:Raw):Boolean =
    target match {
      case DNull => true
      case rawObject:RawObject@unchecked =>
        RawObjectOps.reducesEmpty(rawObject)
      case _ =>
        false
    }
}

object RawOps extends RawOps
