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

  def deltaTraverseConcat(x: Raw, delta: Raw): Option[Raw] =
    (x, delta) match {
      case (xObj:RawObject@unchecked, yObj:RawObject@unchecked) =>
        val reducedObject = RawObjectOps.deltaTraverseConcat(xObj, yObj)
        if (reducedObject.isEmpty) None
        else Some(reducedObject)
      case (_, DNull) => None
      case (_, y) => Some(y)
    }
}

object RawOps extends RawOps
