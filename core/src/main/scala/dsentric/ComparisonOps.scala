package dsentric

trait ComparisonOps {
//
//  def applyDelta[Data, IndexedData]
//    (target:Data, delta:Data, deltaDelete:Data)
//    (implicit prism:Prism[Data, IndexedData], empty:Empty[IndexedData], at:At[]):Data =
//      (target.obj, delta.obj) match {
//        case (Some(ot), Some(od)) =>
//          jObject(od.toList.foldLeft(ot)({
//            case (acc, (k, j)) if j.isNull =>
//              acc - k
//            case (acc, (k, v)) => acc(k) match {
//              case None => acc + (k, v)
//              case Some(l) =>
//                val d = applyDelta(l, v)
//                if (d == jEmptyObject)
//                  acc - k
//                else
//                  acc + (k, d)
//            }
//          }))
//        case _ =>
//          delta
//      }
  }
