package dsentric

import monocle.Prism
import monocle.function.{Each, At, Empty}
import scalaz.Scalaz._

trait ObjectOps {

  def applyDelta[Data, IndexedData]
    (target: Data, delta: Data, deltaDelete: Option[Data])
    (implicit prism: Prism[Data, IndexedData], empty: Empty[IndexedData], at: At[IndexedData, String, Option[Data]], each: Each[IndexedData, (String, Data)]): Data = {
      val traversal = each.each
      val emp = empty.empty.reverseGet(())

      def deltaRecurse(target: Data, delta: Data):Data =
        (prism.getOption(target), prism.getOption(delta)) match {
          case (Some(t), Some(d)) =>
            val newData = each.each.getAll(d).foldLeft(t) {
              case (acc, (k, v)) if deltaDelete.contains(v) =>
                at.at(k).set(None)(acc)
              case (acc, (k, v)) =>
                val lens = at.at(k)
                lens.get(acc).fold(lens.set(Some(v))(acc)) { c =>
                  val r = deltaRecurse(c, v)
                  at.at(k).set((r != emp).option(r))(acc)
                }
            }
            prism.reverseGet(newData)
          case _ =>
            delta
        }

      deltaRecurse(target, delta)
    }
  /*
    provides those values in source which do not match those in target
   */
  def deltaDifference[Data, IndexedData]
    (delta: Data, target: Data, deltaDelete: Option[Data])
    (implicit prism: Prism[Data, IndexedData], empty: Empty[IndexedData], at: At[IndexedData, String, Option[Data]], each: Each[IndexedData, (String, Data)]): Option[Data] = {
      val traversal = each.each
      val emp = empty.empty.reverseGet(())

    def diffRecurse(delta: Data, target: Data):Option[Data] = {
      if (delta == target) None
      else
        (prism.getOption(delta), prism.getOption(target)) match {
          case (Some(d), Some(t)) =>
            val newDelta = each.each.getAll(d).foldLeft(d){ (a, kv) =>
              val lens = at.at(kv._1)
              lens.get(t).fold{
                if (deltaDelete.contains(kv._2))
                  lens.set(None)(a)
                else
                  a
              } { c =>
                val diff = diffRecurse(kv._2, c)
                if (diff != kv._2)
                  lens.set(diff)(a)
                else
                  a
              }
            }
            (newDelta != emp).option(prism.reverseGet(newDelta))
          case _ =>
            Some(target)
        }
    }
    diffRecurse(delta, target)
  }
}
