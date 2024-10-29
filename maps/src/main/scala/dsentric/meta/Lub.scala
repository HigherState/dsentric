package dsentric.meta

trait Lub[-A, -B, Out] extends Serializable {
  def left(a : A): Out
  def right(b : B): Out
}

object Lub {
  implicit def lub[T]: Lub[T, T, T] = new Lub[T, T, T] {
    def left(a : T): T = a
    def right(b : T): T = b
  }
}