package dsentric.performance

/**
  * Created by Guillem.Iscla on 21/01/2016.
  */
object Timer {

  def print[T](name:String)(f: => T):T = {
    val start = System.currentTimeMillis()
    val t = f
    println(name + ": " + (System.currentTimeMillis() - start))
    t
  }
}
