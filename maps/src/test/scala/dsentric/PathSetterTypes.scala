package dsentric

import dsentric.contracts.{ContractFor, PathSetter}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * Created by stefano.paluello on 03/10/2016.
 */
class PathSetterTypesTest extends AnyFunSuite with Matchers {
  import dsentric.codecs.std.DCodecs._
  trait Bob extends DObject with DObjectOps[Bob]

  case class Fred(value: RawObject) extends Bob with DObjectOps[Fred] {
    protected def wrap(value: RawObject): Fred = Fred(value)
  }

  object BobContract extends ContractFor[Bob] {
    val a = \[Int]
  }

  object FredContract extends ContractFor[Fred] {
    val b = \[String]
  }
  val first: PathSetter[Fred] = FredContract.b.$set("hi")
  val second: PathSetter[Fred] = first ~ FredContract.b.$set("hi")
  second ~ BobContract.a.$set(2)
  second.apply(Fred(RawObject.empty))

}
