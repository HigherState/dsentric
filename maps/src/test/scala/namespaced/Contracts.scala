package namespaced

import dsentric.SubContract
import dsentric.PessimisticCodecs._

trait AnotherNested extends SubContract {
  val prop = \[Boolean]
}

trait AnotherNested2 extends SubContract {
  val prop2 = \[Boolean]
}

