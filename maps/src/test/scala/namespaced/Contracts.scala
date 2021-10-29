package namespaced

import dsentric.Dsentric._
import dsentric.schema.{Description, Examples, Nested, Type}
import dsentric.codecs.std.DCodecs._

trait AnotherNested extends SubContract {
  @Description("Another nested prop")
  val prop = \[Boolean]
}

trait AnotherNested2 extends SubContract {
  @Description("Another nested 2 prop 2")
  val prop2 = \[Boolean]
}

@Nested
trait InheritedNested extends SubContract with AnotherNested {

  val propHigher = \[String]
}

@Type("Renamed")
trait ToRename extends SubContract with AnotherNested with AnotherNested2 {
  @Examples("one", "two")
  val propHigher = \?[String]
}
