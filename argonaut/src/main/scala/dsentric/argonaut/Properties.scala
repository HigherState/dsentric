package dsentric.argonaut

import dsentric._
import _root_.argonaut._

import Index._

object \ extends ExpectedDsl[Json, JsonObject]

object \! extends DefaultDsl[Json, JsonObject]

object \? extends MaybeDsl[Json, JsonObject]

abstract class Contract extends dsentric.Contract[Json, JsonObject]
