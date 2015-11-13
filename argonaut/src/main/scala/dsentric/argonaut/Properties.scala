package dsentric.argonaut

import _root_.argonaut._
import Index._

object \ extends dsentric.ExpectedDsl[Json, JsonObject]

object \! extends dsentric.DefaultDsl[Json, JsonObject]

object \? extends dsentric.MaybeDsl[Json, JsonObject]

abstract class Contract extends dsentric.Contract[Json, JsonObject]
