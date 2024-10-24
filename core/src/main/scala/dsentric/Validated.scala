package dsentric

import scala.annotation.nowarn

@nowarn
case class Validated[D] private[dsentric] (validObject: D)
