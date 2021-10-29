package dsentric.codecs.std

import dsentric.codecs.DMatcher

trait DMatchers {
  import DValueCodecs._
  val DString: DMatcher[String] =
    DMatcher[String]()

  val DBoolean: DMatcher[Boolean] =
    DMatcher[Boolean]()

  val DLong: DMatcher[Long] =
    DMatcher[Long]()

  val DDouble: DMatcher[Double] =
    DMatcher[Double]()

  val DNumber: DMatcher[Number] =
    DMatcher[Number]()
}
