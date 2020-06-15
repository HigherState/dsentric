package dsentric.contracts

import dsentric.{Dsentric, PessimisticCodecs}


/**
 * Completely dynamic
 * Partially dynamic
 */
class AdditionalValuePropertiesContract extends Contract with AdditionalProperties {
  import Dsentric._
  import PessimisticCodecs._
  import dsentric.Implicits._

  val internal = new \\ {
    val bob = \[String]

    val $additionalProperties =
      new ValueProperties[String, Int]()
  }
}

/**
 *  A Value
 *
 *  Contract.$additionalProperties("key").$set(value)
 *  Contract.$additionalProperties("key").$drop(value)
 *  Contract.$additionalProperties("key").$modify(v => v)
 */

/**
 * An Object
 *
 * Contract.$additionalProperties("key").$contractModify(
 */
