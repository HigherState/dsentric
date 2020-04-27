package dsentric.contracts


/**
 * Completely dynamic
 * Partially dynamic
 */
class AdditionalPropertiesContract extends Contract {

  val internal = new \\{
    val bob = \[String]

    override def $additionalProperties =
      new \-\[String] {
      val
    }
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
