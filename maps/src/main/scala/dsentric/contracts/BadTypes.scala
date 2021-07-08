package dsentric.contracts

/**
 * When ignoring bad types we only want it to ignore on nested entities.
 * The reason being we want to return immediate bad type information to Expected properties so that they return the
 * type failure as opposed to the Not Found failure
 */
private[contracts] sealed trait BadTypes {
  def nest:BadTypes
}
private[contracts] case object FailOnBadTypes extends BadTypes {
  def nest: BadTypes = this
}
private[contracts] case object ToDropBadTypes extends BadTypes {
  def nest: BadTypes = DropBadTypes
}
private[contracts] case object DropBadTypes extends BadTypes {
  def nest: BadTypes = this
}

