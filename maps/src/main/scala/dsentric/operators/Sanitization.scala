package dsentric.operators

import dsentric._
import dsentric.contracts._

object Sanitization {

  def sanitizeContract[D <: DObject](contract:BaseContract[D], value:RawObject):Option[RawObject] = {

    def getSanitizer[D2 <: DObject, T](property: Property[D2, T]): Option[Sanitizer[T]] =
      property._dataOperators.collectFirst { case s: Sanitizer[T]@unchecked => s }

    def applySanitizer[T](field: String, value: RawObject, sanitizer: Sanitizer[T]): Option[RawObject] = {
      val propertyValue = value.get(field)
      sanitizer.sanitize(propertyValue) match {
        case v if v == propertyValue =>
          None
        case None =>
          Some(value - field)
        case Some(raw) =>
          Some(value + (field -> raw))
      }
    }

    contract._fields.foldLeft[Option[RawObject]](None) {
      case (maybeObj, (field, property: (BaseContract[DObject] & ExpectedObjectProperty[D]) @unchecked)) =>
        getSanitizer(property) match {
          case None =>
            val obj =
              value.get(field).collect { case rv: RawObject@unchecked => rv }.getOrElse(RawObject.empty)
            sanitizeContract(property, obj).map(r => maybeObj.getOrElse(value) + (field -> r)).orElse(maybeObj)
          case Some(sanitizer) =>
            applySanitizer(field, maybeObj.getOrElse(value), sanitizer)
              .orElse(maybeObj)
        }

      case (maybeObj, (field, property: (BaseContract[DObject]@unchecked & Property[D, ?]))) =>
        getSanitizer(property) match {
          case None =>
            value.get(field).collect {
              case rv: RawObject@unchecked =>
                sanitizeContract(property, rv).map(r => maybeObj.getOrElse(value) + (field -> r))
            }.flatten.orElse(maybeObj)
          case Some(sanitizer) =>
            applySanitizer(field, maybeObj.getOrElse(value), sanitizer)
              .orElse(maybeObj)
        }

      case (maybeObj, (field, property)) =>
        getSanitizer(property).flatMap { sanitizer =>
          applySanitizer(field, maybeObj.getOrElse(value), sanitizer)
            .orElse(maybeObj)
        }
    }
  }
}
