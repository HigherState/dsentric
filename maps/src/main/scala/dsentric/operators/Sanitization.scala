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
      case (maybeObj, (field, property: BaseContract[DObject]@unchecked with ExpectedObjectProperty[D])) =>
        getSanitizer(property) match {
          case None =>
            val obj =
              value.get(field).collect { case rv: RawObject@unchecked => rv }.getOrElse(RawObject.empty)
            sanitizeContract(property, obj).map(r => maybeObj.getOrElse(value) + (field -> r)).orElse(maybeObj)
          case Some(sanitizer) =>
            applySanitizer(field, maybeObj.getOrElse(value), sanitizer)
              .orElse(maybeObj)
        }

      case (maybeObj, (field, property: BaseContract[DObject]@unchecked with Property[D, _])) =>
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
      case (maybeObj, (field, property: ObjectsProperty[D, _]@unchecked)) =>
        getSanitizer(property) match {
          case None =>
            value.get(field).collect {
              case ra: RawArray@unchecked =>
                val sanitized = ra.map {
                  case ro: RawObject@unchecked => sanitizeContract(property._contract, ro)
                  case _ => None
                }
                if (sanitized.exists(_.nonEmpty))
                  Some(maybeObj.getOrElse(value) + (field -> (sanitized.zip(ra).map(p => p._1.getOrElse(p._2)))))
                else
                  None
            }.flatten.orElse(maybeObj)

          case Some(sanitizer) =>
            applySanitizer(field, maybeObj.getOrElse(value), sanitizer)
              .orElse(maybeObj)
        }

      case (maybeObj, (field, property: MapObjectsProperty[D, _, _]@unchecked)) =>
        getSanitizer(property) match {
          case None =>
            value.get(field).collect {
              case rv: RawObject@unchecked =>
                val sanitized = rv.flatMap {
                  case (key, rv2: RawObject@unchecked) =>
                    sanitizeContract(property._contract, rv2).map(key -> _)
                  case _ =>
                    None
                }
                val removed = sanitized.collect{ case (key, m) if m.isEmpty => key}
                if (sanitized.nonEmpty)
                  Some(maybeObj.getOrElse(value) + (field -> ((rv ++ sanitized) -- removed)))
                else
                  None
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
