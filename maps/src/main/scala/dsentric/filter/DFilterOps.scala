package dsentric.filter

import dsentric._

trait DFilterOps {

  private[dsentric] def apply(value: Option[Any], query: Map[String, Any], notFoundAsNull: Boolean): Boolean = {
    query.forall {
      case ("$and", values: RawArray@unchecked) =>
        values.collect { case m: RawObject@unchecked => m }.forall(apply(value, _, notFoundAsNull))
      case ("$or", values: RawArray@unchecked) =>
        values.collect { case m: RawObject@unchecked => m }.exists(apply(value, _, notFoundAsNull))
      case ("$eq", v) =>
        value.contains(v)
      case ("$ne", v) =>
        !value.contains(v)
      //TODO regex not escaped
      case ("$regex" | "$options", _) =>
        query.get("$regex").collect {
          case v: String =>
            val options = query.get("$options").collect { case o: String => s"(?$o)" }.getOrElse("")
            value.collect { case s: String => (options + v).r.pattern.matcher(s).matches }.getOrElse(false)
        }.getOrElse(false)
      case ("$like", v: String) =>
        value.collect {
          case s: String =>
            ("(?i)" + v.replace("%", ".*")).r.pattern.matcher(string2RegexEscapedString(s)).matches
        }.getOrElse(false)
      case ("$lt", v) =>
        value.exists(x => order(x -> v).contains(-1))
      case ("$gt", v) =>
        value.exists(x => order(x -> v).contains(1))
      case ("$lte", v) =>
        value.exists(x => order(x -> v).exists(r => r <= 0))
      case ("$gte", v) =>
        value.exists(x => order(x -> v).exists(r => r >= 0))
      case ("$in", values: RawArray@unchecked) =>
        value.exists(j => values.contains(j))
      case ("$nin", values: RawArray@unchecked) =>
        !value.exists(j => values.contains(j)) //nin doesnt require existence, as per mongodb
      case ("$exists", v: Boolean) =>
        value.isDefined == v
      case ("$not", v: Map[String, Any]@unchecked) =>
        !apply(value, v, notFoundAsNull)
      case ("$elemMatch", v: Map[String, Any]@unchecked) =>
        value.collect { case seq: RawArray@unchecked => seq.exists(s => apply(Some(s), v, notFoundAsNull)) }.getOrElse(false)
      case ("$elemMatch", v) =>
        value.collect { case seq: RawArray@unchecked => seq.contains(v) }.getOrElse(false)
      case (key, v: RawObject@unchecked) =>
        val maybeValue = value.collect { case m: RawObject@unchecked => m }.flatMap(_.get(key))
        if (notFoundAsNull)
          apply(maybeValue.orElse(Some(DNull)), v, notFoundAsNull)
        else
          apply(maybeValue, v, notFoundAsNull)
      case (key, v) if notFoundAsNull =>
        value.collect {
          case m: RawObject@unchecked =>
            m.get(key).orElse(Some(DNull)).contains(v)
          case DNull =>
            v == DNull
        }.getOrElse(false)
      case (key, v) =>
        value.collect { case m: RawObject@unchecked => m }.fold(false) { l =>

          l.get(key).contains(v)
        }
    }
  }


  private val order = DValueOps.order.lift

  private val regexChars = Set('\\', '^', '$', '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|')

  def string2RegexEscapedString(s:String):String =
    if (s.exists(regexChars.contains)) {
      val sb = new StringBuilder
      s.foreach { c =>
        if (regexChars.contains(c)) sb.append("\\")
        sb.append(c)
      }
      sb.result()
    } else
      s
}

object DFilterOps extends DFilterOps
