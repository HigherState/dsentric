package dsentric.queryTree

import cats.data.NonEmptyList
import dsentric._

/*
Experimental feature for converting from mongo db style query to a PostGres jsonb query
Uses the jdbc ?? escape for ?
 */
case class QueryJsonb(escapeString:String => String)(implicit R:Renderer) {

  type JbValid = NonEmptyList[(String, Path)] Either String

  def apply(field:String, query:DQuery): JbValid =
    treeToPostgres(field)(QueryTree(query) -> false).map(_.mkString)

  def apply(field:String, query:Tree): JbValid =
    treeToPostgres(field)(query -> false).map(_.mkString)


  private def treeToPostgres(field:String):Function[(Tree, Boolean), NonEmptyList[(String, Path)] Either Vector[String]] = {
    case (&(Seq(value)), g) =>
      treeToPostgres(field)(value -> false).map(_ ++ (if (g) Some(")") else None))
    case (|(Seq(value)), g) =>
      treeToPostgres(field)(value -> false).map(_ ++ (if (g) Some(")") else None))
    case (&(head +: tail), g) =>
      builder(treeToPostgres(field)(head -> false), treeToPostgres(field)(&(tail) -> true)) { (h, t) =>
        ((if (!g) Some("(") else None) ++: h :+ " AND ") ++ t
      }
    case (&(_), g) =>
      Right(Vector("true") ++ (if (g) Some(")") else None))
    case (|(head +: tail), g) =>
      builder(treeToPostgres(field)(head -> false), treeToPostgres(field)(|(tail) -> true)) { (h, t) =>
        ((if (!g) Some("(") else None) ++: h :+ " OR ") ++ t
      }
    case (!!(tree), g) =>
      treeToPostgres(field)(tree -> false).map(v => "NOT (" +: v :+ ")")
    //TODO empty Path
    case (/(path, regex), _) =>
      Right("(" +: field +: " #>> '" +: toPath(path) +: "') ~ '" +: escape(regex.toString) +: Vector("'"))
    case (%(path, like, _), _) =>
      Right("(" +: field +: " #>> '" +: toPath(path) +: "') ILIKE '" +: like +: Vector("'"))
    case (ϵ(path, map), _) =>
      Right(field +: " @> '" +: toObject(path, map, R)  :+ "'::jsonb")
    case (?(path, "$eq", value), _) =>
      Right(field +: " @> '" +: toObject(path, value, R)  :+ "'::jsonb")
    case (?(path, "$ne", value), _) =>
      Right("NOT " +: field +: " @> '" +: toObject(path, value, R)  :+ "'::jsonb")
    case (?(path, "$in", value:Vector[Any]@unchecked), _) =>
      Right(Vector(field, " #> '", toPath(path), "' <@ '", escape(R.print(value)), "'::jsonb"))
    case (?(path, "$nin", value:Vector[Any]@unchecked), _) =>
      Right(Vector("NOT ", field, " #> '", toPath(path), "' <@ '", escape(R.print(value)), "'::jsonb"))
    case (?(path, o@("$nin" | "$in"), _), _) =>
      Left(NonEmptyList(s"Operation $o expected an array" -> path, Nil))
    case (?(path, "$exists", true), _) =>
      Right(field +: toSearch(path))
    case (?(path, "$exists", false), _) =>
      Right("NOT " +: field +: toSearch(path))
    case (?(path, "$exists", _), _) =>
      Left(NonEmptyList("Operation $exists requires a boolean" -> path, Nil))
    //TODO resolve duplicate jsonb_typeOfs which can occur in ands
    case (?(path, Op(op), value), _) =>
      val p = toPath(path)
      //TODO: use applicative builder..
      for {
        v <- serialize(value -> path)
        c <- getCast(value -> path)
        t <- getType(value -> path)
      } yield Vector(
        "(",
        s"jsonb_typeof($field #> '$p') = '$t'",
        " AND ",
        s"($field #>> '$p') :: $c $op $v",
        s")")

    case (∃(path, ?(Path.empty, "$eq", value)), _) =>
      Right(field +: toElement(path) +: " @> '" +: R.print(value) +: Vector("'"))
    case (∃(path, /(Path.empty, regex)), _) =>
      Right("EXISTS (SELECT * FROM jsonb_array_elements_text(" +: field +: toElement(path) +: ") many(elem) WHERE elem ~ '" +: escape(regex.toString) +: Vector("')"))
    case (∃(path, ?(subPath, "$eq", value)), _) =>
      Right(field +: toElement(path) +: " @> " +: "'["  +: toObject(subPath, value, R) :+ "]'")
    case (∃(path, _), _) =>
      Left(NonEmptyList("Currently only equality is supported in element match." -> path, Nil))
    case (?(path, op, _), _) =>
      Left(NonEmptyList(s"Unable to parse query operation $op." -> path, Nil))
  }

  private def toObject(path:Path, value:Any, R:Renderer):Vector[String] =
    path match {
      case PathKey(key, tail) =>
        "{\"" +: escape(key) +: "\":" +: toObject(tail, value, R) :+ "}"
      case PathIndex(index, tail) =>
        "{\"" +: index.toString +: "\":" +: toObject(tail, value, R) :+ "}"
      case _ =>
        Vector(escape(R.print(value)))
    }

  private def serialize:Function[(Any, Path), JbValid] = {
    case (s:String, _) => Right(escape(s))
    case (true, _) => Right("true")
    case (false, _) => Right("false")
    case (l:Long, _) => Right(l.toString)
    case (d:Double, _) => Right(d.toString)
    case (_:DNull, _) => Right("null")
    case (_, path) =>
      Left(NonEmptyList("Unsupported type" -> path, Nil))
  }

  private def escape(s:Either[Int, String]):String =
    escape(s.merge.toString)
  private def escape(s:String):String =
    escapeString(s)
  private def toPath(path:Path) =
    path.toList.map(escape).mkString("{", ",", "}")
  private def toSearch:Function[Path, Vector[String]] = {
    case PathKey(key, PathEnd) =>
      Vector(" ?? '", escape(key), "'")
    case PathKey(key, tail) =>
      " -> '" +: escape(key) +: "'" +: toSearch(tail)
    case PathIndex(index, PathEnd) =>
      Vector(" ?? '", index.toString, "'")
    case PathIndex(index, tail) =>
      " -> '" +: index.toString +: "'" +: toSearch(tail)
    case PathEnd =>
      Vector.empty
  }

  private def toElement(path:Path):String =
    path.toList.map(escape).map(s => s" -> '$s'").mkString("")

  private def getType:Function[(Any, Path), JbValid] = {
    case (_:Long,_) => Right("number")
    case (_:Double,_) => Right("number")
    case (_:String,_) => Right("string")
    case (_:Boolean,_) => Right("boolean")
    case (_:Map[String, Any]@unchecked,_) => Right("object")
    case (_:Vector[Any]@unchecked,_) => Right("array")
    case (_:DNull,_) => Right("null")
    case (_, path) =>
      Left(NonEmptyList("Unsupported type" -> path, Nil))
  }

  private def getCast:Function[(Any, Path), JbValid] = {
    case (_:Number,_) =>
      Right("NUMERIC")
    case (_:String,_) =>
      Right("TEXT")
    case (_:Boolean,_) =>
      Right("BOOLEAN")
    case (_, path) =>
      Left(NonEmptyList("Unsupported type" -> path, Nil))
  }

  object Op {
    def unapply(op: String): Option[String] =
      op match {
        case "$lt" => Some("<")
        case "$lte" => Some("<=")
        case "$gt" => Some(">")
        case "$gte" => Some(">=")
        case _ => None
      }
  }

  private def builder[T](left:NonEmptyList[(String, Path)] Either Vector[String],
                         right:NonEmptyList[(String, Path)] Either Vector[String])
                        (f:(Vector[String], Vector[String]) => T):NonEmptyList[(String, Path)] Either T = {
    (left, right) match {
      case (Right(l), Right(r)) =>
        Right(f(l,r))
      case (Left(NonEmptyList(h,t)), Left(NonEmptyList(h2, t2))) =>
        Left(NonEmptyList(h, t ++ (h2 :: t2)))
      case (_, Left(r)) =>
        Left(r)
      case (Left(l), _) =>
        Left(l)
    }
  }
}
