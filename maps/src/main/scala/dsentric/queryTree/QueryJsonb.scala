package dsentric.queryTree

import cats.data.{OneAnd, Xor, NonEmptyList}
import dsentric.{Renderer, DNull, DQuery, Path}

/*
Experimental feature for converting from mongo db style query to a PostGres jsonb query
Uses the jdbc ?? escape for ?
 */
object QueryJsonb {

  type JbValid = NonEmptyList[(String, Path)] Xor String

  def apply(field:String, query:DQuery)(implicit R:Renderer): JbValid =
    treeToPostgres(field, R)(QueryTree(query) -> false).map(_.mkString)

  def apply(field:String, query:Tree)(implicit R:Renderer): JbValid =
    treeToPostgres(field, R)(query -> false).map(_.mkString)


  private def treeToPostgres(field:String, R:Renderer):Function[(Tree, Boolean), NonEmptyList[(String, Path)] Xor Vector[String]] = {
    case (&(Seq(value)), g) =>
      treeToPostgres(field, R)(value -> false).map(_ ++ (if (g) Some(")") else None))
    case (|(Seq(value)), g) =>
      treeToPostgres(field, R)(value -> false).map(_ ++ (if (g) Some(")") else None))
    case (&(head +: tail), g) =>
      builder(treeToPostgres(field, R)(head -> false), treeToPostgres(field, R)(&(tail) -> true)) { (h, t) =>
        ((if (!g) Some("(") else None) ++: h :+ " AND ") ++ t
      }
    case (|(head +: tail), g) =>
      builder(treeToPostgres(field, R)(head -> false), treeToPostgres(field, R)(&(tail) -> true)) { (h, t) =>
        ((if (!g) Some("(") else None) ++: h :+ " OR ") ++ t
      }
    case (!!(tree), g) =>
      treeToPostgres(field, R)(tree -> false).map(v => "NOT (" +: v :+ ")")
    //TODO empty Path
    case (/(path, regex), _) =>
      Xor.Right("(" +: field +: " #>> '" +: toPath(path) +: "') ~ '" +: escape(regex.toString) +: Vector("'"))
    case (%(path, like, _), _) =>
      Xor.Right("(" +: field +: " #>> '" +: toPath(path) +: "') ILIKE '" +: like +: Vector("'"))
    case (?(path, "$eq", value), _) =>
      Xor.Right(field +: " @> '" +: toObject(path, value, R) :+ "'::jsonb")
    case (?(path, "$ne", value), _) =>
      Xor.Right("NOT " +: field +: " @> '" +: toObject(path, value, R) :+ "'::jsonb")
    case (?(path, "$in", value:Vector[Any]@unchecked), _) =>
      Xor.Right(Vector(field, " #> '", toPath(path), "' <@ '", escape(R.print(value)), "'::jsonb"))
    case (?(path, "$nin", value:Vector[Any]@unchecked), _) =>
      Xor.Right(Vector("NOT ", field, " #> '", toPath(path), "' <@ '", escape(R.print(value)), "'::jsonb"))
    case (?(path, o@("$nin" | "$in"), _), _) =>
      Xor.Left(NonEmptyList(s"Operation $o expected an array" -> path))
    case (?(path, "$exists", true), _) =>
      Xor.Right(field +: toSearch(path))
    case (?(path, "$exists", false), _) =>
      Xor.Right("NOT " +: field +: toSearch(path))
    case (?(path, "$exists", _), _) =>
      Xor.Left(NonEmptyList("Operation $exists requires a boolean" -> path))
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
      Xor.Right(field +: toElement(path) +: Vector(s" ? '${escape(R.print(value))}'"))
    case (∃(path, &(Seq(/(Path.empty, regex)))), _) =>
      Xor.Right("EXISTS (SELECT * FROM jsonb_array_elements_text(" +: field +: toElement(path) +: ") many(elem) WHERE elem ~ '" +: escape(regex.toString) +: Vector("')"))
    case (∃(path, &(Seq(?(subPath, "$eq", value)))), _) =>
      Xor.Right(field +: toElement(path) +: " @> " +: "'["  +: toObject(subPath, value, R) :+ "]'")
    case (∃(path, _), _) =>
      Xor.Left(NonEmptyList("Currently only equality is supported in element match." -> path))
    case (?(path, op, _), _) =>
      Xor.Left(NonEmptyList(s"Unable to parse query operation $op." -> path))
  }

  private def toObject(path:Path, value:Any, R:Renderer):Vector[String] =
    path match {
      case head +: tail =>
        "{\"" +: escape(head) +: "\":" +: toObject(tail, value, R) :+ "}"
      case _ => Vector(escape(R.print(value)))
    }

  private def serialize:Function[(Any, Path), JbValid] = {
    case (s:String, _) => Xor.Right(escape(s))
    case (true, _) => Xor.Right("true")
    case (false, _) => Xor.Right("false")
    case (l:Long, _) => Xor.Right(l.toString)
    case (d:Double, _) => Xor.Right(d.toString)
    case (_:DNull, _) => Xor.Right("null")
    case (_, path) =>
      Xor.Left(NonEmptyList("Unsupported type" -> path))
  }

  private def escape(s:Either[Int, String]):String =
    escape(s.merge.toString)
  private def escape(s:String):String =
    s.replace("'","''")
  private def toPath(path:Path) =
    path.map(escape).mkString("{", ",", "}")
  private def toSearch:Function[Path, Vector[String]] = {
    case tail :: Nil =>
      Vector(" ?? '", escape(tail), "'")
    case head :: tail =>
      " -> '" +: escape(head) +: "'" +: toSearch(tail)
    case Nil =>
      Vector.empty
  }

  private def toElement(path:Path):String =
    path.map(escape).map(s => s" -> '$s'").mkString("")

  private def getType:Function[(Any, Path), JbValid] = {
    case (_:Long,_) => Xor.Right("number")
    case (_:Double,_) => Xor.Right("number")
    case (_:String,_) => Xor.Right("string")
    case (_:Boolean,_) => Xor.Right("boolean")
    case (_:Map[String, Any]@unchecked,_) => Xor.Right("object")
    case (_:Vector[Any]@unchecked,_) => Xor.Right("array")
    case (_:DNull,_) => Xor.Right("null")
    case (_, path) =>
      Xor.Left(NonEmptyList("Unsupported type" -> path))
  }

  private def getCast:Function[(Any, Path), JbValid] = {
    case (_:Number,_) =>
      Xor.Right("NUMERIC")
    case (_:String,_) =>
      Xor.Right("TEXT")
    case (_:Boolean,_) =>
      Xor.Right("BOOLEAN")
    case (_, path) =>
      Xor.Left(NonEmptyList("Unsupported type" -> path))
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

  private def builder[T](
                          left:NonEmptyList[(String, Path)] Xor Vector[String],
                          right:NonEmptyList[(String, Path)] Xor Vector[String])
                        (f:(Vector[String], Vector[String]) => T):NonEmptyList[(String, Path)] Xor T = {
    (left, right) match {
      case (Xor.Right(l), Xor.Right(r)) =>
        Xor.Right(f(l,r))
      case (Xor.Left(OneAnd(h,t)), Xor.Left(OneAnd(h2, t2))) =>
        Xor.Left(OneAnd(h, t ++ (h2 :: t2)))
      case (_, Xor.Left(r)) =>
        Xor.Left(r)
      case (Xor.Left(l), _) =>
        Xor.Left(l)
    }
  }
}


