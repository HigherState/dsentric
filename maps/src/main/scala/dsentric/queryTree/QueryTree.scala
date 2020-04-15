package dsentric.queryTree

import dsentric._
import Dsentric._

import scala.util.matching.Regex

object QueryTree {

  def apply(query:DQuery):Tree = {
    buildTree(query.value, Path.empty)
  }

  private def buildTree(query:Map[String, Any], path:Path):Tree = {
    val (l, r) = query.partition{
      case (k, _) if k.startsWith("$") =>
        true
      case (_, _:Map[String, Any]@unchecked) =>
        true
      case _ =>
        false
    }

    if (r.size > 1)
      if (l.isEmpty)
        In(path, r)
      else
        &(In(path, r) +: treeNodes(l, path))
    else
      treeNodes(query, path) match {
        case Seq(s) => s
        case s =>
          &(s)
      }
  }


  private def treeNodes(query:Map[String, Any], path:Path):Seq[Tree] = {
    val fieldRegex = """^\$\/(.*)\/$""".r
    query.flatMap{
      case ("$and", values:Vector[Any]@unchecked) =>
        Some(&(values.collect { case m:Map[String,Any]@unchecked => buildTree(m, path)}.toList))
      case ("$or", values:Vector[Any]@unchecked) =>
        Some(|(values.collect { case m:Map[String,Any]@unchecked => buildTree(m, path)}.toList))
      case ("$not", value:Map[String,Any]@unchecked) =>
        Some(!!(buildTree(value, path)))
      case ("$elemMatch", value:Map[String,Any]@unchecked ) =>
        Some(Exists(path, buildTree(value, Path.empty)))
      case ("$elemMatch", j) =>
        Some(Exists(path, ?(Path.empty, "$eq", j)))
      case (o@("$eq" | "$ne" | "$lt" | "$gt" | "$lte" | "$gte" | "$in" | "$nin" | "$exists"), v) =>
        Some(?(path, o, v))
      case ("$regex", s:String) =>
        val options = query.get("$options").collect{ case o:String => s"(?$o)"}.getOrElse("")
        Some(/(path, (options + s).r))
      case ("$like", s:String) =>
        Some(%(path, s, ("(?i)" + s.replace("%", ".*")).r))
      case ("$options", _) =>
        None
      case (fieldRegex(regex), value: Map[String, Any]@unchecked) =>
        Some($(new Regex(regex), buildTree(value, path)))
      case (key, value:Map[String,Any]@unchecked) =>
        val p:Path = path \ key
        Some(buildTree(value, p))
      case (key, j) =>
        Some(?(path \ key, "$eq", j))
    }.toSeq
  }


  /**
   * Partitions the tree based on whether the query can be executed on the database using the indexes.
   * The left-hand side contains the tree that can be executed on the database indexes and the right-hand side
   * contains the tree that will be executed after the first results are in.
   *
   * @note There can be paths that will be included in both parts. That is used for the application of partial queries
   *       on existing indexes.
   *
   * @param tree The original tree
   * @param paths The paths (ie indexes) by which to partition
   * @return A tuple of optional trees, the left being the queryTree to be executed against the queries and the right
   *         being the queryTree to be executed subsequently on the results.
   */
  def partition(tree:Tree, paths:Set[Path]):(Option[Tree], Option[Tree]) = {
    tree match {
      case |(trees) =>
        val (l,r) = trees.map(partition(_, paths)).unzip
        if (l.count(_.nonEmpty) < trees.length) //not all elements present in query
          None -> Some(tree)
        else {
          val lm = Some(|(l.flatten))
          if (r.forall(_.isEmpty))
            lm -> None
          else
            lm -> Some(tree)
        }

      case &(trees) =>
        val (l,r) = trees.map(partition(_, paths)).unzip
        val lm = l.flatten
        val rm = r.flatten
        val lv = lm match {
          case Seq() => None
          case Seq(t) => Some(t)
          case s => Some(&(s))
        }
        val rv = rm match {
          case Seq() => None
          case Seq(t) => Some(t)
          case s => Some(&(s))
        }
        lv -> rv

      //Cannot be partitioned
      case $(_, tree) =>
        None -> Option(tree)

      case In(path, values) =>
        val (l,r) = values.partition{
          case (k,_) => paths.exists((path \ k).hasSubPath)
        }
        val lp = if (l.nonEmpty) Some(In(path, l)) else None
        val rp = if (r.nonEmpty) Some(In(path, r)) else None
        lp -> rp

      case ?(path, _, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case /(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case %(path, _, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case Exists(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case !!(t) =>
        val (l, r) = negPartition(t, paths)
        l.map(!!.apply) -> r.map(!!.apply)
    }
  }

  /**
   * Applies De Morgan's laws to the partition in case there is a negation operator.
   *
   * @see [[partition]]
   */
  private def negPartition(tree:Tree, paths:Set[Path]):(Option[Tree], Option[Tree]) = {
    tree match {
      case |(trees) =>
        val (l,r) = trees.map(negPartition(_, paths)).unzip
        val lm = l.flatten
        val rm = r.flatten
        val lv =
          if (lm.nonEmpty) Some(|(lm))
          else None
        val rv =
          if (rm.nonEmpty) Some(|(rm))
          else None
        lv -> rv

      case &(trees) =>
        val (l,r) = trees.map(negPartition(_, paths)).unzip
        if (l.count(_.nonEmpty) < trees.length) //not all elements present in query
          None -> Some(tree)
        else {
          val lm = Some(&(l.flatten))
          if (r.forall(_.isEmpty))
            lm -> None
          else
            lm -> Some(tree)
        }

      //Cannot be partitioned
      case $(_, tree) =>
        None -> Option(tree)

      case t@In(path, values) =>
        if (values.forall(kv => paths.exists((path \ kv._1).hasSubPath)))
          Some(t) -> None
        else
          None -> Some(t)

      case ?(path, _, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case /(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case %(path, _, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case Exists(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case !!(t) =>
        val (l, r) = partition(t, paths)
        l.map(!!.apply) -> r.map(!!.apply)
    }
  }
}