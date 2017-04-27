package dsentric.queryTree

import dsentric._
import Dsentric._

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
        ϵ(path, r)
      else
        &(ϵ(path, r) +: treeNodes(l, path))
    else
      treeNodes(query, path) match {
        case Seq(s) => s
        case s =>
          &(s)
      }
  }


  private def treeNodes(query:Map[String, Any], path:Path):Seq[Tree] = {
    query.flatMap{
      case ("$and", values:Vector[Any]@unchecked) =>
        Some(&(values.collect { case m:Map[String,Any]@unchecked => buildTree(m, path)}.toList))
      case ("$or", values:Vector[Any]@unchecked) =>
        Some(|(values.collect { case m:Map[String,Any]@unchecked => buildTree(m, path)}.toList))
      case ("$not", value:Map[String,Any]@unchecked) =>
        Some(!!(buildTree(value, path)))
      case ("$elemMatch", value:Map[String,Any]@unchecked ) =>
        Some(∃(path, buildTree(value, Path.empty)))
      case ("$elemMatch", j) =>
        Some(∃(path, ?(Path.empty, "$eq", j)))
      case (o@("$eq" | "$ne" | "$lt" | "$gt" | "$lte" | "$gte" | "$in" | "$nin" | "$exists"), v) =>
        Some(?(path, o, v))
      case ("$regex", s:String) =>
        val options = query.get("$options").collect{ case o:String => s"(?$o)"}.getOrElse("")
        Some(/(path, (options + s).r))
      case ("$like", s:String) =>
        Some(%(path, s, ("(?i)" + s.replace("%", ".*")).r))
      case ("$options", _) =>
        None
      case (key, value:Map[String,Any]@unchecked) =>
        val p:Path = path \ key
        Some(buildTree(value, p))
      case (key, j) =>
        Some(?(path \ key, "$eq", j))
    }.toSeq
  }



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

      case ϵ(path, values) =>
        val (l,r) = values.partition{
          case (k,_) => paths.exists((path \ k).hasSubPath)
        }
        val lp = if (l.nonEmpty) Some(ϵ(path, l)) else None
        val rp = if (r.nonEmpty) Some(ϵ(path, r)) else None
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

      case ∃(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case !!(t) =>
        val (l, r) = negPartition(t, paths)
        l.map(!!) -> r.map(!!)
    }
  }

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
      case t@ϵ(path, values) =>
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

      case ∃(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case !!(t) =>
        val (l, r) = partition(t, paths)
        l.map(!!) -> r.map(!!)
    }
  }
}


