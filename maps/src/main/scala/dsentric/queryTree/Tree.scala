package dsentric.queryTree

import dsentric.{DObject, Path, Query}

import scala.util.matching.Regex

sealed trait Tree {
  def isMatch(j:DObject) =
    Query.apply(j.value, this)

  def partition(paths:Set[Path]) =
    QueryTree.partition(this, paths)
}
final case class ?(path:Path, op:String, value:Any) extends Tree
final case class ∃(path:Path, tree:Tree) extends Tree
final case class ϵ(subPath:Path, values:Map[String, Any]) extends Tree
final case class /(path:Path, regex:Regex) extends Tree
final case class %(path:Path, like:String, regex:Regex) extends Tree
final case class &(seq:Seq[Tree]) extends Tree
final case class |(seq:Seq[Tree]) extends Tree
final case class !!(tree:Tree) extends Tree
