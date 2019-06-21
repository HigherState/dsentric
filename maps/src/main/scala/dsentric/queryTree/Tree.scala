package dsentric.queryTree

import dsentric.{DObject, Path, Query}

import scala.util.matching.Regex

sealed trait Tree {
  def isMatch(j:DObject):Boolean =
    Query.apply(j.value, this)

  def partition(paths:Set[Path]): (Option[Tree], Option[Tree]) =
    QueryTree.partition(this, paths)

  def queriedPaths:Set[Path]

}

sealed trait TreeComparison extends Tree {
  def path:Path

  def queriedPaths:Set[Path] =
    Set(path)
}

sealed trait TreeLogical extends Tree {
  def children:Seq[Tree]

  def queriedPaths:Set[Path] =
    children.flatMap(_.queriedPaths).toSet
}

final case class ?(path:Path, op:String, value:Any) extends TreeComparison
final case class Exists(path:Path, tree:Tree) extends TreeComparison
final case class In(subPath:Path, values:Map[String, Any]) extends Tree {
  import dsentric.Dsentric._
  def queriedPaths:Set[Path] =
    values.map(subPath \ _._1).toSet
}
final case class /(path:Path, regex:Regex) extends TreeComparison
final case class %(path:Path, like:String, regex:Regex) extends TreeComparison
final case class &(children:Seq[Tree]) extends TreeLogical
final case class |(children:Seq[Tree]) extends TreeLogical
final case class !!(tree:Tree) extends TreeLogical {
  def children:Seq[Tree] = List(tree)
}