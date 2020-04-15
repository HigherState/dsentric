package dsentric.queryTree

import dsentric.{DObject, Path, Query}

import scala.util.matching.Regex

sealed trait Tree extends Product with Serializable {

  def isMatch(j:DObject, valueNotFoundAsNull:Boolean = false):Boolean =
    Query.apply(j.value, this, valueNotFoundAsNull)


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

//More like contains object
final case class In(subPath:Path, values:Map[String, Any]) extends Tree {
  import dsentric.Dsentric._
  def queriedPaths:Set[Path] =
    values.map(subPath \ _._1).toSet
}
final case class /(path:Path, regex:Regex) extends TreeComparison
final case class %(path:Path, like:String, regex:Regex) extends TreeComparison
final case class &(children:Seq[Tree]) extends TreeLogical {
  override def equals(obj: Any): Boolean =
    obj match {
      case op: & =>
        &.flatten(children).toSet == &.flatten(op.children).toSet
      case !!(op: |) =>
        &.flatten(children).toSet == |.flatten(op.children).toSet.map(!!(_))
      case _ =>
        false
    }

  override def toString: String =
    s"&(${children.mkString(",")})"
}

final case class |(children:Seq[Tree]) extends TreeLogical {
  override def equals(obj: Any): Boolean =
    obj match {
      case op: | =>
        |.flatten(children).toSet == |.flatten(op.children).toSet
      case !!(op: &) =>
        |.flatten(children).toSet == &.flatten(op.children).toSet.map(!!(_))
      case _ =>
        false
    }

  override def toString: String =
    s"|(${children.mkString(",")})"
}

final class !!(val tree:Tree) extends TreeLogical {
  def children:Seq[Tree] = List(tree)

  override def equals(obj: Any): Boolean =
    (obj, tree) match {
      case (compareNot: !!, t) =>
        compareNot.tree == t
      case (compareTree, not: !!) =>
        compareTree == not.tree
      case _ =>
        false
    }

  override def toString: String =
    s"!!($tree)"

  def productElement(n: Int): Any = tree

  def productArity: Int = 1

  def canEqual(that: Any): Boolean =
    this.isInstanceOf[Tree]
}

object & {
  def apply(tree1:Tree, tree2:Tree, trees:Tree*): & =
    &(flatten(tree1 +: tree2 +: trees).distinct)

  def flatten(trees:Seq[Tree]):Seq[Tree] = {
    trees.flatMap{
      case op: &     => flatten(op.children)
      case !!(op: |) => |.flatten(op.children).map(!!.apply)
      case op        => Some(op)
    }
  }
}

/**
 * Further reductions to add
 * |(A, &(A, B))       => A
 * &(A, |(A, B))       => A
 * &(A, |(!!A, B))     => &(A, B)
 * |(&(A, B), &(A, C)) => &(A, |(B, C))
 * &(|(A, B), |(A, C)) => \(A, &(B, C))
 */
object | {
  def apply(tree1:Tree, tree2:Tree, trees:Tree*): | =
    |(flatten(tree1 +: tree2 +: trees).distinct)

  def flatten(trees:Seq[Tree]):Seq[Tree] =
    trees.flatMap{
      case op: |     => flatten(op.children)
      case !!(op: &) => &.flatten(op.children).map(!!.apply)
      case op        => Some(op)
    }
}

object !! {
  def apply(tree:Tree):Tree =
    tree match {
      case not: !! => not.tree
      case t => new !!(t)
    }

  def unapply(not: !!):Option[Tree] =
    Some(not.tree)
}

