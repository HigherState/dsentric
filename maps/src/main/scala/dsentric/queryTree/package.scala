package dsentric

package object queryTree {

  def ϵ(subPath:Path, values:Map[String, Any]): Tree =
    In(subPath, values)

  def ∃(path:Path, tree:Tree): TreeComparison =
    Exists(path, tree)
}
