package dsentric.operators

import dsentric.contracts.{BaseContract, Property}
import dsentric.DObject

trait OperatorTree[M[_]] {
  def operators:Seq[M[_]]
  def leaves:Map[String, Seq[M[_]]]
  def nodes:Map[String, OperatorTree[M]]
}


object OperatorTree {

  def empty[M[_]]:OperatorTree[M] =
    EmptyOperatorTreeBuild.asInstanceOf[OperatorTree[M]]


  def buildTree[M[_], D <: DObject](contract:BaseContract[D])(f:PartialFunction[Property[D, _], Seq[M[_]]]):Option[OperatorTree[M]] = {
    val lifted = f.lift

    def build(parent:Option[OperatorTreeBuild[M]], baseContract: BaseContract[D]): Option[OperatorTreeBuild[M]] =
      parent.flatMap(recursive(baseContract, _)).map(Some.apply).getOrElse{
        val operators = baseContract match {
          case p:Property[D, _]@unchecked => lifted(p)
          case _ =>  None
        }
        val (leavesPre, nodesPre) = split(baseContract)
        val leaves = leavesPre.map{a => a._1 -> lifted(a._2)}.collect { case (key, Some(value)) => key -> value}
        if (nodesPre.isEmpty && leaves.isEmpty) {
          operators.map(ops => new OperatorTreeBuild[M](parent, ops, Map.empty, Map.empty))
        }
        else {
          val otb = new OperatorTreeBuild[M](parent, operators.getOrElse(Seq.empty), leaves, Map.empty)
          val nodes = nodesPre.foldLeft(Map.empty[String, OperatorTree[M]]){ (c, kv) => build(Some(otb), kv._2).foldLeft(c)((c2, ot) => c2 + (kv._1 -> ot))}
          if (nodes.isEmpty && leaves.isEmpty) {
            operators.map(ops => new OperatorTreeBuild[M](parent, ops, Map.empty, Map.empty))
          }
          else {
            otb.nodes = nodes
            Some(otb)
          }
        }
      }

    def split(contract: BaseContract[D]): (Map[String, Property[D, _]], Map[String, BaseContract[D]]) =
      contract._fields.span(kv => !kv._2.isInstanceOf[BaseContract[_]])
        .asInstanceOf[(Map[String, Property[D, _]], Map[String, BaseContract[D]])]

    def recursive(contact:BaseContract[D], parent:OperatorTreeBuild[M]):Option[OperatorTreeBuild[M]] =
      //TODO
      None

    build(None, contract)
  }

}

object EmptyOperatorTreeBuild extends OperatorTree[Nothing] {
  def operators: Seq[Nothing] = Seq.empty

  def leaves: Map[String, Seq[Nothing]] = Map.empty

  def nodes: Map[String, OperatorTree[Nothing]] = Map.empty
}

private class OperatorTreeBuild[M[_]](val parent:Option[OperatorTreeBuild[M]],
                                      val operators:Seq[M[_]],
                                      val leaves:Map[String, Seq[M[_]]],
                                      var nodes:Map[String, OperatorTree[M]]) extends OperatorTree[M]
