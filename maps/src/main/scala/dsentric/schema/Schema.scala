package dsentric.schema

import dsentric.{BaseContract, ContractFor, DObject}
import scala.reflect.runtime.universe._

case class Schema(typeName:String = "", fieldName:String = "", example:String = "") extends scala.annotation.StaticAnnotation

object Schema {


  def getContractName[D <: DObject](contract:BaseContract[D]):String = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    getSchema(t.annotations)
      .orElse(getSchema(t.baseClasses.flatMap(_.annotations)))
      .map(_.typeName)
      .getOrElse(getClassName(contract, t))
  }

  def getClassName[D <: DObject](contract:BaseContract[D], t:ClassSymbol):String = {
    if (contract.isInstanceOf[ContractFor[_]])
      t.name.encodedName.toString
    else {
      val baseclass = t.baseClasses.find(c => c != t && !c.fullName.startsWith("dsentric."))
      baseclass.map(_.name.encodedName.toString)
        .getOrElse("UnknownType")
    }
  }

  def getAnnotationOverrides[D <: DObject](contract:BaseContract[D]):Map[String, Schema] = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    val members = t.toType.members
    val fMembers = members.filter{m => m.annotations.exists(_.tree.tpe == typeOf[Schema])}
    members.flatMap(m => getSchema(m.annotations).map(m.name.toString.trim -> _)).toMap
  }

  def getSchema(annotations:List[Annotation]):Option[Schema] =
    annotations.find(_.tree.tpe == typeOf[Schema])
      .flatMap{ a =>
        a.tree.children.tail match {
          case Literal(Constant(typeName: String)) :: Literal(Constant(fieldName: String)) :: Literal(Constant(example: String)) :: _ =>
            Some(Schema(typeName, fieldName, example))
          case _ =>
            None
        }
      }
}
