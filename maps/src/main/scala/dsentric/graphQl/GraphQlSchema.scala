package dsentric.graphQl

import reflect.runtime.universe._
import dsentric._

case class GraphQlField(name:String, typeName:String, notNullable:Boolean)

case class GraphQlType(name:String, fields:Vector[GraphQlField])

case class GraphQl(typeName:String = "", fieldName:String = "") extends scala.annotation.StaticAnnotation

object GraphQlSchema {

  def renderContract[D <: DObject](contract:BaseContract[D], newLine:String, indent:String):String =
    render(readContract(contract, None), newLine, indent)

  def readContract[D <: DObject](contract:BaseContract[D], typeOverride:Option[String]):Vector[GraphQlType] = {
    val annotatedChildren = getAnnotationOverrides(contract)
    val (fieldList, typeList) =
      contract._fields.foldLeft(Vector.empty[GraphQlField] -> Vector.empty[GraphQlType]){
        case ((fields, types),(name, b:BaseContract[D]@unchecked)) =>
          val typeName = annotatedChildren.getOrElse(name, getContractName(b))
          val newFields = fields :+ GraphQlField(name, typeName, b.isInstanceOf[Expected[_, _]])
          val newTypes = types ++ readContract(b, Some(typeName))
          newFields -> newTypes

        case ((fields, types), (name, p)) =>
          val field = GraphQlField(name, p._codec.schemaName, p.isInstanceOf[Expected[_, _]])
          (fields :+ field) -> types
      }
    (GraphQlType(typeOverride.getOrElse(getContractName(contract)), fieldList) +: typeList).distinct
  }

  def render(graphQlTypes: Vector[GraphQlType], newLine:String, indent:String):String = {
    graphQlTypes.map{
      case GraphQlType(name, fields) =>
        "type " + name + " {" +
          fields
            .map{
              case GraphQlField(fieldName, typeName, true) =>
                fieldName + ": " + typeName + "!"
              case GraphQlField(fieldName, typeName, false) =>
                fieldName + ": " + typeName
            }
            .mkString(newLine + indent, newLine + indent, newLine + "}")
    }.mkString(newLine + newLine)
  }

  def getContractName[D <: DObject](contract:BaseContract[D]):String = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    getAnnotatedName(t.annotations)
      .orElse(getAnnotatedName(t.baseClasses.flatMap(_.annotations)))
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

  def getAnnotationOverrides[D <: DObject](contract:BaseContract[D]):Map[String, String] = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    val members = t.toType.members
    val fMembers = members.filter{m => m.annotations.exists(_.tree.tpe == typeOf[GraphQl])}
    members.flatMap(m => getAnnotatedName(m.annotations).map(m.name.toString.trim -> _)).toMap
  }

  def getAnnotatedName(annotations:List[Annotation]):Option[String] =
    annotations.find(_.tree.tpe == typeOf[GraphQl])
      .flatMap(_.tree.children.tail.collectFirst { case Literal(Constant(c: String)) if c != "" => c })
}
