package dsentric.schema

import dsentric._

case class GraphQlField(name:String, typeName:String, notNullable:Boolean)

case class GraphQlType(name:String, fields:Vector[GraphQlField])


object GraphQlSchema {

  def renderContract[D <: DObject](contract:BaseContract[D], newLine:String, indent:String):String =
    render(readContract(contract, None), newLine, indent)

  def readContract[D <: DObject](contract:BaseContract[D], typeOverride:Option[String]):Vector[GraphQlType] = {
    val annotatedChildren = Schema.getAnnotationOverrides(contract)
    val (fieldList, typeList) =
      contract._fields.foldLeft(Vector.empty[GraphQlField] -> Vector.empty[GraphQlType]){
        case ((fields, types),(name, b:BaseContract[D]@unchecked)) =>
          val typeName = annotatedChildren.get(name).fold(Schema.getContractName(b))(_.typeName)
          val newFields = fields :+ GraphQlField(name, typeName, b.isInstanceOf[Expected[_, _]])
          val newTypes = types ++ readContract(b, Some(typeName))
          newFields -> newTypes

        case ((fields, types), (name, p)) =>
          val field = GraphQlField(name, p._codec.schemaName, p.isInstanceOf[Expected[_, _]])
          (fields :+ field) -> types
      }
    (GraphQlType(typeOverride.getOrElse(Schema.getContractName(contract)), fieldList) +: typeList).distinct
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

}
