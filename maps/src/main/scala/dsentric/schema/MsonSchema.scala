package dsentric.schema

import scala.collection.mutable

class MsonSchema(tab:String, newLine:String) {
  import dsentric.Dsentric._
  import dsentric.PessimisticCodecs._

  def convertObjectDefinition(objectDefinition:ObjectDefinition):String = {
    val sb = new mutable.StringBuilder()
    objectStructure(sb, objectDefinition)
    sb.toString()
  }

  def convertObjectDatastructures(objectDefinitions:(ObjectDefinition, Definition.Definitions)):String = {
    val sb = new mutable.StringBuilder()
    val combined = objectDefinitions._1 +: objectDefinitions._2
    combined.foreach{o =>
      sb.append(s"## ${escape(o.definition.getOrElse(throw SchemaGenerationException("Object definition required")))} (object)")
      o.description.foreach{d => sb.append(" - " + d)}
      o.properties.foreach { p =>
        sb.append(newLine)
        convertPropertyDefinition(sb, "", p)
      }
      sb.append(newLine)
    }
    sb.toString()
  }

  private def objectStructure(stringBuilder:StringBuilder, objectDefinition: ObjectDefinition):Unit = {
    stringBuilder.append(s"# ${escape(objectDefinition.definition.getOrElse(throw SchemaGenerationException("Object definition required")))} (object)")
    stringBuilder.append(newLine)
    objectDefinition.description.foreach{d => stringBuilder.append(d + newLine)}
    stringBuilder.append(newLine)
    stringBuilder.append("## Properties")
    objectDefinition.properties.foreach { p =>
      stringBuilder.append(newLine)
      convertPropertyDefinition(stringBuilder, "", p)
    }
  }


  private def convertPropertyDefinition(stringBuilder:StringBuilder, tabs:String, propertyDefinition: PropertyDefinition):Unit= {
    stringBuilder.append(tabs)
    stringBuilder.append("- " + escape(propertyDefinition.key))
    val (nullable, newTypeDefinition) = nullableCheck(propertyDefinition.typeDefinition)
    val tags = typeTags(!propertyDefinition.required, nullable)
    newTypeDefinition match {
      case ByRefDefinition(name) =>
        stringBuilder.append(s" ($name)")
        propertyDefinition.description.foreach(p => stringBuilder.append("- " + p))
      case o:ObjectDefinition =>
        //no examples allowed for objects
        stringBuilder.append(s" (object$tags)")
        propertyDefinition.description.foreach(p => stringBuilder.append(s" - $p"))
        o.referencedDefinitions.foreach{rd =>
          stringBuilder.append(newLine)
          stringBuilder.append(tabs)
          stringBuilder.append(tab)
          stringBuilder.append(s"- Include $rd")
        }
        o.properties.foreach{p =>
          stringBuilder.append(newLine)
          convertPropertyDefinition(stringBuilder, tabs + tab, p)
        }

      case m:MultipleTypeDefinition =>
        //TODO
        ???

      case a:ArrayDefinition =>
        propertyDefinition.examples.headOption.collect{ case s:Iterable[Any] => stringBuilder.append(s.map(i => escape(i.toString)).mkString(": ",", ",""))}
        propertyDefinition.description.foreach(p => stringBuilder.append(s" - $p"))
        propertyDefinition.default.collect{ case s:Iterable[Any] => s.map(i => escape(i.toString)).mkString(", ")}
          .foreach{p =>
            stringBuilder.append(newLine)
            stringBuilder.append(tabs)
            stringBuilder.append(tab)
            stringBuilder.append("- Default: " + p)
          }

      case t:TypeDefinition =>
        if (t.enum.nonEmpty) {
          stringBuilder.append(s": " + t.enum.map(i => escape(i.toString)).mkString(", "))
          stringBuilder.append(s" (enum[${t.name}]$tags)")
        }
        else {
          propertyDefinition.examples.headOption.foreach(p => s": " + escape(p.toString))
          stringBuilder.append(s" (${t.name}$tags)")
        }
        propertyDefinition.description.foreach(p => stringBuilder.append(s" - $p"))
        propertyDefinition.default.foreach{p =>
          stringBuilder.append(newLine)
          stringBuilder.append(tabs)
          stringBuilder.append(tab)
          stringBuilder.append("- Default: " + escape(p.toString))
        }
    }
  }

  private def typeTags(optional:Boolean, nullable:Boolean):String =
    if (optional && nullable) ", optional, nullable"
    else if (optional) ", optional"
    else if (nullable) ", nullable"
    else ""


  private def nullableCheck:Function[TypeDefinition, (Boolean, TypeDefinition)] = {
    case NullDefinition =>
      true -> NullDefinition

    case m:MultipleTypeDefinition if m.typeDefinitions.contains(NullDefinition) =>
      m.typeDefinitions.filter(_ != NullDefinition) match {
        case t +: _ =>
          true -> t
        case s =>
          true -> MultipleTypeDefinition(s)
      }

    case t =>
      false -> t
  }


  private val unsafeChars = Seq(':', '(',')', '<', '>', '{', '}', '[', ']', '_', '*', '-', '+', '`', '\'')
  private def escape(name:String):String =
    if (name.toSeq.intersect(unsafeChars).nonEmpty) "`" + name.replace("`","``") + "`"
    else name
}
