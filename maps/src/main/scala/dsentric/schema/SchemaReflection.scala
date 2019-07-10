package dsentric.schema

import dsentric.{BaseContract, Property}

import scala.reflect.runtime.universe._

case object IgnoreExample
case class Type(typeName:String) extends scala.annotation.StaticAnnotation
case class Title(title:String)extends scala.annotation.StaticAnnotation
case class Nested() extends scala.annotation.StaticAnnotation
case class Examples(example:Any, example2:Any = IgnoreExample, example3:Any = IgnoreExample, example4:Any = IgnoreExample, example5:Any = IgnoreExample) extends scala.annotation.StaticAnnotation
case class Description(text:String) extends scala.annotation.StaticAnnotation

case class ContractInfo(fullName:String,
                        displayName:Option[String],
                        schemaAnnotations: SchemaAnnotations,
                        inherits:Vector[ContractInfo],
                        fields:Map[String, SchemaAnnotations]) {

  def isSubClass(contractInfo:ContractInfo):Boolean =
    contractInfo == this || inherits.exists(_.isSubClass(contractInfo))

  def getInheritedFieldAnnotation(name:String):Option[SchemaAnnotations] =
    fields.get(name).orElse(inherits.toStream.flatMap(_.getInheritedFieldAnnotation(name)).headOption)
}

case class SchemaAnnotations(typeName:Option[String], title:Option[String], nested:Boolean, examples:List[Any], description:Option[String])

object SchemaAnnotations {
  val empty: SchemaAnnotations = SchemaAnnotations(None, None, false, Nil, None)
}
object SchemaReflection {

  private val TypeTpe = typeOf[Type]
  private val TitleTpe = typeOf[Title]
  private val NestedTpe = typeOf[Nested]
  private val ExampleTpe = typeOf[Examples]
  private val DescriptionTpe = typeOf[Description]


  def getDisplayName(contract:BaseContract[_]):String = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    val schema = getSchemaAnnotation(t.annotations)
    schema.typeName.getOrElse(t.name.toString)
  }

  def getContractInfo(contract:BaseContract[_], current:Vector[ContractInfo] = Vector.empty):(ContractInfo, Vector[ContractInfo]) = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    getContractInfo(t, current)
  }

  def getContractInfo(t:ClassSymbol, current:Vector[ContractInfo]):(ContractInfo, Vector[ContractInfo]) = {
    val fullName = t.fullName
    current.find(_.fullName == fullName) match {
      case Some(ci) =>
        ci -> current
      case None =>
        //val schemas = getSchemaAnnotation(t.annotations) :: t.baseClasses.map(c => getSchemaAnnotation(c.annotations))
        val schema = getSchemaAnnotation(t.annotations)
        val bc = getBaseClasses(t)
        val (inherited, newCurrent) =
          bc.foldLeft(Vector.empty[ContractInfo] -> current) {
            case ((b, v), e) =>
              val (ici, nv) = getContractInfo(e.asClass, v)
              (b :+ ici) -> nv
          }
        val fields = getFieldsSchemaAnnotations(t)
        val displayName =
          schema.typeName.orElse{
            if (t.name.toString.startsWith("$anon$"))
              if (inherited.size == 1 && fields.isEmpty)
                inherited.head.displayName
              else
                None
            else
              Some(t.name.toString)
          }
        val foldedInherited = inherited.filterNot(i => inherited.exists(i2 => i2 != i && i2.isSubClass(i)))

        val contractInfo =
          ContractInfo(fullName, displayName, schema, foldedInherited, fields)

        contractInfo -> (newCurrent :+ contractInfo)
    }
  }

  private val baseContractType =
    typeOf[BaseContract[_]].typeSymbol

  def getBaseClasses(t:ClassSymbol):List[Symbol] =
    t.baseClasses.filter { c =>
      c != t &&
      !c.fullName.startsWith("dsentric.") &&
      c.asClass.baseClasses.contains(baseContractType)
    }

  private val propertyType =
    typeOf[Property[_, _]].typeSymbol

  def getFieldsSchemaAnnotations(t:ClassSymbol):Map[String, SchemaAnnotations] = {
    val members0 =
      t.toType.members
    val members = members0.filter(m => m.typeSignature.baseClasses.contains(propertyType) && !m.isClass && m.owner == t  && (t.isTrait || !m.isMethod) && m.overrides.isEmpty)
    members.map { p =>
      val annotations = getSchemaAnnotation(p.annotations)
      p.name.toString.trim() -> annotations
    }.toMap
  }

//  private def foldSchemaAnnotation(schemas:List[SchemaAnnotations]):SchemaAnnotations =
//    schemas.foldRight(SchemaAnnotations.empty){(e, s) =>
//      val typeName = e.typeName.orElse(s.typeName)
//      val title = e.title.orElse(s.title)
//      val nested = e.nested || s.nested
//      val example = if (e.examples.nonEmpty) e.examples else s.examples
//      val description = e.description.orElse(s.description)
//      SchemaAnnotations(typeName, title, nested, example, description)
//    }

  private def getSchemaAnnotation(annotations:List[Annotation]):SchemaAnnotations =
    annotations
      .map(t => t.tree.tpe -> t.tree.children.tail)
      .foldLeft(SchemaAnnotations.empty){
        case (s, (TypeTpe, tail)) =>
          s.copy(typeName = tail.collectFirst{ case Literal(Constant(c: String)) => c})
        case (s, (TitleTpe, tail)) =>
          s.copy(title = tail.collectFirst{ case Literal(Constant(c: String)) => c})
        case (s, (NestedTpe, _)) =>
          s.copy(nested = true)
        case (s, (ExampleTpe, tail)) =>
          s.copy(examples = tail.collect{ case Literal(Constant(c)) => c})
        case (s, (DescriptionTpe, tail)) =>
          s.copy(description = tail.collectFirst{ case Literal(Constant(c: String)) => c})
        case (s, (t, tail)) =>
          s
      }
}
