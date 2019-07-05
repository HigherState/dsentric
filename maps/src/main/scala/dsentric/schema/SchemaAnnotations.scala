package dsentric.schema

import dsentric.{BaseContract, ContractFor, DObject}

import scala.reflect.runtime.universe._

case object IgnoreExample
case class Type(typeName:String) extends scala.annotation.StaticAnnotation
case class Title(title:String)extends scala.annotation.StaticAnnotation
case class Nested() extends scala.annotation.StaticAnnotation
case class Examples(example:Any, example2:Any = IgnoreExample, example3:Any = IgnoreExample, example4:Any = IgnoreExample, example5:Any = IgnoreExample) extends scala.annotation.StaticAnnotation
case class Description(text:String) extends scala.annotation.StaticAnnotation

case class SchemaAnnotations(typeName:Option[String], title:Option[String], nested:Boolean, examples:List[Any], description:Option[String])


object SchemaAnnotations {
  val empty: SchemaAnnotations = SchemaAnnotations(None, None, false, Nil, None)
  private val TypeTpe = typeOf[Type]
  private val TitleTpe = typeOf[Title]
  private val NestedTpe = typeOf[Nested]
  private val ExampleTpe = typeOf[Examples]
  private val DescriptionTpe = typeOf[Description]

  def getContractSchemaAnnotations[D <: DObject](contract:BaseContract[D]):SchemaAnnotations = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    val schemas = getSchemaAnnotation(t.annotations) :: t.baseClasses.map(c => getSchemaAnnotation(c.annotations))
    val schema = foldSchemaAnnotation(schemas)
    if (schema.typeName.isEmpty) {
      schema.copy(typeName = getClassName(contract, t))
    }
    else
      schema
  }

  private val ignore = Set("Object", "Any")
  def getClassName[D <: DObject](contract:BaseContract[D], t:ClassSymbol):Option[String] = {
    if (contract.isInstanceOf[ContractFor[_]])
      Some(t.name.encodedName.toString)
    else {
      val baseclasses = getBaseClasses(t)
      if (baseclasses.length == 1)
        baseclasses.headOption.map(_.name.encodedName.toString)
      else
        None
    }
  }

  def getBaseClasses(t:ClassSymbol):List[Symbol] = {
    t.baseClasses.filter(c => c != t && !c.fullName.startsWith("dsentric.") && !ignore(c.name.encodedName.toString))
  }
  private val baseContract:Symbol = typeOf[BaseContract[_]].typeSymbol

  def getFieldsSchemaAnnotations[D <: DObject](contract:BaseContract[D]):Map[String, SchemaAnnotations] = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    val members = t.toType.members.filter(m => contract._keys(m.name.toString.trim))
      .foldLeft(Map.empty[String, Symbol])((m, e) => if (m.contains(e.name.toString.trim)) m else m + (e.name.toString.trim -> e)) //duplicate names, drop second one
    members.map { p =>
      p._1 ->
      contract._fields.find(f => f._1 == p._1 && f._2.isInstanceOf[BaseContract[_]])
        .foldLeft(getSchemaAnnotation(p._2.annotations))( (s, p) => foldSchemaAnnotation(s :: getContractSchemaAnnotations(p._2.asInstanceOf[BaseContract[DObject]]) :: Nil))
    }
  }

  private def foldSchemaAnnotation(schemas:List[SchemaAnnotations]):SchemaAnnotations =
    schemas.foldRight(empty){(e, s) =>
      val typeName = e.typeName.orElse(s.typeName)
      val title = e.title.orElse(s.title)
      val nested = e.nested || s.nested
      val example = if (e.examples.nonEmpty) e.examples else s.examples
      val description = e.description.orElse(s.description)
      SchemaAnnotations(typeName, title, nested, example, description)
    }

  private def getSchemaAnnotation(annotations:List[Annotation]):SchemaAnnotations =
    annotations
      .map(t => t.tree.tpe -> t.tree.children.tail)
      .foldLeft(empty){
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
          println(t)
          s
      }
}
