package dsentric.schema

import dsentric.contracts.{BaseContract, Property}

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

case object IgnoreExample
case class Type(typeName: String)    extends scala.annotation.StaticAnnotation
case class Title(title: String)      extends scala.annotation.StaticAnnotation
case class Nested()                  extends scala.annotation.StaticAnnotation
case class Examples(
  example: Any,
  example2: Any = IgnoreExample,
  example3: Any = IgnoreExample,
  example4: Any = IgnoreExample,
  example5: Any = IgnoreExample
)                                    extends scala.annotation.StaticAnnotation
case class Description(text: String) extends scala.annotation.StaticAnnotation

case class ContractInfo(
  fullName: String,
  displayName: Option[String],
  schemaAnnotations: SchemaAnnotations,
  inherits: Vector[ContractInfo],
  fields: Map[String, SchemaAnnotations]
) {

  def isSubClass(contractInfo: ContractInfo): Boolean =
    contractInfo == this || inherits.exists(_.isSubClass(contractInfo))

  def getInheritedFieldAnnotation(name: String): Option[SchemaAnnotations] =
    fields.get(name).orElse(inherits.iterator.flatMap(_.getInheritedFieldAnnotation(name)).nextOption())
}

case class SchemaAnnotations(
  typeName: Option[String],
  title: Option[String],
  nested: Boolean,
  examples: List[Any],
  description: Option[String]
)

object SchemaAnnotations {
  val empty: SchemaAnnotations = SchemaAnnotations(None, None, false, Nil, None)
}
object SchemaReflection  {

  private val TypeTpe        = typeOf[Type]
  private val TitleTpe       = typeOf[Title]
  private val NestedTpe      = typeOf[Nested]
  private val ExampleTpe     = typeOf[Examples]
  private val DescriptionTpe = typeOf[Description]

  private val objectRegex = "\\$(\\w*)\\$.*".r

  def getDisplayName(contract: BaseContract[_]): String = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t      = mirror.classSymbol(contract.getClass)
    val schema = getSchemaAnnotation(t.annotations)
    schema.typeName.getOrElse(getDisplayName(t))
  }

  private def getDisplayName(t: ClassSymbol): String =
    t.name.toString match {
      case objectRegex(name) =>
        name
      case name              =>
        name
    }

  def getContractInfo[A <: dsentric.DObject](
    contract: BaseContract[A],
    current: Vector[ContractInfo] = Vector.empty
  ): (ContractInfo, Vector[ContractInfo]) = {
    val mirror         = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val typ            = mirror.classSymbol(contract.getClass)
    val instanceMirror = mirror.reflect(contract)
    getContractInfo(typ, current)(instanceMirror)
  }

  private def getContractInfo(t: ClassSymbol, current: Vector[ContractInfo])(
    instanceMirror: InstanceMirror
  ): (ContractInfo, Vector[ContractInfo]) = {
    val fullName = t.fullName
    current.find(_.fullName == fullName) match {
      case Some(ci) =>
        ci -> current
      case None     =>
        val schema                  = getSchemaAnnotation(t.annotations)
        val bc                      = getBaseClasses(t)
        val (inherited, newCurrent) =
          bc.foldLeft(Vector.empty[ContractInfo] -> current) { case ((b, v), e) =>
            val (ici, nv) = getContractInfo(e.asClass, v)(instanceMirror)
            (b :+ ici) -> nv
          }
        val annotationFields        = getFieldsSchemaAnnotations(t)(instanceMirror)
        val displayName             =
          schema.typeName.orElse {
            if (t.name.toString.startsWith("$anon$"))
              if (inherited.size == 1 && annotationFields.isEmpty)
                inherited.head.displayName
              else
                None
            else
              Some(getDisplayName(t))
          }
        val foldedInherited         = inherited.filterNot(i => inherited.exists(i2 => i2 != i && i2.isSubClass(i)))

        val contractInfo =
          ContractInfo(fullName, displayName, schema, foldedInherited, annotationFields)

        contractInfo -> (newCurrent :+ contractInfo)
    }
  }

  private val baseContractType: universe.Symbol =
    typeOf[BaseContract[_]].typeSymbol

  private def getBaseClasses(t: ClassSymbol): List[Symbol] =
    t.baseClasses.filter { c =>
      c != t &&
      !c.fullName.startsWith("dsentric.") &&
      c.asClass.baseClasses.contains(baseContractType)
    }

  private val propertyType: universe.Symbol =
    typeOf[Property[_, _]].typeSymbol

  def getFieldsSchemaAnnotations(t: ClassSymbol)(instanceMirror: InstanceMirror): Map[String, SchemaAnnotations] = {
    val members0 =
      t.toType.members
    val members  = members0.filter(m =>
      m.typeSignature.baseClasses.contains(
        propertyType
      ) && !m.isClass && m.owner == t && (t.isTrait || !m.isMethod) && m.overrides.isEmpty
    )

    members.collect {
      case (termSymbol: TermSymbol) if !termSymbol.isAccessor =>
        val rawObj = instanceMirror.reflectField(termSymbol).get
        val keyName = rawObj.asInstanceOf[Property[_, _]]._key
        val annotations = getSchemaAnnotation(termSymbol.annotations)
        keyName.trim() -> annotations
      case (methodSymbol: MethodSymbol) =>
        val rawObj = instanceMirror.reflectMethod(methodSymbol).apply()
        val keyName = rawObj.asInstanceOf[Property[_, _]]._key
        val annotations = getSchemaAnnotation(methodSymbol.annotations)
        keyName.trim() -> annotations
    }.toMap
  }

  private def getSchemaAnnotation(annotations: List[Annotation]): SchemaAnnotations =
    annotations
      .map(t => t.tree.tpe -> t.tree.children.tail)
      .foldLeft(SchemaAnnotations.empty) {
        case (s, (TypeTpe, tail))        =>
          s.copy(typeName = tail.collectFirst { case Literal(Constant(c: String)) => c })
        case (s, (TitleTpe, tail))       =>
          s.copy(title = tail.collectFirst { case Literal(Constant(c: String)) => c })
        case (s, (NestedTpe, _))         =>
          s.copy(nested = true)
        case (s, (ExampleTpe, tail))     =>
          s.copy(examples = tail.collect { case Literal(Constant(c)) => c })
        case (s, (DescriptionTpe, tail)) =>
          s.copy(description = tail.collectFirst { case Literal(Constant(c: String)) => c })
        case (s, (_, _))                 =>
          s
      }
}
