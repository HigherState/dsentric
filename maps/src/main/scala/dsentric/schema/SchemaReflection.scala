package dsentric.schema

import dotty.tools.dotc.core.StdNames
import dsentric.contracts.{BaseContract, Property}
import dsentric.meta.{Annotation, BaseClass, FieldInfo, MethodInfo, TypeInfo, TypeTag}

import java.lang.reflect.{Field, Method, Modifier}
import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.*
import scala.util.Try

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
    contractInfo.fullName == fullName || inherits.exists(_.isSubClass(contractInfo))

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

  private[schema] val TypeTpe         = TypeTag.of[Type]
  private[schema] val TitleTpe        = TypeTag.of[Title]
  private[schema] val NestedTpe       = TypeTag.of[Nested]
  private[schema] val ExampleTpe      = TypeTag.of[Examples]
  private[schema] val DescriptionTpe  = TypeTag.of[Description]
  private[schema] val BaseContractTpe = TypeTag.of[BaseContract[?]]
  private[schema] val PropertyTpe     = TypeTag.of[Property[?, ?]]

  private val objectRegex   = "\\$(\\w*)\\$.*".r
  private val dollarSign    = "(\\w*)\\$".r
  private val objDollarSign = "(\\w|.*)\\$".r

  def getDisplayName_(str: String): String =
    str match {
      case objectRegex(name)   =>
        normalize(name)
      case dollarSign(name)    =>
        normalize(name)
      case objDollarSign(name) =>
        normalize(name)
      case name                =>
        normalize(name)
    }

  private inline def normalize(str: String): String =
    if str.contains("$anon$") then str else str.replace("$", ".")

  def getContractInfo[A <: BaseContract[?]](
    contract: A,
    current: Vector[ContractInfo] = Vector.empty
  )(using typeTag: TypeTag[A], classTag: ClassTag[A]): (ContractInfo, Vector[ContractInfo]) =
    lazy val mirror         = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    lazy val clazz          = contract.getClass
    lazy val typ            = Try(mirror.classSymbol(clazz)).orElse(Try(mirror.reflect(clazz).symbol)).toOption
    lazy val instanceMirror = mirror.reflect(contract)

    getContractInfo(
      contract,
      contract.getClass.getSimpleName,
      contract.getClass.getName,
      current,
      typeTag.annotations,
      typeTag.fields,
      typeTag.methods,
      typ,
      instanceMirror
    )

  def getContractInfoNested[A <: BaseContract[?]: ClassTag](
    contract: A,
    current: Vector[ContractInfo] = Vector.empty
  ): (ContractInfo, Vector[ContractInfo]) = {
    val clazz       = contract.getClass
    lazy val traits = clazz.getInterfaces.filter(isUserDefined).sortBy(-_.getInterfaces.count(isUserDefined)).toList
    val fullName    =
      if clazz.isAnonymousClass && traits.nonEmpty
      then getDisplayName_(traits.head.getName)
      else getDisplayName_(clazz.getName)

    current.find(_.fullName == fullName) match {
      case Some(ci) =>
        ci -> current
      case None =>
        val mirror          = scala.reflect.runtime.universe.runtimeMirror(clazz.getClassLoader)
        val instanceMirror  = mirror.reflect(contract)
        val tryClassSymbol  = Try(mirror.classSymbol(clazz))
        lazy val tryReflect = Try(mirror.reflect(clazz).symbol)
        val typ             = tryClassSymbol.orElse(tryReflect).toOption

        typ match
          case Some(classSymbol) =>
            val annotations = getSchemaAnnotation_(classSymbol.annotations)
            val fields      = clazz.getDeclaredFields.toList
            val methods     = clazz.getDeclaredMethods.toList

            val annotationFields =
              getFieldsSchemaAnnotationsReflect(contract, fields, methods) ++
              getFieldsSchemaAnnotations_(classSymbol)(instanceMirror)

            val displayName             =
              annotations.typeName.orElse {
                Some(getDisplayName_(clazz.getSimpleName))
              }

            val contractInfo            =
              ContractInfo(fullName, displayName, annotations, Vector.empty, annotationFields)

            contractInfo -> (current :+ contractInfo)

          case None =>
            getContractInfo(contract, current)(using
              TypeTag.ofClass(clazz.asInstanceOf[Class[A]]),
              summon[ClassTag[A]]
            )
    }
  }

  private def getContractInfo[A <: BaseContract[?]](
    contract: A,
    simpleName: String,
    fullName: String,
    current: Vector[ContractInfo],
    annotations: List[Annotation],
    fields: List[FieldInfo[?]],
    methods: List[MethodInfo],
    classSymbol: => Option[ClassSymbol],
    instanceMirror: => InstanceMirror
  )(using typeTag: TypeTag[?]): (ContractInfo, Vector[ContractInfo]) =
    val fullName_ = getDisplayName_(fullName)

    current.find(_.fullName == fullName_) match {
      case Some(ci) =>
        ci -> current
      case None =>
        val schema = getSchemaAnnotation(annotations)

        def reflectContractInfo(classSymbol: ClassSymbol) =
          val baseClasses = getBaseClasses(classSymbol)
          baseClasses.foldLeft(Vector.empty[ContractInfo] -> current) { case ((contracts, curr), baseClass) =>
            val (newContract, newCurr) = getContractInfoRuntime(baseClass.asClass, curr)(instanceMirror)
            (contracts :+ newContract) -> newCurr
          }

        def typeTagContractInfo =
          val baseClasses = getBaseClasses(typeTag)
          baseClasses.foldLeft(Vector.empty[ContractInfo] -> current) { case ((contracts, curr), baseClass) =>
            val baseTypeTag  = baseClass.typeTag
            val extraFields  = baseTypeTag.fields.filter(field => !fields.exists(f => f.name == field.name && f.owner == field.owner))
            val extraMethods = baseTypeTag.methods.filter(method => !methods.exists(m => m.name == method.name && m.owner == method.owner))
            val (newContract, newCurr) = getContractInfo(
              contract,
              baseClass.name,
              baseClass.fullName,
              curr,
              baseClass.annotations,
              fields ++ extraFields,
              methods ++ extraMethods,
              None,
              instanceMirror,
            )(using baseTypeTag)
            (contracts :+ newContract) -> newCurr
          }

        val (inherited, newCurrent) =
          classSymbol match
            case Some(cs) =>
              Try(typeTagContractInfo).getOrElse(reflectContractInfo(cs))
            case None =>
              typeTagContractInfo
        val annotationFields        =
          getFieldsSchemaAnnotations(contract, fullName_, fields, methods) ++
            classSymbol.map(symbol => getFieldsSchemaAnnotations_(symbol)(instanceMirror)).getOrElse(Map.empty)

        val displayName             =
          schema.typeName.orElse {
            if (contract.getClass.getName.contains("$anon$"))
              if (inherited.size == 1 && annotationFields.isEmpty)
                inherited.head.displayName
              else
                Some(getDisplayName_(simpleName))
            else
              Some(getDisplayName_(simpleName))
          }
        val foldedInherited         = inherited.filterNot(i => inherited.exists(i2 => i2 != i && i2.isSubClass(i)))

        val contractInfo            =
          ContractInfo(fullName_, displayName, schema, foldedInherited, annotationFields)

        contractInfo -> (newCurrent :+ contractInfo)
    }

  private def getContractInfoRuntime(t: ClassSymbol, current: Vector[ContractInfo])(
    instanceMirror: InstanceMirror
  ): (ContractInfo, Vector[ContractInfo]) = {
    val fullName = t.fullName
    current.find(_.fullName == fullName) match {
      case Some(ci) =>
        ci -> current
      case None =>
        val schema =
          getSchemaAnnotation_(t.annotations)
        val baseClasses             = getBaseClasses(t)
        val (inherited, newCurrent) =
          baseClasses.foldLeft(Vector.empty[ContractInfo] -> current) { case ((b, v), e) =>
            val (ici, nv) = getContractInfoRuntime(e.asClass, v)(instanceMirror)
            (b :+ ici) -> nv
          }
        val annotationFields        = getFieldsSchemaAnnotations_(t)(instanceMirror)
        val displayName             =
          schema.typeName.orElse {
            if (t.name.toString.startsWith("$anon$"))
              if (inherited.size == 1 && annotationFields.isEmpty)
                inherited.head.displayName
              else
                None
            else
              Some(getDisplayName_(t.name.toString))
          }
        val foldedInherited = inherited.filterNot(i => inherited.exists(i2 => i2 != i && i2.isSubClass(i)))

        val contractInfo =
          ContractInfo(fullName, displayName, schema, foldedInherited, annotationFields)

        contractInfo -> (newCurrent :+ contractInfo)
    }
  }

  private def getBaseClasses(t: ClassSymbol): List[Symbol] =
    t.baseClasses.filter { c =>
      c != t &&
        !c.fullName.startsWith("dsentric.") &&
        c.asClass.baseClasses.map(_.fullName).exists(_.contains("BaseContract"))
    }

  def getBaseClasses(typeTag: TypeTag[?]): List[BaseClass] =
    typeTag.baseClasses.filter { c =>
      (c.name != typeTag.name || c.owner != typeTag.owner) && c.userDefined && !c.fullName.startsWith("dsentric.") &&
        c.baseClasses.map(_.fullName).exists(path => TypeInfo.is(BaseContractTpe.typeInfo, path))
    }

  def getSchemaAnnotation(annotations: List[Annotation]): SchemaAnnotations =
    annotations
      .foldLeft(SchemaAnnotations.empty) { case (s, annotation) =>
        annotation.properties match {
          case (_, typeName) :: _ if TypeInfo.is(TypeTpe.typeInfo, annotation.fullName)           =>
            s.copy(typeName = Some(typeName.asInstanceOf[String]))
          case (_, title) :: _ if TypeInfo.is(TitleTpe.typeInfo, annotation.fullName)             =>
            s.copy(title = Some(title.asInstanceOf[String]))
          case _ if TypeInfo.is(NestedTpe.typeInfo, annotation.fullName)                          =>
            s.copy(nested = true)
          case examples if TypeInfo.is(ExampleTpe.typeInfo, annotation.fullName)                  =>
            s.copy(examples = examples.map(_._2))
          case (_, description) :: _ if TypeInfo.is(DescriptionTpe.typeInfo, annotation.fullName) =>
            s.copy(description = Some(description.asInstanceOf[String]))
          case _                                                                                  =>
            s
        }
      }

  private def getSchemaAnnotation_(annotations: List[universe.Annotation]): SchemaAnnotations =
    annotations
      .map(t => t.tree.tpe.typeSymbol.fullName -> t.tree.children.tail)
      .foldLeft(SchemaAnnotations.empty) {
        case (s, (path, tail)) if TypeInfo.is(TypeTpe.typeInfo, path)        =>
          s.copy(typeName = tail.collectFirst { case Literal(Constant(c: String)) => c })
        case (s, (path, tail)) if TypeInfo.is(TitleTpe.typeInfo, path)       =>
          s.copy(title = tail.collectFirst { case Literal(Constant(c: String)) => c })
        case (s, (path, _)) if TypeInfo.is(NestedTpe.typeInfo, path)         =>
          s.copy(nested = true)
        case (s, (path, tail)) if TypeInfo.is(ExampleTpe.typeInfo, path)     =>
          s.copy(examples = tail.collect { case Literal(Constant(c)) => c })
        case (s, (path, tail)) if TypeInfo.is(DescriptionTpe.typeInfo, path) =>
          s.copy(description = tail.collectFirst { case Literal(Constant(c: String)) => c })
        case (s, (_, _))                 =>
          s
      }

  private def getFieldsSchemaAnnotations(
    instance: AnyRef,
    owner: String,
    fields: List[FieldInfo[?]],
    methods: List[MethodInfo]
  ): Map[String, SchemaAnnotations] = {
    val fieldMembers =
      fields
        .filter(_.owner == owner)
        .filter(_.baseClasses.map(_.fullName).exists(path => TypeInfo.is(PropertyTpe.typeInfo, path)))
        .map { field =>
          val rawObj      = field.reflect(instance)
          val keyName     = rawObj.asInstanceOf[Property[?, ?]]._key
          val annotations = getSchemaAnnotation(field.annotations)
          keyName.trim() -> annotations
        }

    val methodMembers =
      methods
        .filter(_.owner == owner)
        .filter(_.baseClasses.map(_.fullName).exists(path => TypeInfo.is(PropertyTpe.typeInfo, path)))
        .map { method =>
          val rawObj      = method.reflect(instance)
          val keyName     = rawObj.asInstanceOf[Property[?, ?]]._key
          val annotations = getSchemaAnnotation(method.annotations)
          keyName.trim() -> annotations
        }

    (fieldMembers ++ methodMembers).toMap
  }

  private def getFieldsSchemaAnnotationsReflect(
    instance: AnyRef,
    fields: List[Field],
    methods: List[Method]
  ): Map[String, SchemaAnnotations] = {
    val fieldMembers =
      fields
        .filterNot(_.getName.startsWith(StdNames.nme.LAZY_FIELD_OFFSET.toString))
        .filterNot(field => Modifier.isStatic(field.getModifiers))
        .filter(_.canAccess(instance))
        .flatMap { field =>
          field.get(instance) match
            case value: Property[?, ?] =>
              Some(value._key.trim() -> SchemaAnnotations.empty)
            case _ =>
              None
        }

    val methodMembers =
      methods
        .filterNot(method => Modifier.isStatic(method.getModifiers))
        .filter(method => method.getParameterCount == 0 && method.canAccess(instance))
        .flatMap { method =>
          method.invoke(instance) match {
            case value: Property[?, ?] =>
              Some(value._key.trim() -> SchemaAnnotations.empty)
            case _ =>
              None
          }
        }

    (fieldMembers ++ methodMembers).toMap
  }

  def getFieldsSchemaAnnotations_(t: ClassSymbol)(instanceMirror: InstanceMirror): Map[String, SchemaAnnotations] = {
    val members0 =
      t.toType.decls

    val members = members0.filter(m =>
      m.typeSignature.baseClasses.map(_.fullName).exists(
        TypeInfo.is(PropertyTpe.typeInfo, _)
      ) && !m.isClass && m.owner == t && (t.isTrait || !m.isMethod) && m.overrides.isEmpty
    )

    members.collect {
      case (termSymbol: TermSymbol) if !termSymbol.isAccessor && !termSymbol.isVar =>
        val rawObj      = instanceMirror.reflectField(termSymbol).get
        val keyName     = rawObj.asInstanceOf[Property[?, ?]]._key
        val annotations = getSchemaAnnotation_(termSymbol.annotations)
        keyName.trim() -> annotations
      case (methodSymbol: MethodSymbol)                       =>
        val rawObj      = instanceMirror.reflectMethod(methodSymbol).apply()
        val keyName     = rawObj.asInstanceOf[Property[?, ?]]._key
        val annotations = getSchemaAnnotation_(methodSymbol.annotations)
        keyName.trim() -> annotations
    }.toMap
  }

  private def isUserDefined(clazz: Class[?]): Boolean =
    !clazz.getName.startsWith("scala.") && !clazz.getName.startsWith("java.") && !clazz.getName.startsWith("dsentric.")
}