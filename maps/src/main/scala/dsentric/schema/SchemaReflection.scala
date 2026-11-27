package dsentric.schema

import dotty.tools.dotc.core.StdNames
import dsentric.contracts.{BaseContract, Property}
import dsentric.meta.{Annotation, BaseClass, FieldInfo, MethodInfo, TypeInfo, TypeTag}

import java.lang.reflect.{Field, Method, Modifier}
import scala.reflect.ClassTag

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
) extends scala.annotation.StaticAnnotation
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

  def getContractInfo[A <: BaseContract[?]](contract: A, current: Vector[ContractInfo] = Vector.empty)(using
    typeTag: TypeTag[A],
    classTag: ClassTag[A]
  ): (ContractInfo, Vector[ContractInfo]) =

    getContractInfo(
      contract,
      contract.getClass.getSimpleName,
      contract.getClass.getName,
      current,
      typeTag.annotations,
      typeTag.fields,
      typeTag.methods
    )

  def getContractInfoNested[A <: BaseContract[?]: ClassTag](
    contract: A,
    current: Vector[ContractInfo] = Vector.empty
  ): (ContractInfo, Vector[ContractInfo]) = {
    val clazz = contract.getClass
    if (!clazz.isAnonymousClass)
      getContractInfo(contract, current)(using TypeTag.ofClass(clazz.asInstanceOf[Class[A]]), summon[ClassTag[A]])
    else {
      val parents  = (clazz.getInterfaces.toList ++ Option(clazz.getSuperclass).toList)
        .filter(isUserDefined)
        .filter(classOf[BaseContract[?]].isAssignableFrom)
      val fullName = getDisplayName_(clazz.getName)
      current.find(_.fullName == fullName) match {
        case Some(info) => info -> current
        case None       =>
          val (inherited, updated) = parents.foldLeft(Vector.empty[ContractInfo] -> current) {
            case ((infos, known), parent) =>
              val tag          = TypeTag.ofClass(parent)
              val (info, next) = getContractInfo(
                contract,
                parent.getSimpleName,
                parent.getName,
                known,
                tag.annotations,
                tag.fields,
                tag.methods
              )(using tag)
              (infos :+ info) -> next
          }
          val fields               =
            getFieldsSchemaAnnotationsReflect(contract, clazz.getDeclaredFields.toList, clazz.getDeclaredMethods.toList)
              .filterNot { case (name, _) => inherited.exists(_.getInheritedFieldAnnotation(name).nonEmpty) }
          val displayName          =
            if (inherited.size == 1 && fields.isEmpty) inherited.head.displayName
            else Some(getDisplayName_(clazz.getSimpleName))
          val info                 = ContractInfo(fullName, displayName, SchemaAnnotations.empty, inherited, fields)
          info -> (updated :+ info)
      }
    }
  }

  private def getContractInfo[A <: BaseContract[?]](
    contract: A,
    simpleName: String,
    fullName: String,
    current: Vector[ContractInfo],
    annotations: List[Annotation],
    fields: List[FieldInfo[?]],
    methods: List[MethodInfo]
  )(using typeTag: TypeTag[?]): (ContractInfo, Vector[ContractInfo]) =
    val fullName_ = getDisplayName_(fullName)

    current.find(_.fullName == fullName_) match {
      case Some(ci) =>
        ci -> current
      case None     =>
        val schema = getSchemaAnnotation(annotations)

        def typeTagContractInfo =
          val baseClasses = getBaseClasses(typeTag)
          baseClasses.foldLeft(Vector.empty[ContractInfo] -> current) { case ((contracts, curr), baseClass) =>
            val baseTypeTag            = baseClass.typeTag
            val extraFields            =
              baseTypeTag.fields.filter(field => !fields.exists(f => f.name == field.name && f.owner == field.owner))
            val extraMethods           =
              baseTypeTag.methods.filter(method => !methods.exists(m => m.name == method.name && m.owner == method.owner))
            val (newContract, newCurr) = getContractInfo(
              contract,
              baseClass.name,
              baseClass.fullName,
              curr,
              baseClass.annotations,
              fields ++ extraFields,
              methods ++ extraMethods
            )(using baseTypeTag)
            (contracts :+ newContract) -> newCurr
          }

        val (inherited, newCurrent) = typeTagContractInfo
        val runtimeFields           =
          if (contract.getClass.isAnonymousClass && fullName_ == getDisplayName_(contract.getClass.getName))
            getFieldsSchemaAnnotationsReflect(
              contract,
              contract.getClass.getDeclaredFields.toList,
              contract.getClass.getDeclaredMethods.toList
            ).filterNot { case (name, _) => inherited.exists(_.getInheritedFieldAnnotation(name).nonEmpty) }
          else Map.empty[String, SchemaAnnotations]
        val annotationFields        =
          runtimeFields ++ getFieldsSchemaAnnotations(contract, fullName_, fields, methods)

        val displayName     =
          schema.typeName.orElse {
            if (contract.getClass.getName.contains("$anon$"))
              if (inherited.size == 1 && annotationFields.isEmpty)
                inherited.head.displayName
              else
                Some(getDisplayName_(simpleName))
            else
              Some(getDisplayName_(simpleName))
          }
        val foldedInherited = inherited.filterNot(i => inherited.exists(i2 => i2 != i && i2.isSubClass(i)))

        val contractInfo =
          ContractInfo(fullName_, displayName, schema, foldedInherited, annotationFields)

        contractInfo -> (newCurrent :+ contractInfo)
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
            case _                     =>
              None
        }

    val methodMembers =
      methods
        .filterNot(method => Modifier.isStatic(method.getModifiers))
        .filter(method => method.getParameterCount == 0 && method.canAccess(instance))
        .filter(method =>
          classOf[Property[?, ?]].isAssignableFrom(method.getReturnType) || classOf[BaseContract[?]]
            .isAssignableFrom(method.getReturnType)
        )
        .flatMap { method =>
          method.invoke(instance) match {
            case value: Property[?, ?] =>
              Some(value._key.trim() -> SchemaAnnotations.empty)
            case _                     =>
              None
          }
        }

    (fieldMembers ++ methodMembers).toMap
  }

  private def isUserDefined(clazz: Class[?]): Boolean =
    !clazz.getName.startsWith("scala.") && !clazz.getName.startsWith("java.") && !clazz.getName.startsWith("dsentric.")
}
