package dsentric.schema

import dsentric.contracts.BaseContract
import dsentric.meta.{TypeInfo, TypeTag}

final case class TypeContract[+T](contractInfo: ContractInfo, infos: Vector[ContractInfo])

object TypeContract:
  inline given of[C <: BaseContract[?]]: TypeContract[C] =
    ${ implementation[C] }

  import scala.quoted.*
  import SchemaReflection.{BaseContractTpe, PropertyTpe, getSchemaAnnotation}

  private[schema] class RuntimeTypeError(message: String) extends Error(message)

  private val objectRegex   = "\\$(\\w*)\\$.*".r
  private val dollarSign    = "(\\w*)\\$".r
  private val objDollarSign = "(\\w|.*)\\$".r

  given schemaAnnotationsToExpr: ToExpr[SchemaAnnotations] with
    def apply(schema: SchemaAnnotations)(using Quotes) =
      val typeNameExpr    = Expr(schema.typeName)
      val titleExpr       = Expr(schema.title)
      val nestedExpr      = Expr(schema.nested)
      val descriptionExpr = Expr(schema.description)
      '{
        SchemaAnnotations(
          $typeNameExpr,
          $titleExpr,
          $nestedExpr,
          Nil,
          $descriptionExpr
        )
      }

  given contractInfoToExpr: ToExpr[ContractInfo] with
    def apply(contractInfo: ContractInfo)(using Quotes) =
      val nameExpr     = Expr(contractInfo.fullName)
      val displayExpr  = Expr(contractInfo.displayName)
      val annExpr      = Expr(contractInfo.schemaAnnotations)
      val inheritsExpr = Expr.ofSeq(contractInfo.inherits.map(contractInfoToExpr(_)))
      val fieldsExpr   = Expr(contractInfo.fields)
      '{
        ContractInfo(
          $nameExpr,
          $displayExpr,
          $annExpr,
          $inheritsExpr.toVector,
          $fieldsExpr
        )
      }

  given typeContractToExpr[T]: ToExpr[TypeContract[T]] with
    def apply(typeContract: TypeContract[T])(using Quotes) =
      val contractInfoExpr = Expr(typeContract.contractInfo)
      val infoExpr         = Expr.ofSeq(typeContract.infos.map(contractInfoToExpr(_)))
      '{ TypeContract($contractInfoExpr, $infoExpr.toVector) }

  private def implementation[T: Type](using Quotes): Expr[TypeContract[T]] =
    import quotes.reflect.*

    TypeRepr.of[T] match
      case term@TypeRef(_: NoPrefix, n) if !term.typeSymbol.isClassDef && !term.typeSymbol.flags.is(Flags.Trait) && !term.typeSymbol.flags.is(Flags.Module) =>
        throw new RuntimeTypeError(s"Type must be known at compile time but got $n")
      case n =>
        val (contractInfo, infos) = resolveForTypeRepr[T](quotes)(n)
        val typeContract          = TypeContract[T](contractInfo, infos)
        Expr(typeContract)

  private def resolveForTypeRepr[T](quotes: Quotes)(
    typeRef: quotes.reflect.TypeRepr,
    current: Vector[ContractInfo] = Vector.empty
  ): (ContractInfo, Vector[ContractInfo]) =
    val symbol   = typeRef.typeSymbol
    val fullName = getDisplayName(normalizeClassName(symbol.fullName))

    current.find(_.fullName == fullName) match
      case Some(ci) =>
        ci -> current
      case None =>
        val simpleName        = normalizeClassName(symbol.name)
        val baseClasses       = resolveUserDefinedContractSymbols(quotes)(typeRef, typeRef.baseClasses)
        val annotations       = TypeTag.resolveAnnotations(quotes)(symbol.annotations)
        val schema            = getSchemaAnnotation(annotations)
        val (fields, methods) = resolvePropertyFields(quotes)(typeRef)
        val annotatedFields   = resolveSchemaAnnotations(quotes)(fields, methods)
        val innerClasses      = resolveUserDefinedContractSymbols(quotes)(typeRef, symbol.typeMembers.filter(_.isClassDef))
        val extraProps        = resolveExtraProperties(quotes)(baseClasses ++ innerClasses, fields, methods)

        val (inherited, newCurrent) =
          (baseClasses ++ innerClasses ++ extraProps).foldLeft(Vector.empty[ContractInfo] -> current) {
            case ((contracts, curr), baseClass) =>
              val (newContract, newCurr) = resolveForTypeRepr(quotes)(typeRef.memberType(baseClass), curr)
              (contracts :+ newContract) -> newCurr
          }

        val displayName =
          schema.typeName.orElse:
            if symbol.name.contains("$anon$") && inherited.size == 1 && fields.isEmpty
            then inherited.head.displayName
            else Some(getDisplayName(simpleName))

        val foldedInherited =
          inherited.filterNot(i => inherited.exists(i2 => i2 != i && i2.isSubClass(i)))

        val contractInfo =
          ContractInfo(fullName, displayName, schema, foldedInherited, annotatedFields)

        contractInfo -> (newCurrent :+ contractInfo)

  private def resolveUserDefinedContractSymbols(quotes: Quotes)(
    typeRef: quotes.reflect.TypeRepr,
    symbols: List[quotes.reflect.Symbol]
  ): List[quotes.reflect.Symbol] =
    val symbol   = typeRef.typeSymbol
    val typeName = normalizeClassName(symbol.name)
    val owner    = normalizeClassName(symbol.maybeOwner.fullName)

    symbols.filter: symbol =>
      val clazzRef    = typeRef.memberType(symbol)
      val typeSymbol  = clazzRef.typeSymbol
      val name        = typeSymbol.name
      val fullName    = normalizeClassName(typeSymbol.fullName)
      val clazzOwner  = normalizeClassName(typeSymbol.maybeOwner.fullName)
      val clazzName   = normalizeClassName(name)
      val userDefined = !isLangSymbol(quotes)(typeSymbol)
      val nonDsentric = !fullName.startsWith("dsentric.")
      val baseClasses = clazzRef.baseClasses.map(symbol => normalizeClassName(symbol.fullName))
      val hasProperty = baseClasses.exists(path => TypeInfo.is(BaseContractTpe.typeInfo, path))
      (clazzName != typeName || clazzOwner != owner) && userDefined && nonDsentric && hasProperty

  private def symbolIsOwnedProperty(quotes: Quotes, owner: String)(
    typeRef: quotes.reflect.TypeRepr,
    symbol: quotes.reflect.Symbol
  ): Boolean =
    symbol.maybeOwner.fullName == owner && typeRef.baseClasses.map(_.fullName).exists(path => TypeInfo.is(PropertyTpe.typeInfo, path))

  private def resolvePropertyFields(quotes: Quotes)(typeRef: quotes.reflect.TypeRepr): (List[quotes.reflect.Symbol], List[quotes.reflect.Symbol]) =
    val symbol  = typeRef.typeSymbol
    val owner   = symbol.fullName
    val fields  = symbol.fieldMembers.filter(symbol => symbolIsOwnedProperty(quotes, owner)(typeRef.memberType(symbol), symbol))
    val methods = symbol.methodMembers.filter(symbol => symbolIsOwnedProperty(quotes, owner)(typeRef.memberType(symbol), symbol))
    (fields, methods)

  private def resolveSchemaAnnotations(quotes: Quotes)(
    fields: List[quotes.reflect.Symbol],
    methods: List[quotes.reflect.Symbol]
  ): Map[String, SchemaAnnotations] =
    val fieldMembers =
      fields.map: field =>
        val annotations       = TypeTag.resolveAnnotations(quotes)(field.annotations)
        val schemaAnnotations = getSchemaAnnotation(annotations)
        field.name -> schemaAnnotations

    val methodMembers =
      methods.map: method =>
        val annotations       = TypeTag.resolveAnnotations(quotes)(method.annotations)
        val schemaAnnotations = getSchemaAnnotation(annotations)
        method.name -> schemaAnnotations

    (fieldMembers ++ methodMembers).toMap

  private def resolveExtraProperties(quotes: Quotes)(
    existing: List[quotes.reflect.Symbol],
    fields: List[quotes.reflect.Symbol],
    methods: List[quotes.reflect.Symbol]
  ): List[quotes.reflect.Symbol] =
    (fields ++ methods).flatMap: field =>
      resolveUserDefinedContractSymbols(quotes)(field.typeRef, field.typeRef.baseClasses).filterNot(existing.contains)

  private[schema] inline def normalizeClassName(name: String): String =
    name.stripSuffix("$")

  private inline def isLangSymbol(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    symbol.fullName.startsWith("scala.") || symbol.fullName.startsWith("java.")

  private inline def getDisplayName(str: String): String =
    str match
      case objectRegex(name)   =>
        normalize(name)
      case dollarSign(name)    =>
        normalize(name)
      case objDollarSign(name) =>
        normalize(name)
      case name                =>
        normalize(name)

  private inline def normalize(str: String): String =
    if str.contains("$anon$") then str else str.replace("$", ".").replace("..", ".")