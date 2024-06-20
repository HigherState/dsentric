package dsentric.meta

import scala.annotation.tailrec

trait HasAccessorField:
  def accessor: Accessor
  def isPrivate: Boolean = accessor == Accessor.Private
  def isProtected: Boolean = accessor == Accessor.Protected

trait HasBaseClasses:
  def baseClasses: List[BaseClass]

case class BaseClass(
  name: String,
  fullName: String,
  annotations: List[Annotation],
  owner: String,
  userDefined: Boolean,
  isClass: Boolean,
  isCase: Boolean,
  isTrait: Boolean,
  accessor: Accessor
) extends HasAccessorField with HasBaseClasses:
  def typeTag: TypeTag[_] =
    TypeTag.ofClass(Class.forName(fullName))

  override def baseClasses: List[BaseClass] =
    typeTag.baseClasses

case class FieldInfo[T](
  name: String,
  fullName: String,
  typeInfo: TypeInfo,
  annotations: List[Annotation],
  baseClasses: List[BaseClass],
  owner: String,
  inherited: Boolean,
  userDefined: Boolean,
  isLazy: Boolean,
  isImplicit: Boolean,
  isGiven: Boolean,
  isFinal: Boolean,
  isInline: Boolean,
  isMutable: Boolean,
  accessor: Accessor,
  parentClassPath: String
) extends HasAccessorField with HasBaseClasses:
  type Out = T

  def typeTags: List[TypeTag[_]] =
    TypeInfo.unsafePaths(typeInfo).map: path =>
      TypeTag.ofClass(TypeTag.findClass(path))

  def reflect(instance: AnyRef): Any =
    val clazz  = TypeTag.findClass(parentClassPath)
    val fields = clazz.getDeclaredFields.toList

    fields.find(_.getName == name) match
      case Some(field) =>
        field.setAccessible(true)
        field.get(instance)
      case None        =>
        val method = clazz.getDeclaredMethod(name)
        method.setAccessible(true)
        method.invoke(instance)

case class MethodInfo(
  name: String,
  typeParams: List[(String, TypeInfo.TypeBounds)],
  parameters: List[List[(String, TypeInfo)]],
  implicits: List[List[(String, TypeInfo)]],
  returnTypePath: String,
  returnType: TypeInfo,
  annotations: List[Annotation],
  baseClasses: List[BaseClass],
  owner: String,
  inherited: Boolean,
  userDefined: Boolean,
  isImplicit: Boolean,
  isGiven: Boolean,
  isFinal: Boolean,
  isInline: Boolean,
  isTransparent: Boolean,
  accessor: Accessor,
  parentClassPath: String
) extends HasAccessorField with HasBaseClasses:
  def typeTags: List[TypeTag[_]] =
    TypeInfo.unsafePaths(returnType).map: path =>
      TypeTag.ofClass(TypeTag.findClass(path))

  def reflect(instance: AnyRef): Any =
    val clazz  = Class.forName(parentClassPath)
    val method = clazz.getDeclaredMethod(name)
    method.setAccessible(true)
    method.invoke(instance)

case class TypeFieldInfo(
  name: String,
  typeInfo: TypeInfo,
  annotations: List[Annotation],
  owner: String,
  inherited: Boolean,
  userDefined: Boolean,
  accessor: Accessor
) extends HasAccessorField

case class Child(
  name: String,
  typeInfo: TypeInfo,
  annotations: List[Annotation],
  owner: String,
  userDefined: Boolean,
  isFinal: Boolean,
  isClass: Boolean,
  isCase: Boolean,
  isTrait: Boolean,
  isModule: Boolean,
  accessor: Accessor
) extends HasAccessorField

case class TypeTag[T](
  name: String,
  typeInfo: TypeInfo,
  annotations: List[Annotation],
  owner: String,
  fields: List[FieldInfo[?]],
  methods: List[MethodInfo],
  types: List[TypeFieldInfo],
  baseClasses: List[BaseClass],
  children: List[Child],
  isFinal: Boolean,
  isModule: Boolean,
  isAbstract: Boolean,
  isTrait: Boolean,
  isEnum: Boolean,
  isSealed: Boolean,
  accessor: Accessor
) extends HasAccessorField

case class Annotation(
  name: String,
  fullName: String,
  properties: List[(String, PrimitiveType)]
)

type PrimitiveType = Int | String | Boolean | Long | Unit | Double | Float | Short | Byte | Char

enum Accessor:
  case Public, Protected, Private

enum ConstantValue:
  case Primitive(value: PrimitiveType)
  case Other(representation: String)

enum TypeInfo:
  case Simple(name: String, path: String)
  case Intersection(first: TypeInfo, second: TypeInfo)
  case Union(first: TypeInfo, second: TypeInfo)
  case TypeBounds(lower: TypeInfo, upper: TypeInfo)
  case Applied(constructor: TypeInfo, args: List[TypeInfo])
  case ByName(underlying: TypeInfo)
  case Reference(names: List[String])
  case Refinement(base: TypeInfo, refined: List[(String, TypeInfo)])
  case Method(
    typeParams: List[(String, TypeBounds)],
    parameters: List[List[(String, TypeInfo)]],
    implicits: List[List[(String, TypeInfo)]],
    returnType: TypeInfo
  )
  case Constant(value: ConstantValue)
  case Repeated
  case Unknown

object TypeInfo:
  def is(typeInfo: TypeInfo, path: String): Boolean =
    typeInfo match
      case Simple(_, path0)     => path == path0
      case Intersection(_, _)   => false
      case Union(fst, snd)      => is(fst, path) || is(snd, path)
      case TypeBounds(_, _)     => false
      case Applied(tpe, _)      => is(tpe, path)
      case ByName(tpe)          => is(tpe, path)
      case Reference(_)         => false
      case Refinement(tpe, _)   => is(tpe, path)
      case Method(_, _, _, tpe) => is(tpe, path)
      case Constant(_)          => false
      case Repeated             => false
      case Unknown              => false

  def unsafePaths(typeInfo: TypeInfo): List[String] =
    typeInfo match
      case Simple(_, path)          => List(path)
      case Intersection(tpe0, tpe1) => unsafePaths(tpe0) ++ unsafePaths(tpe1)
      case Union(tpe0, tpe1)        => unsafePaths(tpe0) ++ unsafePaths(tpe1)
      case TypeBounds(_, _)         => Nil
      case Applied(tpe, _)          => unsafePaths(tpe)
      case ByName(tpe)              => unsafePaths(tpe)
      case Reference(_)             => Nil
      case Refinement(tpe, _)       => unsafePaths(tpe)
      case Method(_, _, _, tpe)     => unsafePaths(tpe)
      case Constant(_)              => Nil
      case Repeated                 => Nil
      case Unknown                  => Nil

object TypeTag:
  import scala.quoted.*
  import scala.quoted.staging.*

  given Compiler = Compiler.make(getClass.getClassLoader)

  private[meta] class RuntimeTypeError(message: String) extends Error(message)

  inline def of[T]: TypeTag[T] =
    ${ typeTagOfImpl[T] }

  def ofClass[T](clazz: Class[T]): TypeTag[T] =
    val fn = (qctx: Quotes) ?=> resolveForTypeRepr[T](quotes)(quotes.reflect.TypeRepr.typeConstructorOf(clazz))
    withQuotes(fn)

  given exprToAccessor: ToExpr[Accessor] with
    def apply(accessor: Accessor)(using Quotes) =
      accessor match
        case Accessor.Public    => '{Accessor.Public}
        case Accessor.Private   => '{Accessor.Private}
        case Accessor.Protected => '{Accessor.Protected}

  given exprToPrimitiveType: ToExpr[PrimitiveType] with
    def apply(primitive: PrimitiveType)(using Quotes) =
      primitive match
        case int: Int       => Expr(int)
        case str: String    => Expr(str)
        case bool: Boolean  => Expr(bool)
        case _: Unit        => '{()}
        case long: Long     => Expr(long)
        case double: Double => Expr(double)
        case float: Float   => Expr(float)
        case byte: Byte     => Expr(byte)
        case short: Short   => Expr(short)
        case char: Char     => Expr(char)

  given exprToConstantValue: ToExpr[ConstantValue] with
    def apply(constant: ConstantValue)(using Quotes) =
      constant match
        case ConstantValue.Primitive(primitive: PrimitiveType) =>
          val expr = Expr(primitive)
          '{ConstantValue.Primitive($expr)}
        case ConstantValue.Other(representation)               =>
          val expr = Expr(representation)
          '{ConstantValue.Other($expr)}

  given exprToTypeInfoBounds: ToExpr[TypeInfo.TypeBounds] with
    def apply(typeBounds: TypeInfo.TypeBounds)(using Quotes) =
      val lowerExpr = Expr(typeBounds.lower)
      val upperExpr = Expr(typeBounds.upper)
      '{TypeInfo.TypeBounds($lowerExpr, $upperExpr)}

  given exprToTypeInfo: ToExpr[TypeInfo] with
    def apply(typeInfo: TypeInfo)(using Quotes) =
      typeInfo match
        case TypeInfo.Simple(name, path)                          =>
          val nameExpr = Expr(name)
          val pathExpr = Expr(path)
          '{TypeInfo.Simple($nameExpr, $pathExpr)}
        case TypeInfo.Intersection(first, second)                 =>
          val firstExpr  = Expr(first)
          val secondExpr = Expr(second)
          '{TypeInfo.Intersection($firstExpr, $secondExpr)}
        case TypeInfo.Union(first, second)                        =>
          val firstExpr = Expr(first)
          val secondExpr = Expr(second)
          '{TypeInfo.Union($firstExpr, $secondExpr)}
        case tpeBs @ TypeInfo.TypeBounds(_, _)                    =>
          exprToTypeInfoBounds(tpeBs)
        case TypeInfo.Applied(constructor, args)                  =>
          val constructorExpr = Expr(constructor)
          val argsExpr        = Expr(args)
          '{TypeInfo.Applied($constructorExpr, $argsExpr)}
        case TypeInfo.ByName(underlying)                          =>
          val expr = Expr(underlying)
          '{TypeInfo.ByName($expr)}
        case TypeInfo.Reference(names)                            =>
          val namesExpr = Expr(names)
          '{TypeInfo.Reference($namesExpr)}
        case TypeInfo.Refinement(base, refined)                   =>
          val baseExpr    = Expr(base)
          val refinedExpr = Expr(refined)
          '{TypeInfo.Refinement($baseExpr, $refinedExpr)}
        case TypeInfo.Method(types, params, implicits, returnTpe) =>
          val typesExpr     = Expr(types)
          val paramsExpr    = Expr(params)
          val implicitsExpr = Expr(implicits)
          val returnExpr    = Expr(returnTpe)
          '{TypeInfo.Method($typesExpr, $paramsExpr, $implicitsExpr, $returnExpr)}
        case TypeInfo.Constant(value)                             =>
          val valueExpr = Expr(value)
          '{TypeInfo.Constant($valueExpr)}
        case TypeInfo.Repeated                                    =>
          '{TypeInfo.Repeated}
        case TypeInfo.Unknown                                     =>
          '{TypeInfo.Unknown}

  given exprToAnnotation: ToExpr[Annotation] with
    def apply(a: Annotation)(using Quotes) =
      val nameExpr = Expr(a.name)
      val fullExpr = Expr(a.fullName)
      val propExpr = Expr(a.properties)
      '{Annotation($nameExpr, $fullExpr, $propExpr)}

  given exprToBaseClass: ToExpr[BaseClass] with
    def apply(b: BaseClass)(using Quotes) =
      val nameExpr     = Expr(b.name)
      val fullExpr     = Expr(b.fullName)
      val annExpr      = Expr(b.annotations)
      val ownerExpr    = Expr(b.owner)
      val userDExpr    = Expr(b.userDefined)
      val classExpr    = Expr(b.isClass)
      val caseExpr     = Expr(b.isCase)
      val traitExpr    = Expr(b.isTrait)
      val accessorExpr = Expr(b.accessor)
      '{
        BaseClass(
          $nameExpr,
          $fullExpr,
          $annExpr,
          $ownerExpr,
          $userDExpr,
          $classExpr,
          $caseExpr,
          $traitExpr,
          $accessorExpr
        )
      }

  given exprToFieldInfo: ToExpr[FieldInfo[?]] with
    def apply(f: FieldInfo[?])(using Quotes) =
      val nameExpr     = Expr(f.name)
      val pathExpr     = Expr(f.fullName)
      val typeExpr     = Expr(f.typeInfo)
      val annExpr      = Expr(f.annotations)
      val baseExpr     = Expr(f.baseClasses)
      val ownerExpr    = Expr(f.owner)
      val inheritExpr  = Expr(f.inherited)
      val userDExpr    = Expr(f.userDefined)
      val lazyExpr     = Expr(f.isLazy)
      val implicitExpr = Expr(f.isImplicit)
      val givenExpr    = Expr(f.isGiven)
      val finalExpr    = Expr(f.isFinal)
      val inlineExpr   = Expr(f.isInline)
      val mutableExpr  = Expr(f.isMutable)
      val accessorExpr = Expr(f.accessor)
      val parentExpr   = Expr(f.parentClassPath)
      '{
        FieldInfo(
          $nameExpr,
          $pathExpr,
          $typeExpr,
          $annExpr,
          $baseExpr,
          $ownerExpr,
          $inheritExpr,
          $userDExpr,
          $lazyExpr,
          $implicitExpr,
          $givenExpr,
          $finalExpr,
          $inlineExpr,
          $mutableExpr,
          $accessorExpr,
          $parentExpr
        )
      }

  given exprToMethodInfo: ToExpr[MethodInfo] with
    def apply(m: MethodInfo)(using Quotes) =
      val nameExpr        = Expr(m.name)
      val typeExpr        = Expr(m.typeParams)
      val paramsExpr      = Expr(m.parameters)
      val implcsExpr      = Expr(m.implicits)
      val returnPathExpr  = Expr(m.returnTypePath)
      val returnExpr      = Expr(m.returnType)
      val annExpr         = Expr(m.annotations)
      val baseExpr        = Expr(m.baseClasses)
      val ownerExpr       = Expr(m.owner)
      val inheExpr        = Expr(m.inherited)
      val userDExpr       = Expr(m.userDefined)
      val implicitExpr    = Expr(m.isImplicit)
      val givenExpr       = Expr(m.isGiven)
      val finalExpr       = Expr(m.isFinal)
      val inlineExpr      = Expr(m.isInline)
      val transparentExpr = Expr(m.isTransparent)
      val accessorExpr    = Expr(m.accessor)
      val parentExpr      = Expr(m.parentClassPath)
      '{
        MethodInfo(
          $nameExpr,
          $typeExpr,
          $paramsExpr,
          $implcsExpr,
          $returnPathExpr,
          $returnExpr,
          $annExpr,
          $baseExpr,
          $ownerExpr,
          $inheExpr,
          $userDExpr,
          $implicitExpr,
          $givenExpr,
          $finalExpr,
          $inlineExpr,
          $transparentExpr,
          $accessorExpr,
          $parentExpr
        )
      }

  given exprToTypeFieldInfo: ToExpr[TypeFieldInfo] with
    def apply(t: TypeFieldInfo)(using Quotes) =
      val nameExpr     = Expr(t.name)
      val typeExpr     = Expr(t.typeInfo)
      val annExpr      = Expr(t.annotations)
      val ownerExpr    = Expr(t.owner)
      val inheExpr     = Expr(t.inherited)
      val userDExpr    = Expr(t.userDefined)
      val accessorExpr = Expr(t.accessor)
      '{TypeFieldInfo($nameExpr, $typeExpr, $annExpr, $ownerExpr, $inheExpr, $userDExpr, $accessorExpr)}

  given exprToChild: ToExpr[Child] with
    def apply(c: Child)(using Quotes) =
      val nameExpr     = Expr(c.name)
      val typeExpr     = Expr(c.typeInfo)
      val annExpr      = Expr(c.annotations)
      val ownerExpr    = Expr(c.owner)
      val userDExpr    = Expr(c.userDefined)
      val finalExpr    = Expr(c.isFinal)
      val classExpr    = Expr(c.isClass)
      val caseExpr     = Expr(c.isCase)
      val traitExpr    = Expr(c.isTrait)
      val moduleExpr   = Expr(c.isModule)
      val accessorExpr = Expr(c.accessor)
      '{
        Child(
          $nameExpr,
          $typeExpr,
          $annExpr,
          $ownerExpr,
          $userDExpr,
          $finalExpr,
          $classExpr,
          $caseExpr,
          $traitExpr,
          $moduleExpr,
          $accessorExpr
        )
      }

  given exprToTag[T: Type]: ToExpr[TypeTag[T]] with
    def apply(t: TypeTag[T])(using Quotes) =
      val nameExpr     = Expr(t.name)
      val typeExpr     = Expr(t.typeInfo)
      val annExpr      = Expr(t.annotations)
      val ownerExpr    = Expr(t.owner)
      val fieldExpr    = Expr(t.fields)
      val methodExpr   = Expr(t.methods)
      val typesExpr    = Expr(t.types)
      val baseExpr     = Expr(t.baseClasses)
      val childrenExpr = Expr(t.children)
      val finalExpr    = Expr(t.isFinal)
      val moduleExpr   = Expr(t.isModule)
      val abstractExpr = Expr(t.isAbstract)
      val traitExpr    = Expr(t.isTrait)
      val enumExpr     = Expr(t.isEnum)
      val sealedExpr   = Expr(t.isSealed)
      val accessorExpr = Expr(t.accessor)
      '{
        TypeTag[T](
          $nameExpr,
          $typeExpr,
          $annExpr,
          $ownerExpr,
          $fieldExpr,
          $methodExpr,
          $typesExpr,
          $baseExpr,
          $childrenExpr,
          $finalExpr,
          $moduleExpr,
          $abstractExpr,
          $traitExpr,
          $enumExpr,
          $sealedExpr,
          $accessorExpr
        )
      }

  inline given typeTag[T]: TypeTag[T] =
    TypeTag.of[T]

  private def typeTagOfImpl[T: Type](using Quotes): Expr[TypeTag[T]] =
    import quotes.reflect.*

    TypeRepr.of[T] match
      case term @ TypeRef(_: NoPrefix, n) if !term.typeSymbol.isClassDef && !term.typeSymbol.flags.is(Flags.Trait) && !term.typeSymbol.flags.is(Flags.Module) =>
        throw new RuntimeTypeError(s"Type must be known at compile time but got $n")
      case n =>
        Expr(resolveForTypeRepr[T](quotes)(n))

  private def resolveForTypeRepr[T](quotes: Quotes)(typeRef: quotes.reflect.TypeRepr): TypeTag[T] =
    import quotes.reflect.*

    inline def symbolFullName(symbol: Symbol): String =
      typeRef.memberType(symbol).typeSymbol.fullName

    inline def resolveField(field: Symbol, classPath: String): FieldInfo[?] =
      val declared  = typeRef.typeSymbol.declaredFields
      val fullName  = symbolFullName(field)
      val inherited = !declared.exists(symbol => symbolFullName(symbol) == fullName && symbol.name == field.name)
      resolveForField(quotes)(field, typeRef.memberType(field), classPath, inherited)

    inline def resolveMethod(method: Symbol, classPath: String): MethodInfo =
      val declared  = typeRef.typeSymbol.declaredMethods
      val fullName  = symbolFullName(method)
      val inherited = !declared.exists(symbol => symbolFullName(symbol) == fullName && symbol.name == method.name)
      resolveForMethod(quotes)(method, typeRef.memberType(method), classPath, inherited)

    inline def resolveType_(typeSymbol: Symbol): TypeFieldInfo =
      val declared    = typeRef.typeSymbol.declaredTypes
      val fullName    = symbolFullName(typeSymbol)
      val inherited   = !declared.exists(symbol => symbolFullName(symbol) == fullName && symbol.name == typeSymbol.name)
      resolveType(quotes)(typeSymbol, typeRef.memberType(typeSymbol), typeRef.select(typeSymbol), inherited)

    val symbol      = typeRef.typeSymbol
    val classPath   = symbol.fullName
    val owner       = symbol.maybeOwner.fullName
    val typeName    = normalizeClassName(symbol.name)
    val typeInfo    = resolveTypeInfo(quotes)(typeRef)
    val annotations = resolveAnnotations(quotes)(symbol.annotations)
    val baseClasses = typeRef.baseClasses.map(clazz => resolveForClass(quotes)(clazz, typeRef.memberType(clazz)))
    val fields      = symbol.fieldMembers.map(resolveField(_, classPath))
    val methods     = symbol.methodMembers.map(resolveMethod(_, classPath))
    val types       = symbol.typeMembers.filterNot(_.isNoSymbol).map(resolveType_(_))
    val children    = symbol.children.map(child => resolveForChild(quotes)(child, typeRef.memberType(child), typeRef.select(child)))
    val isFinal     = symbol.flags.is(Flags.Final)
    val isModule    = symbol.flags.is(Flags.Module)
    val isAbstract  = symbol.flags.is(Flags.Abstract)
    val isTrait     = symbol.flags.is(Flags.Trait)
    val isEnum      = symbol.flags.is(Flags.Enum)
    val isSealed    = symbol.flags.is(Flags.Sealed)
    val isPrivate   = symbol.flags.is(Flags.Private)
    val isProtected = symbol.flags.is(Flags.Protected)
    val accessor    = if isPrivate then Accessor.Private else if isProtected then Accessor.Protected else Accessor.Public

    TypeTag(
      typeName,
      typeInfo,
      annotations,
      owner,
      fields,
      methods,
      types,
      baseClasses,
      children,
      isFinal,
      isModule,
      isAbstract,
      isTrait,
      isEnum,
      isSealed,
      accessor
    )

  private def resolveForClass(quotes: Quotes)(
    symbol: quotes.reflect.Symbol,
    typeRef: quotes.reflect.TypeRepr
  ): BaseClass =
    import quotes.reflect.Flags

    val typeSymbol  = typeRef.typeSymbol
    val owner       = normalizeClassName(symbol.maybeOwner.fullName)
    val typeName    = normalizeClassName(typeSymbol.name)
    val fullName    = normalizeClassName(typeSymbol.fullName)
    val annotations = resolveAnnotations(quotes)(symbol.annotations)
    val userDefined = !isLangSymbol(quotes)(typeSymbol)
    val isClass     = symbol.isClassDef
    val isCase      = symbol.flags.is(Flags.Case)
    val isTrait     = symbol.flags.is(Flags.Trait)
    val isPrivate   = symbol.flags.is(Flags.Private)
    val isProtected = symbol.flags.is(Flags.Protected)
    val accessor    = if isPrivate then Accessor.Private else if isProtected then Accessor.Protected else Accessor.Public
    BaseClass(typeName, fullName, annotations, owner, userDefined, isClass, isCase, isTrait, accessor)

  private def resolveForField(quotes: Quotes)(
    symbol: quotes.reflect.Symbol,
    typeRef: quotes.reflect.TypeRepr,
    parentClassPath: String,
    inherited: Boolean
  ): FieldInfo[?] =
    import quotes.reflect.*

    val name        = symbol.name
    val typeSymbol  = typeRef.typeSymbol
    val fullName    = normalizeClassName(typeSymbol.fullName)
    val typeInfo    = resolveTypeInfo(quotes)(typeRef)
    val owner       = normalizeClassName(symbol.maybeOwner.fullName)
    val annotations = resolveAnnotations(quotes)(symbol.annotations)
    val baseClasses = typeRef.baseClasses.map(clazz => resolveForClass(quotes)(clazz, typeRef.memberType(clazz)))
    val userDefined = !isLangSymbol(quotes)(symbol.maybeOwner)
    val isLazy      = symbol.flags.is(Flags.Lazy)
    val isImplicit  = symbol.flags.is(Flags.Implicit)
    val isGiven     = symbol.flags.is(Flags.Given)
    val isFinal     = symbol.flags.is(Flags.Final)
    val isInline    = symbol.flags.is(Flags.Inline)
    val isMutable   = symbol.flags.is(Flags.Mutable)
    val isPrivate   = symbol.flags.is(Flags.Private)
    val isProtected = symbol.flags.is(Flags.Protected)
    val accessor    = if isPrivate then Accessor.Private else if isProtected then Accessor.Protected else Accessor.Public
    val typeTpe     = typeRef.asType

    new FieldInfo(
      name,
      fullName,
      typeInfo,
      annotations,
      baseClasses,
      owner,
      inherited,
      userDefined,
      isLazy,
      isImplicit,
      isGiven,
      isFinal,
      isInline,
      isMutable,
      accessor,
      parentClassPath
    )

  private def resolveForChild(quotes: Quotes)(
    symbol: quotes.reflect.Symbol,
    typeRef: quotes.reflect.TypeRepr,
    fallback: quotes.reflect.TypeRepr
  ): Child =
    import quotes.reflect.Flags

    val name        = symbol.name
    val typeInfo    = resolveTypeInfoWithFallback(quotes)(typeRef, fallback)
    val owner       = normalizeClassName(symbol.maybeOwner.fullName)
    val annotations = resolveAnnotations(quotes)(symbol.annotations)
    val userDefined = !isLangSymbol(quotes)(symbol.maybeOwner)
    val isFinal     = symbol.flags.is(Flags.Final)
    val isClass     = symbol.isClassDef
    val isCase      = symbol.flags.is(Flags.Case)
    val isTrait     = symbol.flags.is(Flags.Trait)
    val isModule    = symbol.flags.is(Flags.Module)
    val isPrivate   = symbol.flags.is(Flags.Private)
    val isProtected = symbol.flags.is(Flags.Protected)
    val accessor    = if isPrivate then Accessor.Private else if isProtected then Accessor.Protected else Accessor.Public

    Child(
      name,
      typeInfo,
      annotations,
      owner,
      userDefined,
      isFinal,
      isClass,
      isCase,
      isTrait,
      isModule,
      accessor
    )

  private def resolveForMethod(quotes: Quotes)(
    symbol: quotes.reflect.Symbol,
    typeRef: quotes.reflect.TypeRepr,
    parentClassPath: String,
    inherited: Boolean
  ): MethodInfo =
    import quotes.reflect.*

    def methodInfo(
      typeParams: List[(String, TypeInfo.TypeBounds)],
      parameters: List[List[(String, TypeInfo)]],
      implicits: List[List[(String, TypeInfo)]],
      returnType: TypeInfo
    ): MethodInfo =
      val name          = symbol.name
      val returnTypeSig = symbol.signature.resultSig
      val typeSymbol    = typeRef.typeSymbol
      val owner         = normalizeClassName(symbol.maybeOwner.fullName)
      val annotations   = resolveAnnotations(quotes)(symbol.annotations)
      val baseClasses   = typeRef.baseClasses.map(clazz => resolveForClass(quotes)(clazz, typeRef.memberType(clazz)))
      val userDefined   = !isLangSymbol(quotes)(symbol.maybeOwner)
      val isImplicit    = symbol.flags.is(Flags.Implicit)
      val isGiven       = symbol.flags.is(Flags.Given)
      val isFinal       = symbol.flags.is(Flags.Final)
      val isInline      = symbol.flags.is(Flags.Inline)
      val isTransparent = symbol.flags.is(Flags.Transparent)
      val isPrivate     = symbol.flags.is(Flags.Private)
      val isProtected   = symbol.flags.is(Flags.Protected)
      val accessor      = if isPrivate then Accessor.Private else if isProtected then Accessor.Protected else Accessor.Public

      MethodInfo(
        name,
        typeParams,
        parameters,
        implicits,
        returnTypeSig,
        returnType,
        annotations,
        baseClasses,
        owner,
        inherited,
        userDefined,
        isImplicit,
        isGiven,
        isFinal,
        isInline,
        isTransparent,
        accessor,
        parentClassPath
      )

    typeRef match
      case method @ MethodType(_, _, _)                                  =>
        val TypeInfo.Method(typeParams, parameters, implicits, returnType) = resolveMethodTypeInfo(quotes)(method)
        methodInfo(typeParams, parameters, implicits, returnType)
      case poly @ PolyType(_, _, _)                                      =>
        val TypeInfo.Method(typeParams, parameters, implicits, returnType) = resolveMethodTypeInfo(quotes)(poly)
        methodInfo(typeParams, parameters, implicits, returnType)
      case applied @ AppliedType(_, _)                                   =>
        methodInfo(Nil, Nil, Nil, resolveAppliedType(quotes)(applied))
      case ref @ TypeRef(_, _)                                           =>
        methodInfo(Nil, Nil, Nil, TypeInfo.Simple(ref.name, ref.typeSymbol.fullName))
      case ByNameType(ref)                                               =>
        resolveForMethod(quotes)(symbol, ref, parentClassPath, inherited)
      case AnnotatedType(ref, _)                                         =>
        resolveForMethod(quotes)(symbol, ref, parentClassPath, inherited)
      case _                                                             =>
        methodInfo(Nil, Nil, Nil, TypeInfo.Unknown)

  private def resolveMethodTypeInfo(quotes: Quotes)(typeRef: quotes.reflect.MethodType | quotes.reflect.PolyType): TypeInfo.Method =
    import quotes.reflect.*

    @tailrec def resolveMethodParameters(
      typeParams: List[(String, TypeInfo.TypeBounds)],
      methodRef: quotes.reflect.MethodType,
      params: List[List[(String, TypeInfo)]],
      implicits: List[List[(String, TypeInfo)]]
    ): TypeInfo.Method =
      methodRef match
        case outer @ MethodType(names, types, inner @MethodType(_, _, _)) =>
          val parameters = names.zip(types).map((name, tpeRef) => name -> resolveTypeInfo(quotes)(tpeRef))

          if (outer.isImplicit)
          then resolveMethodParameters(typeParams, inner, params, implicits :+ parameters)
          else resolveMethodParameters(typeParams, inner, params :+ parameters, implicits)

        case method @ MethodType(names, types, ref)                       =>
          val parameters = names.zip(types).map((name, tpeRef) => name -> resolveTypeInfo(quotes)(tpeRef))
          val returnType = resolveTypeInfo(quotes)(ref)

          if (method.isImplicit)
          then TypeInfo.Method(typeParams, params, implicits :+ parameters, returnType)
          else TypeInfo.Method(typeParams, params :+ parameters, implicits, returnType)

    typeRef match
      case method @ MethodType(_, _, _)                                  =>
        resolveMethodParameters(Nil, method, Nil, Nil)
      case PolyType(typeNames, typeBounds, method @ MethodType(_, _, _)) =>
        val typeParams = typeNames.zip(typeBounds).map { case (name, TypeBounds(ref0, ref1)) => name -> resolveTypeBounds(quotes)(ref0, ref1) }
        resolveMethodParameters(typeParams, method, Nil, Nil)
      case PolyType(typeNames, typeBounds, ref)                          =>
        val typeParams = typeNames.zip(typeBounds).map { case (name, TypeBounds(ref0, ref1)) => name -> resolveTypeBounds(quotes)(ref0, ref1) }
        val returnType = resolveTypeInfo(quotes)(ref)
        TypeInfo.Method(typeParams, Nil, Nil, returnType)

  private def resolveTypeInfo(quotes: Quotes)(typeRef: quotes.reflect.TypeRepr): TypeInfo =
    import quotes.reflect.*

    typeRef match
      case ref @ TypeRef(_, _)                =>
        TypeInfo.Simple(ref.name, ref.typeSymbol.fullName)
      case applied @ AppliedType(_, _)        =>
        resolveAppliedType(quotes)(applied)
      case ParamRef(PolyType(names, _, _), _) =>
        TypeInfo.Reference(names)
      case method @ MethodType(_, _, _)       =>
        resolveMethodTypeInfo(quotes)(method)
      case poly @ PolyType(_, _, _)           =>
        resolveMethodTypeInfo(quotes)(poly)
      case OrType(ref0, ref1)                 =>
        resolveOrType(quotes)(ref0, ref1)
      case AndType(ref0, ref1)                =>
        resolveAndType(quotes)(ref0, ref1)
      case TypeBounds(ref0, ref1)             =>
        resolveTypeBounds(quotes)(ref0, ref1)
      case AnnotatedType(ref, _)              =>
        resolveTypeInfo(quotes)(ref)
      case ByNameType(ref)                    =>
        TypeInfo.ByName(resolveTypeInfo(quotes)(ref))
      case ref @ TermRef(_, _)                =>
        TypeInfo.Simple(ref.name, normalizeClassName(ref.typeSymbol.fullName))
      case ThisType(ref)                      =>
        resolveTypeInfo(quotes)(ref)
      case ref @ Refinement(_, _, _)          =>
        resolveRefinementType(quotes)(ref)
      case RecursiveType(ref)                 =>
        resolveTypeInfo(quotes)(ref)
      case RecursiveThis(ref)                 =>
        resolveTypeInfo(quotes)(ref)
      case ConstantType(constant)             =>
        val constantValue =
          if (constant.value.isInstanceOf[PrimitiveType])
          then ConstantValue.Primitive(constant.value.asInstanceOf[PrimitiveType])
          else ConstantValue.Other(constant.show)
        TypeInfo.Constant(constantValue)
      case term                               =>
        term.dealias match
          case ParamRef(PolyType(names, _, _), _) =>
            TypeInfo.Reference(names)
          case _                                  =>
            TypeInfo.Unknown

  private def resolveTypeInfoWithFallback(quotes: Quotes)(
    typeRef: quotes.reflect.TypeRepr,
    fallback: quotes.reflect.TypeRepr
  ): TypeInfo =
    val typeInfo = resolveTypeInfo(quotes)(typeRef)
    if typeInfo == TypeInfo.Unknown then resolveTypeInfo(quotes)(fallback) else typeInfo

  private def resolveAppliedType(quotes: Quotes)(applied: quotes.reflect.AppliedType): TypeInfo.Applied =
    val typeConstructorRef    = applied.tycon
    val typeConstructorSymbol = typeConstructorRef.typeSymbol
    val typeConstructorName   = typeConstructorSymbol.name
    val typeConstructoPath    = typeConstructorSymbol.fullName
    val argsInfo              = applied.args.map(resolveTypeInfo(quotes))
    val simpleTypeInfo        = TypeInfo.Simple(typeConstructorName, typeConstructoPath)
    val typeInfo              = if (typeConstructoPath == "scala.<repeated>") then TypeInfo.Repeated else simpleTypeInfo
    TypeInfo.Applied(typeInfo, argsInfo)

  private def resolveAndType(quotes: Quotes)(
    typeRef0: quotes.reflect.TypeRepr,
    typeRef1: quotes.reflect.TypeRepr
  ): TypeInfo.Intersection =
    TypeInfo.Intersection(resolveTypeInfo(quotes)(typeRef0), resolveTypeInfo(quotes)(typeRef1))

  private def resolveOrType(quotes: Quotes)(
    typeRef0: quotes.reflect.TypeRepr,
    typeRef1: quotes.reflect.TypeRepr
  ): TypeInfo.Union =
    TypeInfo.Union(resolveTypeInfo(quotes)(typeRef0), resolveTypeInfo(quotes)(typeRef1))

  private def resolveTypeBounds(quotes: Quotes)(
    typeRef0: quotes.reflect.TypeRepr,
    typeRef1: quotes.reflect.TypeRepr
  ): TypeInfo.TypeBounds =
    TypeInfo.TypeBounds(resolveTypeInfo(quotes)(typeRef0), resolveTypeInfo(quotes)(typeRef1))

  private def resolveType(quotes: Quotes)(
    symbol: quotes.reflect.Symbol,
    typeRef: quotes.reflect.TypeRepr,
    fallback: quotes.reflect.TypeRepr,
    inherited: Boolean
  ): TypeFieldInfo =
    import quotes.reflect.Flags

    val name        = symbol.name
    val typeSymbol  = typeRef.typeSymbol
    val typeInfo    = resolveTypeInfoWithFallback(quotes)(typeRef, fallback)
    val owner       = normalizeClassName(symbol.maybeOwner.fullName)
    val annotations = resolveAnnotations(quotes)(symbol.annotations)
    val userDefined = !isLangSymbol(quotes)(symbol.maybeOwner)
    val isPrivate   = symbol.flags.is(Flags.Private)
    val isProtected = symbol.flags.is(Flags.Protected)
    val accessor    = if isPrivate then Accessor.Private else if isProtected then Accessor.Protected else Accessor.Public
    TypeFieldInfo(name, typeInfo, annotations, owner, inherited, userDefined, accessor)

  private def resolveRefinementType(quotes: Quotes)(ref: quotes.reflect.Refinement): TypeInfo.Refinement =
    import quotes.reflect.*

    @tailrec def resolveRefinedTypes(
      refinement: quotes.reflect.Refinement,
      refined: List[(String, TypeInfo)]
    ): TypeInfo.Refinement =
      refinement match
        case Refinement(inner @ Refinement(_, _, _), name, ref) =>
          resolveRefinedTypes(inner, refined.prepended(name -> resolveTypeInfo(quotes)(ref)))

        case Refinement(baseRef, name, ref)                     =>
          TypeInfo.Refinement(resolveTypeInfo(quotes)(baseRef), refined.prepended(name -> resolveTypeInfo(quotes)(ref)))

    resolveRefinedTypes(ref, Nil)

  private inline def resolveAnnotations(quotes: Quotes)(terms: List[quotes.reflect.Term]): List[Annotation] =
    import quotes.reflect.*

    terms.collect:
      case term @ Apply(Select(New(ident @TypeIdent(name) ), n), params) =>
        val fields = ident.symbol.declaredFields.map(_.name)
        val values = annotationParams(quotes)(params)

        Annotation(
          name = name,
          fullName = normalizeClassName(term.symbol.signature.resultSig),
          properties = fields.zip(values)
        )

  private inline def annotationParams(quotes: Quotes)(terms: List[quotes.reflect.Term]): List[PrimitiveType] =
    import quotes.reflect.*

    terms.collect:
      case NamedArg(_, Literal(BooleanConstant(argValue))) => argValue
      case NamedArg(_, Literal(ByteConstant(argValue)))    => argValue
      case NamedArg(_, Literal(ShortConstant(argValue)))   => argValue
      case NamedArg(_, Literal(CharConstant(argValue)))    => argValue
      case NamedArg(_, Literal(IntConstant(argValue)))     => argValue
      case NamedArg(_, Literal(LongConstant(argValue)))    => argValue
      case NamedArg(_, Literal(FloatConstant(argValue)))   => argValue
      case NamedArg(_, Literal(DoubleConstant(argValue)))  => argValue
      case NamedArg(_, Literal(StringConstant(argValue)))  => argValue
      case Literal(BooleanConstant(argValue))              => argValue
      case Literal(ByteConstant(argValue))                 => argValue
      case Literal(ShortConstant(argValue))                => argValue
      case Literal(CharConstant(argValue))                 => argValue
      case Literal(IntConstant(argValue))                  => argValue
      case Literal(LongConstant(argValue))                 => argValue
      case Literal(FloatConstant(argValue))                => argValue
      case Literal(DoubleConstant(argValue))               => argValue
      case Literal(StringConstant(argValue))               => argValue

  private[meta] inline def findClass(className: String): Class[?] =
    try Class.forName(className)
    catch
      case _: ClassNotFoundException =>
        Class.forName(denormalizeClassName(className))

  private inline def isLangSymbol(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    symbol.fullName.startsWith("scala.") || symbol.fullName.startsWith("java.")

  private inline def normalizeClassName(name: String): String =
    name.stripSuffix("$")

  private[meta] inline def denormalizeClassName(name: String): String =
    val denormalised = name.replace("\\", "$bslash").replace("?", "$qmark")
    val lastDotIndex = denormalised.lastIndexOf(".")
    denormalised.updated(lastDotIndex, '$')