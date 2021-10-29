package dsentric.macros

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox._

/**
 *  Generates an implicit def to build a DCodec for an annotated value class based
 *  off an implicit DCodec for the underlying type. This cannot be used in conjunction with
 *  another macro annotation - see https://stackoverflow.com/questions/52982928/combining-scala-macro-annotations?noredirect=1&lq=1 -
 *  unless a companion object already exists for the value class being annotated. A work around to this problem is to manually
 *  add an empty companion object to your value class.
 */
class DCodify extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DCodifyMacro.impl
}

object DCodifyMacro {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def mkDCodecImplicit(className: TypeName, termName: TermName, termType: Tree) =
      if (termType.toString() == "String")
        q"""implicit def ${TermName("dCodec" + className.toTermName.toString)}: dsentric.codecs.DStringCodec[${className}] =
         new dsentric.codecs.DStringCodec[$className] {
           def apply(t:$className):String = t.${termName}
           def fromString(s:String):Option[$className] = Some(new ${className.toTypeName}(s))
           def typeDefinition:dsentric.schema.StringDefinition = dsentric.schema.StringDefinition.empty
         }
       """
      else
        q"""implicit def ${TermName("dCodec" + className.toTermName.toString)}(implicit D: dsentric.codecs.DCodec[$termType]): dsentric.codecs.DCodec[${className}] =
         dsentric.codecs.DValueClassCodec[${className}, $termType](t => t.${termName},  v => Some(new ${className.toTypeName}(v)))(D)
       """

    def modifiedCompanion(maybeCompDecl: Option[ModuleDef], dCodecImplicit: Tree, className: TypeName) =
      maybeCompDecl.fold(q"object ${className.toTermName} { $dCodecImplicit }") { compDecl =>
        val q"$mods object $obj extends ..$bases { ..$body }" = compDecl
        q"""
          $mods object $obj extends ..$bases {
            ..$body
            $dCodecImplicit
          }
        """
      }

    def modifiedDecl(classDecl: ClassDef, name: TypeName, value: ValDef, maybeCompDecl: Option[ModuleDef] = None) = {
      val dcodecImplicit = mkDCodecImplicit(name, value.name, value.tpt)
      val companion      = modifiedCompanion(maybeCompDecl, dcodecImplicit, name)

      c.Expr(q"""
        $classDecl
        $companion
      """)
    }

    def isValueClass(template: Template): Boolean =
      template.collect { case x: Ident => x }.headOption.fold(false)(n => n.name.toString == "AnyVal")

    def abort: Nothing                            = c.abort(c.enclosingPosition, "Invalid: Can not annotate structure with @DCodify")

    annottees.map(_.tree).toList match {
      case ClassDef(_, _, tParams, _) :: _ if !tParams.isEmpty                                                    => abort
      case (classDecl @ ClassDef(_, name, _, template)) :: Nil if isValueClass(template)                          =>
        template.body.collectFirst { case x: ValDef => x } match {
          case Some(value) => modifiedDecl(classDecl, name, value)
          case _           => abort
        }
      case (classDecl @ ClassDef(_, name, _, template)) :: (compDecl: ModuleDef) :: Nil if isValueClass(template) =>
        template.body.collectFirst { case x: ValDef => x } match {
          case Some(value) => modifiedDecl(classDecl, name, value, Some(compDecl))
          case _           => abort
        }
      case _                                                                                                      =>
        abort
    }
  }
}
