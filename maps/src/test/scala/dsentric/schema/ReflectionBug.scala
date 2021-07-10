package dsentric.schema

import com.github.ghik.silencer.silent

@silent
object ReflectionBug {

  @native
  val outer: Int = 5

  val nested = new {
    @native
    val field: Int = 4
  }

  def main(args: Array[String]) = {
    val mirror           = scala.reflect.runtime.universe.runtimeMirror(ReflectionBug.getClass.getClassLoader)
    val t                = mirror.classSymbol(ReflectionBug.getClass)
    val members          = t.toType.members
    val annotatedMembers = members.filter(_.annotations.nonEmpty)
    //Outer is incuded with annotation native

    val nested1 = members.drop(1).head
    val nested2 = members.drop(2).head

    val subs = members.filter(_.typeSignature.typeSymbol.isType).map(s => s -> s.typeSignature.members)
    //'field' members under nested member has no annotations

    val mirror2    = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)
    val t2         = mirror2.reflect(ReflectionBug.nested)
    val members2   = t2.symbol.toType.members
    val annotates2 = members2.filter(_.annotations.nonEmpty)
    //annotates2 does not contain field
  }
}
