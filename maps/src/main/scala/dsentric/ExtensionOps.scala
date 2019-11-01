package dsentric

import dsentric.contracts.PathSetter

final class StringOps(val self:String) extends AnyVal {
  def \(part:String):Path =
    Path(self, part)
  def \(part:Int):Path =
    Path(self, part)

  def :=[T](t:T)(implicit D: DCodec[T]):(String, Data) =
    self -> D(t)

  def ::=(pairs:(String, Data)*):(String, Data) =
    self -> DObject(pairs:_*)

  def p:Path = Path(self)
}

final class IntOps(val self:Int) extends AnyVal {
  def \(part:String):Path =
    Path(self, part)
  def \(part:Int):Path =
    Path(self, part)
}

final class PathOps(val self:Path) extends AnyVal {
  def :=[T](v:T)(implicit D:DCodec[T]):(Path, Data) =
    self -> D(v)

}

final class FunctionOps[D <: DObjectLike[D] with DObject](val f:D => D) extends AnyVal with LensCompositor[D] {
  def ~+(kv:(String, Data)):D => D =
    f andThen (_ + kv)

  def ~++(kv:Seq[(String, Data)]):D => D =
    f andThen (_ ++ kv)
}

trait ToExtensionOps {

  implicit def toStringOps(s: String): StringOps =
    new StringOps(s)

  implicit def toIntOps(i: Int): IntOps =
    new IntOps(i)

  implicit def toFunctionOps[D <: DObjectLike[D] with DObject](f: D => D): FunctionOps[D] =
    new FunctionOps[D](f)

  implicit def pathOps(p:Path): PathOps =
    new PathOps(p)

  def ~+[D <: DObjectLike[D] with DObject](kv:(String, Data)):D => D =
    _ + kv

  def ~++[D <: DObjectLike[D] with DObject](kv:Seq[(String, Data)]):D => D =
    _ ++ kv
}