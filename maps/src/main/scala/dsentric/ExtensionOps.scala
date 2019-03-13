package dsentric

final class StringOps(val self:String) extends AnyVal {
  def \(part:String):Path =
    Path(self, part)
  def \(part:Int):Path =
    Path(self, part)
  def :=[T](t:T)(implicit jCodec: DCodec[T]):(String, Data) =
    self -> Data(t)(jCodec)
  def p:Path = Path(self)
}

final class IntOps(val self:Int) extends AnyVal {
  def \(part:String):Path =
    Path(self, part)
  def \(part:Int):Path =
    Path(self, part)
}

final class FunctionOps[D <: DObject](val f:D => D) extends AnyVal with LensCompositor[D]

trait ToExtensionOps {

  implicit def toStringOps(s: String): StringOps =
    new StringOps(s)

  implicit def toIntOps(i: Int): IntOps =
    new IntOps(i)

  implicit def toFunctionOps[D <: DObject](f: D => D): FunctionOps[D] =
    new FunctionOps[D](f)
}