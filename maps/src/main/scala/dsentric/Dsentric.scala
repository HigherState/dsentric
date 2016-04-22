package dsentric

object Dsentric {

  implicit def strictness:Strictness = MaybePessimistic
  implicit class functionExt(val f:JObject => JObject) extends AnyVal with LensCompositor[JObject]
}
