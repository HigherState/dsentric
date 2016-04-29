package dsentric

object Dsentric {

  implicit def strictness:Strictness = MaybePessimistic
  implicit class functionExt(val f:JObject => JObject) extends AnyVal with LensCompositor[JObject]

  implicit class stringExt(val self:String) extends AnyVal {
    def :=[T](t:T)(implicit jCodec: JCodec[T]):JPair =
      JPair(self, jCodec(t))
  }

  val jNull:JNull = new JNull {}
}
