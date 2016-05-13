package dsentric

object Dsentric extends JsonMatchers with AndMatcher with PropertyOps {

  implicit def strictness:Strictness = MaybePessimistic
  implicit class functionExt(val f:JObject => JObject) extends AnyVal with LensCompositor[JObject]

  val jNull:JNull = new JNull {}

  implicit class PathExt(val self:Path) extends AnyVal {

    def \(part:String):Path = self ++ List[Either[Int, String]](Right(part))
    def \(part:Int):Path = self ++List[Either[Int, String]](Left(part))

    def hasSubPath(path:Path) =
      path.zip(self).foldLeft(true) {
        case (a, (s, p)) =>  a && s == p
      }
  }

  implicit class StringExt(val self:String) extends AnyVal {
    def \(part:String):Path =
      List(Right(self), Right(part))
    def \(part:Int):Path =
      List(Right(self), Left(part))
    def :=[T](t:T)(implicit jCodec: JCodec[T]):(String, Json) =
      self -> Json(t)(jCodec)
  }

  implicit class IntExt(val self:Int) extends AnyVal {
    def \(part:String):Path =
      List(Left(self), Right(part))
    def \(part:Int):Path =
      List(Left(self), Left(part))
  }
}
