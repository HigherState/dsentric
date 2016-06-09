package dsentric

object Dsentric extends DataMatchers with AndMatcher with PropertyOps with ToPathOps with ToExtensionOps {

  implicit def strictness:Strictness = MaybePessimistic

  val dNull:DNull = new DNull
}
