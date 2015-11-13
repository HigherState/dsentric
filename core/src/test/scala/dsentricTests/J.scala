package dsentricTests

import dsentric.ExpectedSubContract
import monocle._

import scalaz.{Applicative, \/}


object J {

  sealed trait Json

  case class JsString(s: String) extends Json
  case class JsNumber(n: Int) extends Json
  case class JsArray(l: List[Json]) extends Json
  case class JsObject(m: Map[String, Json]) extends Json
  case class JsBool(b:Boolean) extends Json
  case object JsNull extends Json

  implicit val jsString = Prism[Json, String]{ case JsString(s) => Some(s); case _ => None}(JsString.apply)
  implicit val jsBool = Prism[Json, Boolean]{ case JsBool(b) => Some(b); case _ => None}(JsBool.apply)
  implicit val jsNumber = Prism[Json, Int]{ case JsNumber(n) => Some(n); case _ => None}(JsNumber.apply)
  implicit val jsArray  = Prism[Json, List[Json]]{ case JsArray(a) => Some(a); case _ => None}(JsArray.apply)
  implicit val jsObject = Prism[Json, Map[String, Json]]{ case JsObject(m) => Some(m); case _ => None}(JsObject.apply)
  implicit val jsO:Prism[Json, JsObject] = Prism[Json, JsObject]{ case j:JsObject => Some(j); case _ => None}(j => j)
  implicit val jsIndex = new function.Index[JsObject, String, Json] {
    override def index(i: String): Optional[JsObject, Json] = new POptional[JsObject, JsObject, Json, Json] {
      override def getOrModify(s: JsObject): \/[JsObject, Json] = ???
      override def modify(f: (Json) => Json): (JsObject) => JsObject = ???
      override def set(b: Json): (JsObject) => JsObject = ???
      override def getOption(s: JsObject): Option[Json] = s.m.get(i)
      override def modifyF[F[_]](f: (Json) => F[Json])(s: JsObject)(implicit evidence$1: Applicative[F]): F[JsObject] = ???
    }
  }

  object \ extends dsentric.ExpectedDsl[Json, JsObject]

  object \! extends dsentric.DefaultDsl[Json, JsObject]

  object \? extends dsentric.MaybeDsl[Json, JsObject]

  abstract class Contract extends dsentric.Contract[Json, JsObject]

  abstract class \\(private val name:Option[String]) extends ExpectedSubContract[Json, JsObject](name) {
    def this() = this(None)
    def this(name:String) = this(Some(name))
  }


}


