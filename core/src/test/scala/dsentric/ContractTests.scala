package dsentric

import monocle.{POptional, Optional, function, Prism}
import org.scalatest.{Matchers, FunSuite}

import scalaz.{Applicative, \/}

class ContractTests extends FunSuite with Matchers {
  import Dsentric._

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

  implicit val strictness = MaybePessimistic

  object Test extends Contract[Json, JsObject] {
    val one = new Expected[Json, JsObject, String](Validator.empty[Json], None)
    val two = new Maybe[Json, JsObject, Boolean](Validator.empty[Json], None)
    val three = new Default[Json, JsObject, Int](3, Validator.empty[Json], None)
  }

  test("Contract pattern matching") {


    (JsObject(Map("one" -> JsString("string"), "two" -> JsBool(false))) match {
      case Test.one(s) && Test.two(Some(v)) =>
        s -> v
    }) should equal("string" -> false)

    (JsObject(Map("one" -> JsString("string"))) match {
      case Test.one(s) && Test.two(None) && Test.three(int) =>
        true
    }) should equal(true)

    (JsObject(Map("one" -> JsNumber(123))) match {
      case Test.one(s) && Test.two(None) => s
      case _ => "wrong type"
    }) should equal("wrong type")

    (JsObject(Map("two" -> JsBool(false))) match {
      case Test.one(s) && Test.two(None) => s
      case Test.two(Some(false)) => "two match"
    }) should equal("two match")

    (JsObject(Map("three" -> JsNumber(4))) match {
      case Test.three(i) => i
    }) should equal(4)

    (JsObject(Map("three" -> JsString("4"))) match {
      case Test.three(i) => true
      case _ => false
    }) should equal(false)

    (JsObject(Map("two" -> JsString("String"))) match {
      case Test.two(None) => true
      case _ => false
    }) should equal(false)

    (JsObject(Map("two" -> JsNull)) match {
      case Test.two(None) => true
      case _ => false
    }) should be(true)

    (JsObject(Map("two" -> JsString("false"))) match {
      case Test.two(Some(i)) => true
      case _ => false
    }) should equal(false)

    (JsObject(Map("three" -> JsString("not a number"))) match {
      case Test.three(i) => i
      case _ => "wrong type"
    }) should equal("wrong type")

    (JsObject(Map.empty) match {
      case Test.two(None) => true
      case _ => false
    }) should be(true)
  }
}
