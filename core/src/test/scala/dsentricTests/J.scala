package dsentricTests

import dsentric.{Strictness, MaybeSubContract, ExpectedSubContract, LensCompositor}
import monocle._
import monocle.function.{Empty, At}

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
  implicit val jsInt = Prism[Json, Int]{ case JsNumber(n) => Some(n); case _ => None}(JsNumber.apply)
  implicit val jsArray  = Prism[Json, List[Json]]{ case JsArray(a) => Some(a); case _ => None}(JsArray.apply)
  implicit val jsObject = Prism[Json, Map[String, Json]]{ case JsObject(m) => Some(m); case _ => None}(JsObject.apply)
  implicit val jsO:Prism[Json, JsObject] = Prism[Json, JsObject]{ case j:JsObject => Some(j); case _ => None}(j => j)
  implicit val jsIndex = new function.Index[JsObject, String, Json] {
    def index(i: String): Optional[JsObject, Json] = new POptional[JsObject, JsObject, Json, Json] {
      def getOrModify(s: JsObject): scalaz.\/[JsObject, Json] =
        s.m.get(i).fold[scalaz.\/[JsObject, Json]](scalaz.-\/(s))(scalaz.\/-.apply)
      def modify(f: (Json) => Json): (JsObject) => JsObject =
        (j:JsObject) => {
          j.m.get(i).fold(j){v =>
            JsObject(j.m + (i -> f(v)))
          }
        }
      def set(b: Json): (JsObject) => JsObject =
        (j:JsObject) => JsObject(j.m + (i -> b))
      def getOption(s: JsObject): Option[Json] =
        s.m.get(i)
      def modifyF[F[_]](f: (Json) => F[Json])(s: JsObject)(implicit evidence$1: scalaz.Applicative[F]): F[JsObject] = ???
    }
  }

  implicit val jsAt = new At[JsObject, String, Json] {
    def at(i: String): Lens[JsObject, Option[Json]] =
      new PLens[JsObject, JsObject, Option[Json], Option[Json]]{
        def get(s: JsObject): Option[Json] =
          s.m.get(i)
        def modify(f: (Option[Json]) => Option[Json]): (JsObject) => JsObject =
          j => set(f(get(j)))(j)

        def set(b: Option[Json]): (JsObject) => JsObject =
          j => b.fold(JsObject(j.m - i)){v => JsObject(j.m + (i -> v))}

        def modifyF[F[_]](f: (Option[Json]) => F[Option[Json]])(s: JsObject)(implicit evidence$1: scalaz.Functor[F]): F[JsObject] =
          ???
      }
  }

  implicit val jsEmpty = new Empty[JsObject] {
    override val empty: Prism[JsObject, Unit] =
      Prism[JsObject, Unit](j => if (j.m.isEmpty) Some(()) else None)(_ => JsObject(Map.empty))
  }

  object \ extends dsentric.ExpectedDsl[Json, JsObject]

  object \! extends dsentric.DefaultDsl[Json, JsObject]

  object \? extends dsentric.MaybeDsl[Json, JsObject]

  abstract class Contract extends dsentric.Contract[Json, JsObject]

  abstract class \\(private val name:Option[String]) extends ExpectedSubContract[Json, JsObject](name) {
    def this() = this(None)
    def this(name:String) = this(Some(name))
  }

  abstract class \\?(private val name:Option[String])(implicit strictness:Strictness) extends MaybeSubContract[Json, JsObject](name) {
    def this()(implicit strictness:Strictness) = this(None)
    def this(name:String)(implicit strictness:Strictness) = this(Some(name))
  }

  implicit class JCompositor(val f:Json => Json) extends AnyVal with LensCompositor[Json]

  implicit class MaybeDeltaDelete[T](val maybeProperty:dsentric.Maybe[Json, JsObject, T]) extends AnyVal with dsentric.MaybeDeltaDelete[Json, JsObject, T] {
     protected def deleteValue: Json = JsNull
  }

  implicit class DefaultDeltaDelete[T](val defaultProperty:dsentric.Default[Json, JsObject, T]) extends AnyVal with dsentric.DefaultDeltaDelete[Json, JsObject, T] {
    protected def deleteValue: Json = JsNull
  }
}


