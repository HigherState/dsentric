package dsentric.argonaut

import argonaut._
import dsentric.{LensCompositor, MaybeSubContract, Strictness, ExpectedSubContract}
import monocle._
import monocle.function.{Each, Empty}

object Dsentric extends
  dsentric.AndMatcher {

  implicit val jsString =
    Prism[Json, String](_.string)(Argonaut.jString)
  implicit val jsBool =
    Prism[Json, Boolean](_.bool)(Argonaut.jBool)
  implicit val jsInt =
    Prism[Json, Int](_.number.collect {
      case JsonLong(l) if l >= Int.MinValue && l <= Int.MaxValue => l.toInt
    })(Argonaut.jNumber)
  implicit val jsLong =
    Prism[Json, Long](_.number.collect {
      case JsonLong(l) => l.toInt
    })(Argonaut.jNumber)
  implicit val jsFloat =
    Prism[Json, Float](_.number.collect {
      case JsonDouble(d) if d >= Float.MinValue && d <= Float.MaxValue => d.toFloat
    })(n => Argonaut.jNumber(n.toDouble))
  implicit val jsDouble =
    Prism[Json, Double](_.number.collect {
      case JsonDouble(d) => d
    })(Argonaut.jNumber(_).get)

  implicit val jsArray =
    Prism[Json, List[Json]](_.array)(Argonaut.jArray)

  implicit val jsObject =
    Prism[Json, JsonObject](_.obj)(Argonaut.jObject)

  implicit val jsonObjectAt = new function.At[JsonObject, String, Option[Json]] {
    def at(i: String) = Lens{m: JsonObject => m(i)}(optV => map => optV.fold(map - i)(v => map.+:(i -> v)))
  }

  implicit val jsEach = new Each[JsonObject, (String, Json)] {
    import scalaz._
    import Scalaz._
    def each: Traversal[JsonObject, (String, Json)] = new PTraversal[JsonObject, JsonObject, (String, Json), (String, Json)] {
      def modifyF[F[_]](f: ((String, Json)) => F[(String, Json)])(s: JsonObject)(implicit evidence$1: Applicative[F]): F[JsonObject] = {
        val m = s.toMap.map(f)
        val ss = evidence$1.sequence(m.toList)
        ss.map(i => JsonObject.from(i))
      }
    }
  }

  implicit val jsObjectEmpty = new Empty[JsonObject] {
    override val empty: Prism[JsonObject, Unit] =
      Prism[JsonObject, Unit](j => if (j.isEmpty) Some(()) else None)(_ => JsonObject.empty)
  }

  object \ extends dsentric.ExpectedDsl[Json, JsonObject]

  object \! extends dsentric.DefaultDsl[Json, JsonObject]

  object \? extends dsentric.MaybeDsl[Json, JsonObject]

  abstract class Contract extends dsentric.Contract[Json, JsonObject]

  abstract class \\(private val name:Option[String]) extends ExpectedSubContract[Json, JsonObject](name) {
    def this() = this(None)
    def this(name:String) = this(Some(name))
  }

  abstract class \\?(private val name:Option[String])(implicit strictness:Strictness) extends MaybeSubContract[Json, JsonObject](name) {
    def this()(implicit strictness:Strictness) = this(None)
    def this(name:String)(implicit strictness:Strictness) = this(Some(name))
  }

  implicit class JCompositor(val f:Json => Json) extends AnyVal with LensCompositor[Json]

  implicit class MaybeDeltaDelete[T](val maybeProperty:dsentric.Maybe[Json, JsonObject, T]) extends AnyVal with dsentric.MaybeDeltaDelete[Json, JsonObject, T] {
    protected def deleteValue: Json = Argonaut.jNull
  }

  implicit class DefaultDeltaDelete[T](val defaultProperty:dsentric.Default[Json, JsonObject, T]) extends AnyVal with dsentric.DefaultDeltaDelete[Json, JsonObject, T] {
    protected def deleteValue: Json = Argonaut.jNull
  }

  object MaybeNull extends Strictness {
    override def apply[Data, T](value: Data, prism: Prism[Data, T]): Option[Option[T]] =
      if (value == Argonaut.jNull) Some(None)
      else dsentric.MaybePessimistic(value, prism)
  }
}
