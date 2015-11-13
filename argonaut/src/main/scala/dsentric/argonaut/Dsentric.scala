package dsentric.argonaut

import argonaut.{Json, JsonObject}
import monocle._

import scalaz.{-\/, \/-, Applicative, \/}

object Dsentric extends
  dsentric.AndMatcher

object Index {
  implicit val index =  new function.Index[JsonObject, String, Json] {
    def index(i: String): Optional[JsonObject, Json] = new POptional[JsonObject, JsonObject, Json, Json] {

      def getOrModify(s: JsonObject): \/[JsonObject, Json] =
        s(i).fold[\/[JsonObject, Json]](-\/(s))(\/-(_))

      def modify(f: (Json) => Json): (JsonObject) => JsonObject =
        j => j(i).fold(j)(v => j + (i, v))

      def set(b: Json): (JsonObject) => JsonObject =
        j => j + (i, b)

      def getOption(s: JsonObject): Option[Json] =
        s(i)

      def modifyF[F[_]](f: (Json) => F[Json])(s: JsonObject)(implicit evidence$1: Applicative[F]): F[JsonObject] = ???
    }
  }

  implicit val indexedPrism:Prism[Json, JsonObject] = Json.jObjectPrism
}
