package dsentric.contracts

import dsentric.codecs.DStringCodec
import dsentric.schema.StringDefinition

case class Length4String(value:String)

object Length4String{
  implicit val fixedLength4StringCodec: DStringCodec[Length4String] =
    new DStringCodec[Length4String] {
      override def apply (t: Length4String): String = t.value

      def fromString (s: String): Option[Length4String] =
      if (s.length == 4) Some (Length4String (s) )
      else None

      def typeDefinition: StringDefinition = ???
    }
}