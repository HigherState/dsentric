package dsentric.contracts

import dsentric.{DNullable, SimpleRenderer}
import dsentric.Dsentric.Contract
import dsentric.codecs.DStringCodec
import dsentric.schema.{Definition, JsonSchema, StringDefinition}

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


  import dsentric.codecs.std.DCodecs._

  object ExpectedStructure extends Contract {
    val copy          = \[String]
    val maybeCopied   = \?[String]
    val defaultCopied = \![String]("default")
    val nulled        = \[DNullable[Int]]
    val field         = \[String]
    val expected      = new \\ with Open{
      val field   = \[String]
      val default = \![String]("default")
    }
    val maybe         = new \\? {
      val field   = \[String]
      val default = \![String]("default")
    }
  }

  def main(args:Array[String]):Unit ={
    println(JsonSchema.convertObjectDefinition(Definition.nestedContractObjectDefinition(ExpectedStructure)).render(SimpleRenderer))
  }
}