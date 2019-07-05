package dsentric.schema

import scala.util.matching.Regex

trait TypeDefinition {
  def name:String
}

case class IntegerDefinition(enum:List[Long] = Nil,
                            minimum:Option[Long] = None,
                            exclusiveMinimum:Option[Long] = None,
                            maximum:Option[Long] = None,
                            exclusiveMaximum:Option[Long] = None,
                            multipleOf:Option[Long] = None
                           ) extends TypeDefinition {
  def name:String = "integer"
}

case class NumberDefinition(enum:List[Double] = Nil,
                            minimum:Option[Double] = None,
                            exclusiveMinimum:Option[Double] = None,
                            maximum:Option[Double] = None,
                            exclusiveMaximum:Option[Double] = None,
                            multipleOf:Option[Double] = None
                           ) extends TypeDefinition {
  def name:String = "number"
}

object NumberDefinition {
  val empty = NumberDefinition()
}

case class StringDefinition(format:Option[String] = None,
                            pattern:Option[String] = None,
                            minLength:Option[Int] = None,
                            maxLength:Option[Int] = None,
                            enum:List[String] = Nil) extends TypeDefinition {
  def name:String = "string"
}

object StringDefinition {
  val empty = StringDefinition()
}

case object BooleanDefinition extends TypeDefinition {
  def name:String = "boolean"
}

case class ArrayDefinition(
                          items:Option[TypeDefinition] = None,
                          minLength:Option[Int] = None,
                          maxLength:Option[Int] = None,
                          uniqueness:Boolean = false
                          ) extends TypeDefinition {
  def name:String = "array"
}
object ArrayDefinition {
  val empty = ArrayDefinition()
}

case object NullDefinition extends TypeDefinition {
  def name:String = "null"
}

case class ByRefDefinition(name:String) extends TypeDefinition

case class PropertyDefinition(key:String,
                              typeDefinition:TypeDefinition,
                              examples:List[Any],
                              default:Option[Any],
                              required:Boolean,
                              description:Option[String])

case class ObjectDefinition(
                             definition:Option[String] = None,
                             title:Option[String] = None,
                             description:Option[String] = None,
                             referencedDefinitions:Vector[String] = Vector.empty,
                             properties:Vector[PropertyDefinition] = Vector.empty,
                             additionalProperties:Boolean = true,
                             propertyNames:Option[StringDefinition] = None,
                             minProperties:Option[Int] = None,
                             maxProperties:Option[Int] = None,
                             patternProperties:Map[Regex, TypeDefinition] = Map.empty
                           ) extends TypeDefinition {

def name:String = "object"
}

object ObjectDefinition {
  val empty = ObjectDefinition()
}

case class MultipleTypeDefinition(typeDefinitions:TypeDefinition*) extends TypeDefinition {
  def name:String = typeDefinitions.map(_.name).mkString(",")
}

case object AnyDefinition extends TypeDefinition {
  val name:String = "string,integer,number,boolean,object,array"
}

object TypeDefinition {
  def nullable(typeDefinition: TypeDefinition):TypeDefinition =
    MultipleTypeDefinition(typeDefinition, NullDefinition)

  val anyVal:TypeDefinition =
    MultipleTypeDefinition(StringDefinition(), NumberDefinition(), BooleanDefinition)
}