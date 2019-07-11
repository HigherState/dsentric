package dsentric.schema

import scala.util.matching.Regex

trait TypeDefinition {
  def name:String
  def enum:List[Any]
}

case class IntegerDefinition(enum:List[Any] = Nil,
                            minimum:Option[Long] = None,
                            exclusiveMinimum:Option[Long] = None,
                            maximum:Option[Long] = None,
                            exclusiveMaximum:Option[Long] = None,
                            multipleOf:Option[Long] = None
                           ) extends TypeDefinition {
  def name:String = "integer"
}

case class NumberDefinition(enum:List[Any] = Nil,
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

case class StringDefinition(enum:List[Any] = Nil,
                            format:Option[String] = None,
                            pattern:Option[String] = None,
                            minLength:Option[Int] = None,
                            maxLength:Option[Int] = None,
                            contentEncoding:Option[String] = None,
                            contentMediaType:Option[String] = None
                           ) extends TypeDefinition {
  def name:String = "string"
}

object StringDefinition {
  val empty = StringDefinition()
}

case object BooleanDefinition extends TypeDefinition {
  def name:String = "boolean"

  def enum: List[Any] = Nil
}

case class ArrayDefinition(
                          items:Option[TypeDefinition] = None,
                          minLength:Option[Int] = None,
                          maxLength:Option[Int] = None,
                          uniqueness:Boolean = false
                          ) extends TypeDefinition {
  def name:String = "array"

  def enum: List[Any] = Nil
}
object ArrayDefinition {
  val empty = ArrayDefinition()

}

case object NullDefinition extends TypeDefinition {
  def name:String = "null"

  def enum: List[Any] = Nil
}

case class ByRefDefinition(name:String) extends TypeDefinition {
  def enum: List[Any] = Nil
}

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
                             additionalProperties:Boolean = true, // can be object
                             propertyNames:Option[StringDefinition] = None,
                             minProperties:Option[Int] = None,
                             maxProperties:Option[Int] = None,
                             patternProperties:Map[Regex, TypeDefinition] = Map.empty
                           ) extends TypeDefinition {

  def name:String = "object"
  def enum: List[Any] = Nil
}

object ObjectDefinition {
  val empty = ObjectDefinition()
}

case class MultipleTypeDefinition(typeDefinitions:TypeDefinition*) extends TypeDefinition {
  def name:String = typeDefinitions.map(_.name).mkString(",")
  def enum: List[Any] = Nil

  def remap(f:PartialFunction[TypeDefinition,TypeDefinition]):MultipleTypeDefinition =
    MultipleTypeDefinition(typeDefinitions.map(d => f.lift(d).getOrElse(d)):_*)

  def withRemap(
                 objs:Vector[ObjectDefinition]
               )
               (
                 f:PartialFunction[(TypeDefinition, Vector[ObjectDefinition]), (TypeDefinition, Vector[ObjectDefinition])]
               ):(MultipleTypeDefinition, Vector[ObjectDefinition]) = {
    val (ts, newObjs) = typeDefinitions.foldLeft(Vector.empty[TypeDefinition] -> objs) {
      case ((v, o), t) =>
        f.lift(t -> o).fold((v :+ t) -> o)(p => (v :+ p._1) -> p._2)
    }
    MultipleTypeDefinition(ts:_*) -> objs
  }
}

case object AnyDefinition extends TypeDefinition {
  val name:String = "string,integer,number,boolean,object,array"
  def enum: List[Any] = Nil
}

object TypeDefinition {
  def nullable(typeDefinition: TypeDefinition):TypeDefinition =
    MultipleTypeDefinition(typeDefinition, NullDefinition)

  val anyVal:TypeDefinition =
    MultipleTypeDefinition(StringDefinition(), NumberDefinition(), BooleanDefinition)
}