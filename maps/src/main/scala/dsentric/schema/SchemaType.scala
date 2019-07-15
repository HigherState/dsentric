package dsentric.schema

import scala.util.matching.Regex

sealed trait TypeDefinition {
  def name:String
  def enum:List[Any]
}


sealed trait SingleTypeDefinition extends TypeDefinition {
  def isEmpty:Boolean
}

case class IntegerDefinition(enum:List[Any] = Nil,
                            minimum:Option[Long] = None,
                            exclusiveMinimum:Option[Long] = None,
                            maximum:Option[Long] = None,
                            exclusiveMaximum:Option[Long] = None,
                            multipleOf:Option[Long] = None
                           ) extends SingleTypeDefinition {
  def name:String = "integer"

  def isEmpty: Boolean = this == IntegerDefinition.empty
}

object IntegerDefinition {
  val empty:IntegerDefinition = IntegerDefinition()
}

case class NumberDefinition(enum:List[Any] = Nil,
                            minimum:Option[Double] = None,
                            exclusiveMinimum:Option[Double] = None,
                            maximum:Option[Double] = None,
                            exclusiveMaximum:Option[Double] = None,
                            multipleOf:Option[Double] = None
                           ) extends SingleTypeDefinition {
  def name:String = "number"

  def isEmpty: Boolean = this == NumberDefinition.empty
}

object NumberDefinition {
  val empty:NumberDefinition = NumberDefinition()
}

case class StringDefinition(enum:List[Any] = Nil,
                            format:Option[String] = None,
                            pattern:Option[String] = None,
                            minLength:Option[Int] = None,
                            maxLength:Option[Int] = None,
                            contentEncoding:Option[String] = None,
                            contentMediaType:Option[String] = None
                           ) extends SingleTypeDefinition {
  def name:String = "string"

  def isEmpty: Boolean = this == StringDefinition.empty
}

object StringDefinition {
  val empty:StringDefinition = StringDefinition()
}

case object BooleanDefinition extends SingleTypeDefinition {
  def name:String = "boolean"

  def enum: List[Any] = Nil

  def isEmpty: Boolean = true
}

case class ArrayDefinition(
                          items:Vector[TypeDefinition] = Vector.empty,
                          minLength:Option[Int] = None,
                          maxLength:Option[Int] = None,
                          uniqueness:Boolean = false
                          ) extends SingleTypeDefinition {
  def name:String = "array"

  def enum: List[Any] = Nil

  def isEmpty: Boolean = this == ArrayDefinition.empty
}
object ArrayDefinition {
  val empty:ArrayDefinition = ArrayDefinition()

}

case object NullDefinition extends SingleTypeDefinition {
  def name:String = "null"

  def enum: List[Any] = Nil

  def isEmpty: Boolean = true
}

case class ByRefDefinition(name:String) extends SingleTypeDefinition {
  def enum: List[Any] = Nil

  def isEmpty: Boolean = false
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
                           ) extends SingleTypeDefinition {

  def name:String = "object"
  def enum: List[Any] = Nil

  def isEmpty: Boolean = this == ObjectDefinition.empty
}

object ObjectDefinition {
  val empty = ObjectDefinition()
}

case class MultipleTypeDefinition(typeDefinitions:Vector[SingleTypeDefinition]) extends TypeDefinition {
  def name:String = typeDefinitions.map(_.name).mkString(",")
  def enum: List[Any] = Nil

  def remap(f:PartialFunction[TypeDefinition,TypeDefinition]):MultipleTypeDefinition = {
    val singleTypeDefinitions =
      typeDefinitions.map(d => f.lift(d).getOrElse(d)).foldLeft(Vector.empty[SingleTypeDefinition]) {
        case (v, e: SingleTypeDefinition) if !v.contains(e) =>
          v :+ e
        case (v, e: MultipleTypeDefinition) =>
          v ++ e.typeDefinitions.filter(v.contains)
        case (v, _) =>
          v
      }
    MultipleTypeDefinition(singleTypeDefinitions)
  }

  def withRemap(objs:Vector[ObjectDefinition])
               (f:PartialFunction[(TypeDefinition, Vector[ObjectDefinition]), (TypeDefinition, Vector[ObjectDefinition])]
               ):(MultipleTypeDefinition, Vector[ObjectDefinition]) = {
    val (ts, defs3) = typeDefinitions.foldLeft(Vector.empty[SingleTypeDefinition] -> objs) {
      case ((v, defs0), e) =>
        f.lift(e -> defs0).fold((v :+ e) -> defs0){
          case (e:SingleTypeDefinition, defs1) =>
            (v :+ e) -> defs1
          case (e:MultipleTypeDefinition, defs1) =>
            (v ++ e.typeDefinitions.filter(v.contains)) -> defs1
        }
    }
    MultipleTypeDefinition(ts) -> defs3
  }
}

object MultipleTypeDefinition {
  def apply(singleTypeDefinition: SingleTypeDefinition*):MultipleTypeDefinition =
    MultipleTypeDefinition(singleTypeDefinition.toVector)
}


object TypeDefinition {
  def nullable:Function[TypeDefinition, TypeDefinition] = {
    case m@MultipleTypeDefinition(td) =>
      if (td.contains(NullDefinition)) m
      else MultipleTypeDefinition(td :+ NullDefinition)
    case t:SingleTypeDefinition =>
      MultipleTypeDefinition(t, NullDefinition)
  }

  val anyDefinition =
    MultipleTypeDefinition(StringDefinition(), IntegerDefinition(), NumberDefinition(), BooleanDefinition, ArrayDefinition(), ObjectDefinition())

  val anyVal:TypeDefinition =
    MultipleTypeDefinition(StringDefinition(), IntegerDefinition(), NumberDefinition(), BooleanDefinition)
}