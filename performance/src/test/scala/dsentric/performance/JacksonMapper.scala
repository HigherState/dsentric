package dsentric.performance

import com.fasterxml.jackson.core.{JsonFactory, JsonToken, JsonParser}
import com.fasterxml.jackson.databind.deser.Deserializers
import com.fasterxml.jackson.databind.deser.std.{UntypedObjectDeserializer => JacksonUntypedObjectDeserializer}
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.module.scala._

import scala.collection.immutable
import scala.collection.immutable.VectorBuilder


private object NestedTypeObjectDeserializer extends JacksonUntypedObjectDeserializer(null, null) {

  //Code taken from jackson
  override def mapArray(jp: JsonParser, ctxt: DeserializationContext): Vector[Any] = {
    if (jp.nextToken()  == JsonToken.END_ARRAY) {
      return Vector.empty[Any]
    }
    val value = deserialize(jp, ctxt)
    if (jp.nextToken() == JsonToken.END_ARRAY)
      Vector(value)
    else {
      val value2 = deserialize(jp, ctxt)
      if (jp.nextToken() == JsonToken.END_ARRAY)
        Vector(value, value2)
      else {
        val buffer = new VectorBuilder[Any] // no leasing
        buffer += value
        buffer += value2
        buffer += deserialize(jp, ctxt)
        while(jp.nextToken() != JsonToken.END_ARRAY) {
          buffer += deserialize(jp, ctxt)
        }
        buffer.result()
      }
    }
  }

  override def mapObject(jp: JsonParser, ctxt: DeserializationContext): Map[String, Any] = {
    var key1:String = null

    val t = jp.getCurrentToken

    if (t == JsonToken.START_OBJECT) {
      key1 = jp.nextFieldName()
    } else if (t == JsonToken.FIELD_NAME) {
      key1 = jp.getCurrentName
    } else {
      if (t != JsonToken.END_OBJECT) {
        throw ctxt.mappingException(handledType(), jp.getCurrentToken)
      }
      key1 = null
    }
    if (key1 == null)
      Map.empty
    else {
      jp.nextToken()
      val value1 = deserialize(jp, ctxt)
      val key2 = jp.nextFieldName()
      if (key2 == null)
        Map(key1 -> value1)
      else {
        jp.nextToken()
        val value2 = deserialize(jp, ctxt)
        var key = jp.nextFieldName()
        if (key == null)
          Map(key1 -> value1, key2 -> value2)
        else {
          val builder = immutable.Map.newBuilder[String, Any]
          builder += (key1 -> value1)
          builder += (key2 -> value2)
          while(key != null) {
            jp.nextToken()
            builder += (key -> deserialize(jp, ctxt))
            key = jp.nextFieldName()
          }
          builder.result()
        }
      }
    }
  }

}

//private object NestedTypeObjectDeserializer extends JacksonUntypedObjectDeserializer(null, null) {
//  import scala.collection.JavaConverters._
//  override def mapArray(jp: JsonParser, ctxt: DeserializationContext): AnyRef =
//    super.mapArray(jp, ctxt).asInstanceOf[java.util.ArrayList[AnyRef]].asScala.toList
//
//  override def mapObject(jp: JsonParser, ctxt: DeserializationContext): AnyRef =
//    super.mapObject(jp, ctxt).asInstanceOf[java.util.LinkedHashMap[String, AnyRef]].asScala.toMap
//
//  override def deserialize(jp: JsonParser, ctxt: DeserializationContext): AnyRef =
//    super.deserialize(jp, ctxt) match {
//      //case JDate(d) => d
//      case o => o
//    }
//}


private object NestedTypeObjectDeserializerResolver extends Deserializers.Base {

  lazy val OBJECT = classOf[AnyRef]

  override def findBeanDeserializer(javaType: JavaType,
                                    config: DeserializationConfig,
                                    beanDesc: BeanDescription) =
    if (!OBJECT.equals(javaType.getRawClass)) null
    else NestedTypeObjectDeserializer
}

trait NestedTypeObjectDeserializerModule extends JacksonModule {
  this += (_ addDeserializers NestedTypeObjectDeserializerResolver)
}

object JacksonMapper extends ObjectMapper() {
  private val module = new OptionModule
    with MapModule
    with SeqModule
    with IteratorModule
    with NestedTypeObjectDeserializerModule

  this.registerModule(module)

}

object TokenReader {
  val f = new JsonFactory()

  def apply(s:String) = {
    val jp = f.createParser(s)
    jp.nextToken()
    deserialize(jp)
  }

  private def deserialize(jp:JsonParser) =
    jp.getCurrentToken match {
      case JsonToken.VALUE_STRING => jp.getText
      case JsonToken.VALUE_NUMBER_INT => jp.getLongValue
      case JsonToken.VALUE_NUMBER_FLOAT => jp.getDoubleValue
      case JsonToken.VALUE_NULL => null
      case JsonToken.VALUE_TRUE => true
      case JsonToken.VALUE_FALSE => false
      case JsonToken.START_OBJECT => mapObject(jp)
      case JsonToken.START_ARRAY => mapArray(jp)
      case _ => ???
    }

  def mapArray(jp: JsonParser): Vector[Any] = {
    if (jp.nextToken()  == JsonToken.END_ARRAY) {
      return Vector.empty[Any]
    }
    val value = deserialize(jp)
    if (jp.nextToken() == JsonToken.END_ARRAY)
      Vector(value)
    else {
      val value2 = deserialize(jp)
      if (jp.nextToken()  == JsonToken.END_ARRAY) {
        Vector(value, value2)
      }
      else {
        val buffer = new VectorBuilder[Any] // no leasing
        buffer += value
        buffer += value2
        buffer += deserialize(jp)
        while(jp.nextToken() != JsonToken.END_ARRAY) {
          buffer += deserialize(jp)
        }
        buffer.result()
      }
    }
  }

  def mapObject(jp: JsonParser): Map[String, Any] = {
    var key1:String = null

    val t = jp.getCurrentToken

    if (t == JsonToken.START_OBJECT) {
      key1 = jp.nextFieldName()
    } else if (t == JsonToken.FIELD_NAME) {
      key1 = jp.getCurrentName
    } else {
      if (t != JsonToken.END_OBJECT) {
        throw new Exception("parse failed")
      }
      key1 = null
    }
    if (key1 == null)
      Map.empty
    else {
      jp.nextToken()
      val value1 = deserialize(jp)
      val key2 = jp.nextFieldName()
      if (key2 == null)
        Map(key1 -> value1)
      else {
        jp.nextToken()
        val value2 = deserialize(jp)
        var key = jp.nextFieldName()
        if (key == null)
          Map(key1 -> value1, key2 -> value2)
        else {
          val builder = immutable.Map.newBuilder[String, Any]
          builder += (key1 -> value1)
          builder += (key2 -> value2)
          while(key != null) {
            jp.nextToken()
            builder += (key -> deserialize(jp))
            key = jp.nextFieldName()
          }
          builder.result()
        }
      }
    }
  }
}