package dsentric.performance

import java.util.UUID

import com.fasterxml.jackson.core.{JsonFactory, JsonToken, JsonParser, JsonGenerator}
import com.fasterxml.jackson.databind.deser.Deserializers
import com.fasterxml.jackson.databind.deser.std.{UntypedObjectDeserializer => JacksonUntypedObjectDeserializer}
import com.fasterxml.jackson.databind.ser.Serializers
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
      if (jp.nextToken()  == JsonToken.END_ARRAY) {
        Vector(value, value2)
      }
      else {
        val buffer = new VectorBuilder[Any] // no leasing
        buffer += value
        buffer += value2
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

  private def deserialize(jp:JsonParser) = {
    val nt = jp.getCurrentToken
    println(nt)
    valMap(nt)(jp)
  }

  private val valMap = Map[JsonToken, JsonParser => Any](
    JsonToken.VALUE_STRING -> (p => p.getText),
    JsonToken.VALUE_NUMBER_INT -> (p => p.getLongValue),
    JsonToken.VALUE_NUMBER_FLOAT -> (p => p.getDoubleValue),
    JsonToken.VALUE_NULL -> (p => null),
    JsonToken.VALUE_TRUE -> (p => true),
    JsonToken.VALUE_FALSE -> (P => false),
    JsonToken.START_OBJECT -> mapObject,
    JsonToken.START_ARRAY -> mapArray
  )


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