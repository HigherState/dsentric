package dsentric

/**
  * Created by Jamie Pullar on 06/05/2016.
  */
trait Renderer {

  def print(value:Raw):String

  def bytes(value:Raw):Array[Byte]
}

object SimpleRenderer extends Renderer {

  def print(value:Raw):String = {
    val sb = new StringBuilder()
    jsonPrint(sb)(value)
    sb.result()
  }

  def bytes(value:Raw): Array[Byte] =
    print(value).getBytes("UTF-8")

  private[dsentric] def jsonPrint(sb:StringBuilder):Function[Any, Unit] = {
    case s:String =>
      sb ++= "\"" ++= s.replace("\"", "\\\"") ++= "\""
      ()
    case n:Number =>
      sb ++= n.toString
      ()
    case true =>
      sb ++= "true"
      ()
    case false =>
      sb ++= "false"
      ()
    case DNull =>
      sb ++= "null"
      ()
    case v:RawArray@unchecked =>
      sb += '['
      v match {
        case h +: tail =>
          jsonPrint(sb)(h)
          tail.foreach{t =>
            sb += ','
            jsonPrint(sb)(t)
          }
        case _ =>
      }
      sb += ']'
      ()
    case m:RawObject@unchecked if m.isEmpty =>
      sb ++= "{}"
      ()
    case m:RawObject@unchecked =>
      sb += '{'
      m.headOption.foreach{p =>
        sb ++= "\"" ++= p._1.replace("\"", "\\\"") ++= "\""
        sb += ':'
        jsonPrint(sb)(p._2)
      }
      m.tail.foreach{p =>
        sb += ','
        sb ++= "\"" ++= p._1.replace("\"", "\\\"") ++= "\""
        sb += ':'
        jsonPrint(sb)(p._2)
      }
      sb += '}'
      ()
    case v =>
      sb ++= v.toString
      ()
  }
}
