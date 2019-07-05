package dsentric.schema

import dsentric._


case class MsonField(
                      name:String,
                      _type:Option[String],
                      children:Vector[MsonField],
                      examples:List[Any],
                      description:Option[String],
                      required:Boolean,
                      additional:DObject)

case class MsonRecord(name:Option[String], example:Option[String], description:Option[String], fields:Vector[MsonField])



object MsonSchema {
  import dsentric.Dsentric._
  import dsentric.PessimisticCodecs._

//  def processContract[D <: DObject](contract:BaseContract[D], newLine:String, indent:String):String =
//    generateRecord(readContract(contract, None), newLine, indent, 0)  + ("$schema" := "https://avro.apache.org/docs/1.8.1/spec.html#Schema")

//  def generateRecord(msonRecord:MsonRecord, newLine:String, indent:String, indentCount:Int):String = {
//    s"# ${msonRecord.name}$newLine$newLine" +
//    msonRecord.fields.map(generateField(_, newLine, indent, indentCount)).mkString(newLine)
//  }

//  def generateField(msonField:MsonField, newLine:String, indent:String, indentCount:Int):String = {
//    msonField._type match {
//      case Left(_type) =>
//        s"${0.until(indentCount).map(_ => indent).mkString}+ ${escape(msonField.name)} (${_type})"
//      case Right(record) =>
//        s"${0.until(indentCount).map(_ => indent).mkString}+ ${escape(msonField.name)} (${record.name})$newLine" +
//          record.fields.map(generateField(_, newLine, indent, indentCount + 1)).mkString(newLine)
//    }
//  }



  val unsafeChars = Seq(':', '(',')', '<', '>', '{', '}', '[', ']', '_', '*', '-', '+', '`', '\'')
  private def escape(name:String):String =
    if (name.intersect(unsafeChars).nonEmpty) "'" + name.replace("'","''") + "'"
    else name
}
