package dsentric.schema

import dsentric._


case class MsonField(name:String, _type:Either[String, MsonRecord], notNullable:Boolean, additional:DObject)

case class MsonRecord(name:String, fields:Vector[MsonField])


object MsonSchema {
  import dsentric.Dsentric._
  import dsentric.PessimisticCodecs._

  def processContract[D <: DObject](contract:BaseContract[D], newLine:String, indent:String):String =
    generateRecord(readContract(contract, None), newLine, indent, 0)  + ("$schema" := "https://avro.apache.org/docs/1.8.1/spec.html#Schema")

  //TODO Support type extraction like GraphQL, necessary for recursive types as well
  def readContract[D <: DObject](contract:BaseContract[D], typeOverride:Option[String]):MsonRecord = {
    val annotatedChildren = Schema.getAnnotationOverrides(contract)
    val fieldList =
      contract._fields.map {
        case (name, b:BaseContract[D]@unchecked with Property[D,_]@unchecked) =>
          MsonField(name, Right(readContract(b, None)), b.isInstanceOf[Expected[_, _]], getAdditional(b))

        case (name, p) =>
          MsonField(name, Left(p._codec.schemaName), p.isInstanceOf[Expected[_, _]], p._codec.schemaAdditional ++ getAdditional(p))
      }
    MsonRecord(typeOverride.getOrElse(Schema.getContractName(contract)), fieldList)
  }

  def generateRecord(msonRecord:MsonRecord, newLine:String, indent:String, indentCount:Int):String = {
    s"# ${msonRecord.name}$newLine$newLine" +
    msonRecord.fields.map(generateField(_, newLine, indent, indentCount)).mkString(newLine)
  }

  def getAdditional[D <: DObject, T](property:Property[D, T]):DObject = {
    val default =
      property match {
        case d:Default[D, T] =>
          DObject("default" := ForceWrapper.data(d._default))
        case _ =>
          DObject.empty
      }
    default ++ property._pathValidator.schemaInfo
  }

  def generateField(msonField:MsonField, newLine:String, indent:String, indentCount:Int):String = {
    msonField._type match {
      case Left(_type) =>
        s"${0.until(indentCount).map(_ => indent).mkString}+ ${escape(msonField.name)} (${_type})"
      case Right(record) =>
        s"${0.until(indentCount).map(_ => indent).mkString}+ ${escape(msonField.name)} (${record.name})$newLine" +
          record.fields.map(generateField(_, newLine, indent, indentCount + 1)).mkString(newLine)
    }
  }



  val unsafeChars = Seq(':', '(',')', '<', '>', '{', '}', '[', ']', '_', '*', '-', '+', '`', '\'')
  private def escape(name:String):String =
    if (name.intersect(unsafeChars).nonEmpty) "'" + name.replace("'","''") + "'"
    else name
}
