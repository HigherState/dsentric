package dsentric.avro

import reflect.runtime.universe._
import dsentric._

case class AvroField(name:String, _type:Either[String, AvroRecord], notNullable:Boolean, additional:DObject)

case class AvroRecord(name:String, fields:Vector[AvroField])

case class Avro(typeName:String = "", fieldName:String = "") extends scala.annotation.StaticAnnotation


object AvroSchema {
  import dsentric.Dsentric._
  import dsentric.PessimisticCodecs._

  def processContract[D <: DObject](contract:BaseContract[D]):DObject =
    generateRecord(readContract(contract, None))

  //TODO Support type extraction like GraphQL, necessary for recursive types as well
  def readContract[D <: DObject](contract:BaseContract[D], typeOverride:Option[String]):AvroRecord = {
    val fieldList =
      contract._fields.map {
        case (name, b:BaseContract[D]@unchecked with Property[D,_]@unchecked) =>
          AvroField(name, Right(readContract(b, None)), b.isInstanceOf[Expected[_, _]], getAdditional(b))

        case (name, p) =>
          AvroField(name, Left(p._codec.schemaName), p.isInstanceOf[Expected[_, _]], p._codec.schemaAdditional ++ getAdditional(p))
      }
    AvroRecord(typeOverride.getOrElse(getContractName(contract)), fieldList)
  }

  def generateRecord(avroRecord:AvroRecord):DObject = {
    DObject(
      "type" := "record",
      "name" := avroRecord.name,
      "fields" := avroRecord.fields.map(generateField))
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

  def generateField(avroField:AvroField):DObject = {
    val _type = avroField._type match {
      case Left(typ) =>
        Data(typ)
      case Right(record) =>
        generateRecord(record)
    }
    DObject(
      "name" := avroField.name,
      "type" -> (if (avroField.notNullable) _type else DArray(Dsentric.dNull, _type))) ++ avroField.additional
  }

  def getContractName[D <: DObject](contract:BaseContract[D]):String = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    getAnnotatedName(t.annotations)
      .orElse(getAnnotatedName(t.baseClasses.flatMap(_.annotations)))
      .getOrElse(getClassName(contract, t))
  }

  def getClassName[D <: DObject](contract:BaseContract[D], t:ClassSymbol):String = {
    if (contract.isInstanceOf[ContractFor[_]])
      t.name.encodedName.toString
    else {
      val baseclass = t.baseClasses.find(c => c != t && !c.fullName.startsWith("dsentric."))
      baseclass.map(_.name.encodedName.toString)
        .getOrElse("UnknownType")
    }
  }

  def getAnnotationOverrides[D <: DObject](contract:BaseContract[D]):Map[String, String] = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(contract.getClass.getClassLoader)
    val t = mirror.classSymbol(contract.getClass)
    val members = t.toType.members
    val fMembers = members.filter{m => m.annotations.exists(_.tree.tpe == typeOf[Avro])}
    members.flatMap(m => getAnnotatedName(m.annotations).map(m.name.toString.trim -> _)).toMap
  }

  def getAnnotatedName(annotations:List[Annotation]):Option[String] =
    annotations.find(_.tree.tpe == typeOf[Avro])
      .flatMap(_.tree.children.tail.collectFirst { case Literal(Constant(c: String)) if c != "" => c })
}
