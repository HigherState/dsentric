package dsentric.performance

import org.scalatest.{Matchers, FunSuite}

/**
  * Created by Jamie Pullar on 18/05/2016.
  */
class JsonDeserializationTest  extends FunSuite with Matchers  {

  test("jackson deserialization") {

    val init = JacksonMapper.readValue(JsonStringExample.text, classOf[Vector[Any]])
    val t = System.currentTimeMillis()
    var c = 0
    for (_ <- 1 to 10000) {
      c += JacksonMapper.readValue(JsonStringExample.text, classOf[Vector[Any]]).size
    }
    println("Jack " + c + " " + (System.currentTimeMillis() - t))
  }

  test("parser deserialization") {

    val init = TokenReader(JsonStringExample.text).asInstanceOf[Vector[Any]]
    val t = System.currentTimeMillis()
    var c = 0
    for (_ <- 1 to 10000) {
      c += TokenReader(JsonStringExample.text).asInstanceOf[Vector[Any]].size
    }
    println("Jack " + c + " " + (System.currentTimeMillis() - t))
  }

  test ("argonaut deserialization") {
    val init = argonaut.Parse.parse(JsonStringExample.text)
    val t = System.currentTimeMillis()
    var c = 0
    for (_ <- 1 to 10000) {
      c += argonaut.Parse.parse(JsonStringExample.text).map(_.arrayOrEmpty.size).getOrElse(0)
    }
    println("Argo " + c + " " + (System.currentTimeMillis() - t))
  }
}
