package dsentric.macros

import dsentric.codecs.{DCodec, DStringCodec}
import shapeless.test.illTyped
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import dsentric.codecs.std.DCodecs._

class DCodecMacroTests extends AnyFlatSpec with Matchers {


  "DCodify" should "create implicit dcodec for a value case class if dcodec exists for value" in {
    val dcodec = implicitly[DCodec[Foo]]
    dcodec.isInstanceOf[DStringCodec[Foo]] shouldBe true
    dcodec(Foo("bob")) shouldBe stringCodec("bob")
  }

  "DCodify" should "create implicit dcodec for a value case class if dcodec exists for value and companion object exists" in {
    val dcodec = implicitly[DCodec[Bar]]

    dcodec(Bar(1)) shouldBe intCodec(1)
  }

  it should "not compile when the class doesn't extend AnyVal" in {
    illTyped("@DCodify case class Bar(baz: String)", ".*Invalid: Can not annotate structure with @DCodify.*")
    illTyped("@DCodify class Bar(val baz: String)", ".*Invalid: Can not annotate structure with @DCodify.*")
  }

  "DCodify" should "create implicit dcodec for a value class" in {
    val dcodec = implicitly[DCodec[Hello]]

    dcodec(new Hello(Vector("hi there"))) shouldBe vectorCodec[String].apply(Vector("hi there"))

  }

  it should "not compile when the class has type parameters" in {
    illTyped(
      "@DCodify case class Bar[A](baz: String) extends AnyVal",
      ".*Invalid: Can not annotate structure with @DCodify.*"
    )
  }
}

@DCodify final case class Foo(bob: String) extends AnyVal
@DCodify final case class Bar(baz: Int)    extends AnyVal

object Bar {
  def foo: String = "abcdefghijklmnop"
}

@DCodify class Hello(val hi:Vector[String]) extends AnyVal