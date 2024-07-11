package dsentric.codecs

import dsentric.codecs.{DCodec, DStringCodec}
import dsentric.codecs.std.DCodecs.*

import org.scalatest.{Outcome, Succeeded}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.exceptions.TestFailedException

class DCodecMacroTests extends AnyFlatSpec with Matchers {
  import scala.compiletime.testing.*

  inline def illTyped(inline code: String, inline errorMsg: String): Outcome =
    val errors = typeCheckErrors(code)

    if (errors.exists(_.message.contains(errorMsg))) then
      Succeeded
    else if errors.isEmpty then
      throw new TestFailedException(s"Code did compile", 0)
    else
      throw new TestFailedException(s"Code failed for different reasons: ${errors.map(_.message)}", 0)

  "derives DCodec" should "create implicit dcodec for a value case class if dcodec exists for value" in {
    val dcodec = implicitly[DCodec[Foo]]
    dcodec.isInstanceOf[DStringCodec[Foo]] shouldBe true
    dcodec(Foo("bob")) shouldBe stringCodec("bob")
    dcodec.unapply(dcodec(Foo("bob"))).isDefined shouldBe true
  }

  "derives DCodec" should "create implicit dcodec for a value case class if dcodec exists for value and companion object exists" in {
    val dcodec = implicitly[DCodec[Bar]]

    dcodec(Bar(1)) shouldBe intCodec(1)
    dcodec.unapply(dcodec(Bar(1))).isDefined shouldBe true
  }

  it should "not compile when the class doesn't extend AnyVal" in {
    illTyped("case class Bar(baz: String) derives DCodec", "must be a value class")
    illTyped("class Bar(val baz: String) derives DCodec", "must be a value class")
  }

  "derives DCodec" should "create implicit dcodec for a value class" in {
    val dcodec = implicitly[DCodec[Hello]]

    dcodec(new Hello(Vector("hi there"))) shouldBe vectorCodec[String].apply(Vector("hi there"))
    dcodec.unapply(dcodec(new Hello(Vector("hi there")))).isDefined shouldBe true
  }

  it should "not compile when the class has type parameters" in {
    illTyped(
      "case class Bar[A](baz: String) extends AnyVal derives DCodec",
      "cannot contain type parameters"
    )
  }
}

final case class Foo(bob: String) extends AnyVal derives DCodec
final case class Bar(baz: Int)    extends AnyVal derives DCodec
class Hello(val hi:Vector[String]) extends AnyVal derives DCodec