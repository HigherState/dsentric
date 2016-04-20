package dsentric.std

import dsentric._
import monocle._
import monocle.function.Each

trait CommonPrisms {

  implicit val anyString =
    Prism[Any, String]{ case v:String => Some(v); case _ => None}(s => s)

  implicit val anyList:Prism[Any, List[Any]] =
    Prism[Any, List[Any]]{
      case v:List[Any] => Some(v)
      case _ => None
    }(l => l)

  implicit val anyMap:Prism[Any, Map[String, Any]] =
    Prism[Any, Map[String, Any]]{
      case m:Map[String,Any]@unchecked => Some(m)
      case _ => None
    }(m => m)

  implicit val mapAt = monocle.std.map.atMap[String, Any]

  implicit val mapEmpty = monocle.std.map.mapEmpty[String, Any]

  implicit val mapEach = new Each[Map[String, Any], (String, Any)] {
    import scalaz._
    import Scalaz._
    def each: Traversal[Map[String, Any], (String, Any)] = new PTraversal[Map[String, Any], Map[String, Any], (String, Any), (String, Any)] {
      def modifyF[F[_]](f: ((String, Any)) => F[(String, Any)])(s: Map[String, Any])(implicit evidence$1: Applicative[F]): F[Map[String, Any]] = {
        val m = s.map(f)
        val ss = evidence$1.sequence(m.toList)
        ss.map(_.toMap)
      }
    }
  }
}

//Types must match, (expected number primitives dont have to be exact type, just value)
trait PessimisticPrisms extends CommonPrisms {

  implicit val anyBool =
    Prism[Any, Boolean]{
    case v:Boolean => Some(v)
    case _ => None}(a => a)
  implicit val anyByte =
    Prism[Any, Byte](NumericPartialFunctions.byte.lift)(a => a)
  implicit val anyShort =
    Prism[Any, Short](NumericPartialFunctions.short.lift)(a => a)
  implicit val anyInt =
    Prism[Any, Int](NumericPartialFunctions.int.lift)(a => a)
  implicit val anyLong =
    Prism[Any, Long](NumericPartialFunctions.long.lift)(a => a)
  implicit val anyFloat =
    Prism[Any, Float](NumericPartialFunctions.float.lift)(a => a)
  implicit val anyDouble =
    Prism[Any, Double](NumericPartialFunctions.double.lift)(a => a)
}

trait OptimisticPrisms extends CommonPrisms {
  implicit val anyBool =
    Prism[Any, Boolean]{
    case v:Boolean => Some(v)
    case 1 => Some(true)
    case 0 => Some(false)
    case "true" | "True" | "TRUE" => Some(true)
    case "false" | "False" | "FALSE" => Some(false)
    case _ => None}(a => a)
  implicit val anyByte =
    Prism[Any, Byte](NumericPartialFunctions.byte.orElse(NumericPartialFunctions.stringDouble.andThen(NumericPartialFunctions.byte)).lift)(a => a)
  implicit val anyShort =
    Prism[Any, Short](NumericPartialFunctions.short.orElse(NumericPartialFunctions.stringDouble.andThen(NumericPartialFunctions.short)).lift)(a => a)
  implicit val anyInt =
    Prism[Any, Int](NumericPartialFunctions.int.orElse(NumericPartialFunctions.stringDouble.andThen(NumericPartialFunctions.int)).lift)(a => a)
  implicit val anyLong =
    Prism[Any, Long](NumericPartialFunctions.long.orElse(NumericPartialFunctions.stringDouble.andThen(NumericPartialFunctions.long)).lift)(a => a)
  implicit val anyFloat =
    Prism[Any, Float](NumericPartialFunctions.float.orElse(NumericPartialFunctions.stringDouble.andThen(NumericPartialFunctions.float)).lift)(a => a)
  implicit val anyDouble =
    Prism[Any, Double](NumericPartialFunctions.double.orElse(NumericPartialFunctions.stringDouble).lift)(a => a)
}


object Dsentric extends
  dsentric.AndMatcher with
  PessimisticPrisms {

  object \ extends dsentric.ExpectedDsl[Any, Map[String, Any]]

  object \! extends dsentric.DefaultDsl[Any, Map[String, Any]]

  object \? extends dsentric.MaybeDsl[Any, Map[String, Any]]

  abstract class Contract extends dsentric.Contract[Any, Map[String, Any]]

  abstract class \\(private val name:Option[String]) extends ExpectedSubContract[Any, Map[String, Any]](name) {
    def this() = this(None)
    def this(name:String) = this(Some(name))
  }

  abstract class \\?(private val name:Option[String])(implicit strictness:Strictness) extends MaybeSubContract[Any, Map[String, Any]](name){
    def this()(implicit strictness:Strictness) = this(None)
    def this(name:String)(implicit strictness:Strictness) = this(Some(name))
  }

  implicit class JCompositor(val f:Any => Any) extends AnyVal with LensCompositor[Any]

  implicit class MaybeDeltaDelete[T](val maybeProperty:dsentric.Maybe[Any, Map[String, Any], T]) extends AnyVal with dsentric.MaybeDeltaDelete[Any, Map[String, Any], T] {
    protected def deleteValue: Any = null
  }

  implicit class DefaultDeltaDelete[T](val defaultProperty:dsentric.Default[Any, Map[String, Any], T]) extends AnyVal with dsentric.DefaultDeltaDelete[Any, Map[String, Any], T] {
    protected def deleteValue: Any = null
  }

  object MaybeNull extends Strictness {
    override def apply[Data, T](value: Data, prism: Prism[Data, T]): Option[Option[T]] =
      if (value == null) Some(None)
      else MaybePessimistic(value, prism)
  }
}
