package dsentric.std

import dsentric.{ExpectedSubContract, LensCompositor, MaybeSubContract, Strictness}
import monocle._

object Dsentric extends
  dsentric.AndMatcher {

  implicit val anyString =
    Prism[Any, String]{ case v:String => Some(v); case _ => None}(_)
  implicit val anyBool =
    Prism[Any, Boolean]{ case v:Boolean => Some(v); case _ => None}(_)
  implicit val anyInt =
    Prism[Any, Int]{ case v:Int => Some(v); case _ => None}(_)
  implicit val anyLong =
    Prism[Any, Long]{ case v:Int => Some(v); case v:Long => Some(v); case _ => None}(_)
  implicit val anyFloat =
    Prism[Any, Float]{ case v:Float => Some(v); case _ => None}(_)
  implicit val anyDouble =
    Prism[Any, Double]{
      case v:Int => Some(v)
      case v:Long => Some(v)
      case v:Float => Some(v)
      case v:Double => Some(v)
      case _ => None
    }(_)

  implicit val anyList =
    Prism[Any, List[Any]]{
      case v:List[Any] => Some(v)
      case _ => None
    }(_)

  implicit val anyMap =
    Prism[Any, Map[String, Any]]{
      case m:Map[String,Any]@unchecked => Some(m)
      case _ => None
    }(_)

  implicit val mapAt = monocle.std.map.atMap[String, Any]

  implicit val mapEmpty = monocle.std.map.mapEmpty[String, Any]

  object \ extends dsentric.ExpectedDsl[Any, Map[String, Any]]

  object \! extends dsentric.DefaultDsl[Any, Map[String, Any]]

  object \? extends dsentric.MaybeDsl[Any, Map[String, Any]]

  abstract class Contract extends dsentric.Contract[Any, Map[String, Any]]

  abstract class \\(private val name:Option[String]) extends ExpectedSubContract[Any, Map[String, Any]](name) {
    def this() = this(None)
    def this(name:String) = this(Some(name))
  }

  abstract class \\?(private val name:Option[String])(implicit strictness:Strictness) extends MaybeSubContract[Any, Map[String, Any]](name) {
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
}
