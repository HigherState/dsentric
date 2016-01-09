package dsentric

import monocle.Monocle._
import monocle._
import monocle.function.{Empty, At, Index}

private[dsentric] sealed trait Struct {

  def apply[R](f:this.type => R):R = f(this)

}

sealed trait Property[Data, IndexedData, T <: Any] extends Struct {
  private[dsentric] var _path:Optional[Data, Option[Data]] = null
  private[dsentric] def _prism: Prism[Data, T]
  private[dsentric] def _nameOverride:Option[String]
  private[dsentric] def _pathValidator:Validator[_]
  private[dsentric] def _isValidType(j:Data):Boolean
  private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures

  private[dsentric] def _getValue(data:Data) =
    _path.getOption(data).flatMap{
      case Some(v) => _prism.getOption(v)
      case None => None
    }
}

//prevents clash of local nested implicit requirements with those in scope
private[dsentric] sealed trait MapPrism[Data, IndexedData, T] {

  implicit private[dsentric] val __prism: Prism[Data, T]
  private[dsentric] def _prism: Prism[Data, T] =
    __prism
}

private[dsentric] sealed trait ContractBase[Data, IndexedData]
  extends Struct with MapPrism[Data, IndexedData, IndexedData]{
  private var __fields = Vector.empty[(String, Property[Data, IndexedData, Any])]

  implicit private[dsentric] val __at: At[IndexedData, String, Option[Data]]
  implicit private[dsentric] val __empty: Empty[IndexedData]

  private[dsentric] val _index: Index[IndexedData, String, Data] = Index.atIndex(_at)
  private[dsentric] def _at: At[IndexedData, String, Option[Data]] = __at
  private[dsentric] def _empty: Empty[IndexedData] = __empty
  private[dsentric] def _prism:Prism[Data, IndexedData]
  private[dsentric] def _pathPrism:POptional[Data, Data, IndexedData, IndexedData]
  private[dsentric] def _fields = __fields

  private[dsentric] def _setPropertyNames():Unit = {
    __fields = this.getClass.getMethods.foldLeft(__fields) { (f, m) =>
      if (classOf[Struct].isAssignableFrom(m.getReturnType) && m.getTypeParameters.isEmpty) {
        m.invoke(this) match {
          case prop: Property[Data, IndexedData, Any]@unchecked =>
            val name = prop._nameOverride.getOrElse(m.getName)
            prop._path = _pathPrism.composeLens(at[IndexedData, String, Option[Data]](name))
            prop match {
              case c: SubContract[Data, IndexedData]@unchecked =>
                c._pathPrism = _pathPrism.composeOptional(_index.index(name)).composeOptional(_prism.asOptional)
                c._setPropertyNames()
              case _ =>
            }
            f :+ name -> prop
          case _ =>
            f
        }
      }
      else f
    }
  }

  private[dsentric] def _validateFields(path:Path, value:Option[Data], currentState:Option[Data]) =
    value.flatMap(_prism.getOption).fold(Failures.empty){ indexedData =>
      val cs = currentState.flatMap(_prism.getOption)
      _fields.flatMap{kv =>
        val i = _index.index(kv._1)
        val v = i.getOption(indexedData)
        val c = cs.flatMap(i.getOption)
        kv._2._validate(path :+ Right(kv._1), v, c)
      }
    }

  lazy val $sanitize:Data => Data =
    __fields.foldLeft[Data => Data](Predef.identity[Data]) {
      case (f, (_, prop:Maybe[Data, IndexedData, _]@unchecked)) if prop._pathValidator.isInternal =>
        prop.$drop.compose(f)
      case (f, (_, prop:ContractBase[Data, IndexedData]@unchecked)) =>
        prop.$sanitize.compose(f)
      case (f, _) =>
        f
    }

  def $create(f:Data => Data):Data =
    f(_prism.reverseGet(_empty.empty.reverseGet(())))

  def $dynamic[T](field:String)(implicit prism:Prism[Data, T], strictness:Strictness) = {
    val prop = new Maybe[Data, IndexedData, T](Validator.empty, None)(prism, strictness)
    prop._path = _pathPrism.composeLens(at[IndexedData, String, Option[Data]](field))
    prop
  }
}

private[dsentric] sealed trait SubContract[Data, IndexedData] extends ContractBase[Data, IndexedData] {
  private[dsentric] var _pathPrism:Optional[Data, IndexedData] = null
}

abstract class ExpectedSubContract[Data, IndexedData]
  (private[dsentric] override val _pathValidator:Validator[IndexedData], private[dsentric] override val _nameOverride:Option[String])
  (implicit
    private[dsentric] override val __prism: Prism[Data, IndexedData],
    private[dsentric] override val __at:At[IndexedData, String, Option[Data]],
    private[dsentric] override val __empty:Empty[IndexedData]
  )
  extends
    Expected[Data, IndexedData, IndexedData](_pathValidator, _nameOverride) with
    SubContract[Data, IndexedData]
  {


  def this(name:Option[String])(implicit prism: Prism[Data, IndexedData], at:At[IndexedData, String, Option[Data]], empty:Empty[IndexedData]) =
    this(Validator.empty, name)

  override private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    super._validate(path, value, currentState) match {
      case Failures.empty =>
        _validateFields(path, value, currentState)
      case failures =>
        failures
    }
}

abstract class MaybeSubContract[Data, IndexedData]
  (private[dsentric] override val _pathValidator:Validator[Option[IndexedData]], private[dsentric] override val _nameOverride:Option[String])
  (implicit
    private[dsentric] override val __prism: Prism[Data, IndexedData],
    private[dsentric] override val __at:At[IndexedData, String, Option[Data]],
    private[dsentric] override val __empty:Empty[IndexedData],
    strictness:Strictness)
  extends Maybe[Data, IndexedData, IndexedData](_pathValidator, _nameOverride) with SubContract[Data, IndexedData]{

  def this(name:Option[String])(implicit prism: Prism[Data, IndexedData], at:At[IndexedData, String, Option[Data]], empty:Empty[IndexedData], strictness:Strictness) =
    this(Validator.empty, name)

  override private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    super._validate(path, value, currentState) match {
      case Failures.empty =>
        _validateFields(path, value, currentState)
      case failures =>
        failures
    }
}


abstract class Contract[Data, IndexedData]
  (implicit
    private[dsentric] val __prism:Prism[Data, IndexedData],
    private[dsentric] val __at:At[IndexedData, String, Option[Data]],
    private[dsentric] override val __empty:Empty[IndexedData]
  )
  extends ContractBase[Data, IndexedData] with App {

  private[dsentric] val _pathPrism = __prism.asOptional

  override def delayedInit(body: => Unit) = {
    body
    _setPropertyNames()
  }

  def $validate(value:Data, currentState:Option[Data] = None):Failures =
    __prism.getOption(value).fold(Failures(Path.empty -> ValidationText.UNEXPECTED_TYPE)){ temp =>
      _validateFields(Path.empty, Some(value), currentState)
    }
}

class Expected[Data, IndexedData, T]
  (private[dsentric] val _pathValidator:Validator[T], private[dsentric] val _nameOverride:Option[String])
  (implicit private[dsentric] val __prism: Prism[Data, T])
  extends Property[Data, IndexedData, T] with MapPrism[Data, IndexedData, T] with ExpectedLens[Data, T] {

  def unapply(j:Data):Option[T] =
    _getValue(j)

  private[dsentric] def _isValidType(j:Data) =
    __prism.getOption(j).isDefined

  private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    value -> currentState match {
      case (None, None) =>
        Failures(path -> ValidationText.EXPECTED_VALUE)
      case (Some(v), c) =>
        __prism.getOption(v).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(__prism.getOption))
        }
      case (None, c) =>
        _pathValidator(path, None, c.flatMap(__prism.getOption))
    }
}


class Maybe[Data, IndexedData, T] private[dsentric](private[dsentric] val _pathValidator:Validator[Option[T]], private[dsentric] val _nameOverride:Option[String])
                                (implicit private[dsentric] val __prism: Prism[Data, T], implicit private[dsentric] val __strictness:Strictness)
  extends Property[Data, IndexedData, T] with MapPrism[Data, IndexedData, T] with MaybeLens[Data, T] {

  private[dsentric] def _strictness = __strictness

  private[dsentric] def _isValidType(j:Data) =
    _strictness(j, __prism).isDefined
  def unapply(j:Data):Option[Option[T]] = {
    _strictness(j, _path, __prism)
  }

  private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    value -> currentState match {
      case (Some(v), c)  =>
        _strictness(v, __prism).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(_strictness(_, __prism)))
        }
      case (None, c) =>
        _pathValidator(path, None, c.flatMap(_strictness(_, __prism)))
    }
}

class Default[Data, IndexedData, T] private[dsentric](val _default:T, private[dsentric] val _pathValidator:Validator[Option[T]], private[dsentric] val _nameOverride:Option[String])
                                        (implicit private[dsentric] val __prism: Prism[Data, T], implicit private[dsentric] val __strictness:Strictness)
  extends Property[Data, IndexedData, T] with MapPrism[Data, IndexedData, T] with DefaultLens[Data, T] {

  private[dsentric] def _strictness = __strictness

  private[dsentric] def _isValidType(j:Data) =
    _strictness(j, __prism).isDefined

  def unapply(j:Data):Option[T] =
    _strictness(j, _path, __prism).map(_.getOrElse(_default))

  private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    value -> currentState match {
      case (Some(v), c) =>
        _strictness(v, __prism).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(_strictness(_, __prism)))
        }
      case (None, c) =>
        _pathValidator(path, Some(_default), c.flatMap(_strictness(_, __prism)))
    }
}

abstract class ValueContract[Data, IndexedData, T] private[dsentric](val _pathValidator: Validator[T] = Validator.empty)
                                                       (implicit private[dsentric] val __prism: Prism[Data, T])
  extends Property[Data, IndexedData, T] with MapPrism[Data, IndexedData, T]{

  private[dsentric] def _isValidType(j:Data) =
    __prism.getOption(j).isDefined

  def unapply(j:Data):Option[T] =
    _getValue(j)
}

//
//class EmptyProperty[Data, IndexedData, T](implicit val _codec: CodecJson[T]) extends Property[T] {
//
//  private[jsentric] def _nameOverride: Option[String] = None
//  def _pathValidator: Validator[T] = ???
//  def _isValidType(j:Json) = false
//}

