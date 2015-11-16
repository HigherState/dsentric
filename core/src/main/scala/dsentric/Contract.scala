package dsentric

import monocle.Monocle._
import monocle._
import monocle.function.Index

private[dsentric] sealed trait Struct {

  def apply[R](f:this.type => R):R = f(this)

}

sealed trait Property[Data, IndexedData, T <: Any] extends Struct {
  private[dsentric] var _path:Option[Optional[Data, Data]] = None
  private[dsentric] lazy val _pathPrism: Option[Optional[Data, T]] =
    _path.map(_.composePrism(_prism))

  private[dsentric] def _index: Index[IndexedData, String, Data]
  private[dsentric] def _prism: Prism[Data, T]
  private[dsentric] def _nameOverride:Option[String]
  private[dsentric] def _pathValidator:Validator[_]
  private[dsentric] def _isValidType(j:Data):Boolean

  private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures
}

//prevents clash of local nested implicit requirements with those in scope
private[dsentric] sealed trait MapImplicits[Data, IndexedData, T] {

  implicit private[dsentric] val __prism: Prism[Data, T]
  implicit private[dsentric] val __index: Index[IndexedData, String, Data]

  private[dsentric] def _index: Index[IndexedData, String, Data] =
    __index
  private[dsentric] def _prism: Prism[Data, T] =
    __prism
}

private[dsentric] sealed trait ContractBase[Data, IndexedData] extends Struct with MapImplicits[Data, IndexedData, IndexedData] {
  private var __fields = Vector.empty[(String, Property[Data, IndexedData, Any])]

  private[dsentric] def _prism:Prism[Data, IndexedData]
  private[dsentric] val _pathPrism:Option[Optional[Data, IndexedData]]
  private[dsentric] def _fields = __fields

  private[dsentric] def _setPropertyNames():Unit = {
    __fields = this.getClass.getMethods.foldLeft(__fields) { (f, m) =>
      if (classOf[Struct].isAssignableFrom(m.getReturnType) && m.getTypeParameters.isEmpty) {
        m.invoke(this) match {
          case prop: Property[Data, IndexedData, Any]@unchecked =>
            val name = prop._nameOverride.getOrElse(m.getName)
            prop._path = _pathPrism.map(_.composeOptional(index[IndexedData, String, Data](name)(prop._index)))
            prop match {
              case c: ContractBase[Data, IndexedData]@unchecked =>
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

  private [dsentric] def _validateFields(path:Path, value:Option[Data], currentState:Option[Data]) =
    value.flatMap(_prism.getOption).fold(Failures.empty){ indexedData =>
      val cs = currentState.flatMap(_prism.getOption)
      _fields.flatMap{kv =>
        val i = _index.index(kv._1)
        val v = i.getOption(indexedData)
        val c = cs.flatMap(i.getOption)
        kv._2._validate(path :+ Right(kv._1), v, c)
      }
    }

}


abstract class ExpectedSubContract[Data, IndexedData](private[dsentric] override val _pathValidator:Validator[IndexedData], private[dsentric] override val _nameOverride:Option[String])
 (implicit private[dsentric] override val __prism: Prism[Data, IndexedData],  private[dsentric] override val __index:Index[IndexedData, String, Data])
  extends Expected[Data, IndexedData, IndexedData](_pathValidator, _nameOverride) with ContractBase[Data, IndexedData]{

  def this(name:Option[String])(implicit prism: Prism[Data, IndexedData], index:Index[IndexedData, String, Data]) =
    this(Validator.empty, name)

  override private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    super._validate(path, value, currentState) match {
      case Failures.empty =>
        _validateFields(path, value, currentState)
      case failures =>
        failures
    }
}

abstract class MaybeSubContract[Data, IndexedData](private[dsentric] override val _pathValidator:Validator[Option[IndexedData]], private[dsentric] override val _nameOverride:Option[String])
                                                     (implicit private[dsentric] override val __prism: Prism[Data, IndexedData],  private[dsentric] override val __index:Index[IndexedData, String, Data], strictness:Strictness)
  extends Maybe[Data, IndexedData, IndexedData](_pathValidator, _nameOverride) with ContractBase[Data, IndexedData]{

  def this(name:Option[String])(implicit prism: Prism[Data, IndexedData], index:Index[IndexedData, String, Data], strictness:Strictness) =
    this(Validator.empty, name)

  override private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    super._validate(path, value, currentState) match {
      case Failures.empty =>
        _validateFields(path, value, currentState)
      case failures =>
        failures
    }
}


abstract class Contract[Data, IndexedData](implicit private[dsentric] val __prism:Prism[Data, IndexedData], private[dsentric] val __index:Index[IndexedData, String, Data])
  extends ContractBase[Data, IndexedData] with App {

  private[dsentric] val _pathPrism = Some(__prism.asOptional)

  override def delayedInit(body: => Unit) = {
    body
    _setPropertyNames()
  }

  def $validate(value:Data, currentState:Option[Data] = None):Failures =
    __prism.getOption(value).fold(Failures(Path.empty -> ValidationText.UNEXPECTED_TYPE)){ _ =>
      _validateFields(Path.empty, Some(value), currentState)
    }
}

class Expected[Data, IndexedData, T](private[dsentric] val _pathValidator:Validator[T], private[dsentric] val _nameOverride:Option[String])
                                     (implicit private[dsentric] val __prism: Prism[Data, T], private[dsentric] val __index: Index[IndexedData, String, Data])
  extends Property[Data, IndexedData, T] with MapImplicits[Data, IndexedData, T] {

  def unapply(j:Data):Option[T] =
    _pathPrism.flatMap(_.getOption(j))

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
                                (implicit private[dsentric] val __prism: Prism[Data, T], private[dsentric] val __index: Index[IndexedData, String, Data], strictness:Strictness)
  extends Property[Data, IndexedData, T] with MapImplicits[Data, IndexedData, T] {

  private[dsentric] def _isValidType(j:Data) =
    strictness(j, Optional.id, __prism).isDefined
  def unapply(j:Data):Option[Option[T]] = {
    _path.flatMap(strictness(j, _, __prism))
  }

  private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    value -> currentState match {
      case (Some(v), c)  =>
        strictness(v, Optional.id, __prism).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(strictness(_, Optional.id, __prism)))
        }
      case (None, c) =>
        _pathValidator(path, None, c.flatMap(strictness(_, Optional.id, __prism)))
    }
}

class Default[Data, IndexedData, T] private[dsentric](val _default:T, private[dsentric] val _pathValidator:Validator[Option[T]], private[dsentric] val _nameOverride:Option[String])
                                        (implicit private[dsentric] val __prism: Prism[Data, T], private[dsentric] val __index: Index[IndexedData, String, Data], strictness:Strictness)
  extends Property[Data, IndexedData, T] with MapImplicits[Data, IndexedData, T] {

  private[dsentric] def _isValidType(j:Data) =
    strictness(j, Optional.id, __prism).isDefined

  def unapply(j:Data):Option[T] =
    _path.flatMap(strictness(j, _, __prism)).map(_.getOrElse(_default))

  private[dsentric] def _validate(path:Path, value:Option[Data], currentState:Option[Data]):Failures =
    value -> currentState match {
      case (Some(v), c) =>
        strictness(v, Optional.id, __prism).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(strictness(_, Optional.id, __prism)))
        }
      case (None, c) =>
        _pathValidator(path, Some(_default), c.flatMap(strictness(_, Optional.id, __prism)))
    }
}

abstract class ValueContract[Data, IndexedData, T] private[dsentric](val _pathValidator: Validator[T] = Validator.empty)
                                                                    (implicit private[dsentric] val __prism: Prism[Data, T], private[dsentric] val __index: Index[IndexedData, String, Data])
  extends Property[Data, IndexedData, T] with MapImplicits[Data, IndexedData, T]{

  private[dsentric] def _isValidType(j:Data) =
    __prism.getOption(j).isDefined

  def unapply(j:Data):Option[T] =
    _pathPrism.flatMap(_.getOption(j))
}

//
//class EmptyProperty[Data, IndexedData, T](implicit val _codec: CodecJson[T]) extends Property[T] {
//
//  private[jsentric] def _nameOverride: Option[String] = None
//  def _pathValidator: Validator[T] = ???
//  def _isValidType(j:Json) = false
//}

