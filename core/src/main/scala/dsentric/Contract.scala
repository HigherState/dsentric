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

private[dsentric] sealed trait ContractBase[Data, IndexedData] extends Struct {
  private[dsentric] def _prism:Prism[Data, IndexedData]
  private[dsentric] val _pathPrism:Option[Optional[Data, IndexedData]]

  private[dsentric] def _setPropertyNames():Unit = {
    this.getClass.getMethods.foreach(m =>
      if (classOf[Struct].isAssignableFrom(m.getReturnType) && m.getTypeParameters.isEmpty) {
        m.invoke(this) match {
          case prop:Property[Data, IndexedData, Any] @unchecked =>
            val name = prop._nameOverride.getOrElse(m.getName)
            prop._path = _pathPrism.map(_.composeOptional(index[IndexedData, String, Data](name)(prop._index)))
            prop match {
              case c:ContractBase[Data, IndexedData] @unchecked =>
                c._setPropertyNames()
              case _ =>
            }
          case _ =>
        }
      }
    )
  }
}


abstract class ExpectedSubContract[Data, IndexedData](private[dsentric] override val _pathValidator:Validator[IndexedData], private[dsentric] override val _nameOverride:Option[String])
 (implicit private[dsentric] override val __prism: Prism[Data, IndexedData],  private[dsentric] override val __index:Index[IndexedData, String, Data])
  extends Expected[Data, IndexedData, IndexedData](_pathValidator, _nameOverride) with ContractBase[Data, IndexedData]{

  def this(name:Option[String])(implicit prism: Prism[Data, IndexedData], index:Index[IndexedData, String, Data]) =
    this(Validator.empty, name)
}


abstract class Contract[Data, IndexedData](implicit private[dsentric] val __prism:Prism[Data, IndexedData], private[dsentric] val __index:Index[IndexedData, String, Data])
  extends ContractBase[Data, IndexedData] with MapImplicits[Data, IndexedData, IndexedData] with App {

  private[dsentric] val _pathPrism = Some(__prism.asOptional)

  override def delayedInit(body: => Unit) = {
    body
    _setPropertyNames()
  }
}

class Expected[Data, IndexedData, T](private[dsentric] val _pathValidator:Validator[T], private[dsentric] val _nameOverride:Option[String])
                                     (implicit private[dsentric] val __prism: Prism[Data, T], private[dsentric] val __index: Index[IndexedData, String, Data])
  extends Property[Data, IndexedData, T] with MapImplicits[Data, IndexedData, T] {

  private[dsentric] def _isValidType(j:Data) =
    __prism.getOption(j).isDefined
  def unapply(j:Data):Option[T] =
    _pathPrism.flatMap(_.getOption(j))
}


class Maybe[Data, IndexedData, T] private[dsentric](private[dsentric] val _pathValidator:Validator[Option[T]], private[dsentric] val _nameOverride:Option[String])
                                (implicit private[dsentric] val __prism: Prism[Data, T], private[dsentric] val __index: Index[IndexedData, String, Data], strictness:Strictness)
  extends Property[Data, IndexedData, T] with MapImplicits[Data, IndexedData, T] {

  private[dsentric] def _isValidType(j:Data) =
    strictness(j, Optional.id, __prism).isDefined
  def unapply(j:Data):Option[Option[T]] = {
    _path.flatMap(strictness(j, _, __prism))
  }
}

class Default[Data, IndexedData, T] private[dsentric](val _default:T, private[dsentric] val _pathValidator:Validator[Option[T]], private[dsentric] val _nameOverride:Option[String])
                                        (implicit private[dsentric] val __prism: Prism[Data, T], private[dsentric] val __index: Index[IndexedData, String, Data], strictness:Strictness)
  extends Property[Data, IndexedData, T] with MapImplicits[Data, IndexedData, T] {

  private[dsentric] def _isValidType(j:Data) =
    strictness(j, Optional.id, __prism).isDefined

  def unapply(j:Data):Option[T] =
    _path.flatMap(strictness(j, _, __prism)).map(_.getOrElse(_default))
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

