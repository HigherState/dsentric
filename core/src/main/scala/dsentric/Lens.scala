package dsentric

import monocle._

sealed trait PropertyLens[Data, T] {

  private[dsentric] var _path:Optional[Data, Option[Data]]
  private[dsentric] def _prism: Prism[Data, T]

  def $get(data:Data):Option[T] =
    _path.getOption(data).flatMap{
      case Some(v) => _prism.getOption(v)
      case None => None
    }

  def $set(value:T):Data => Data =
    _path.set(Some(_prism.reverseGet(value)))

  def $maybeSet(value:Option[T]):Data => Data =
    value.fold(idFunc[Data]){ v =>
      $set(v)
    }
}

trait ExpectedLens[Data, T] extends PropertyLens[Data, T] with ComposableLens[Data, T, T] {

  def $modify(f:T => T):Data => Data =
    _path.modify{
      case None => None
      case Some(t) =>
        //failure to prism will modify to the same value
        _prism.getOption(t).fold(Some(t)){v =>
          Some(_prism.reverseGet(f(v)))
        }
    }

  def $copy(p:PropertyLens[Data, T]):Data => Data =
    (d:Data) => {
      p.$get(d).fold(d){p =>
        $set(p)(d)
      }
    }
}

trait MaybeLens[Data, T] extends PropertyLens[Data, T] with ComposableLens[Data, Option[T], T] {

  private[dsentric] def _strictness:Strictness

  def $modify(f:Option[T] => Option[T]):Data => Data =
    _path.modify{
      case None =>
        f(None).map(_prism.reverseGet)
      case Some(v) =>
        _strictness(v, _prism).fold[Option[Data]](Some(v)){ maybeT =>
          f(maybeT).map(_prism.reverseGet)
        }
    }

  def $drop:Data => Data =
    _path.modify(_ => None)

  def $setOrDrop(value:Option[T]):Data => Data =
    _path.modify(_ => value.map(_prism.reverseGet))

  //no enforcement of strict yet
  def $copy(p:PropertyLens[Data, T]):Data => Data =
    (d:Data) => {
      p.$get(d).fold($drop(d)){p =>
        $set(p)(d)
      }
    }
}

trait DefaultLens[Data, T] extends PropertyLens[Data, T] with ComposableLens[Data, T, T]{

  def _default:T

  private[dsentric] def _strictness:Strictness

  override def $get(data:Data):Option[T] =
    _path.getOption(data).flatMap{
      case Some(v) =>
        _strictness(v, _prism).map(_.getOrElse(_default))
      case None =>
        Some(_default)
    }

  def $modify(f:T => T):Data => Data =
    _path.modify{
      case None =>
        Some(_prism.reverseGet(f(_default)))
      case Some(v) =>
        _strictness(v, _prism).fold[Option[Data]](Some(v)){ maybeT =>
          Some(_prism.reverseGet(f(maybeT.getOrElse(_default))))
        }
    }

  def $restore:Data => Data =
    _path.modify(_ => None)

  def $setOrRestore(value:Option[T]):Data => Data =
    _path.modify(_ => value.map(_prism.reverseGet))

  def $copy(p:PropertyLens[Data, T]):Data => Data =
    (d:Data) => {
      p.$get(d).fold(d){p =>
        $set(p)(d)
      }
    }
}

trait MaybeDeltaDelete[Data, IndexedData, T] extends Any {
  def maybeProperty:Maybe[Data, IndexedData, T]
  protected def deleteValue:Data

  def $deltaDelete:Data => Data =
    maybeProperty._path.set(Some(deleteValue))
}

trait DefaultDeltaDelete[Data, IndexedData, T] extends Any {
  def defaultProperty:Default[Data, IndexedData, T]
  protected def deleteValue:Data

  def $deltaDelete:Data => Data =
    defaultProperty._path.set(Some(deleteValue))
}