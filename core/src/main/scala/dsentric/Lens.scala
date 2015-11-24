package dsentric

import monocle._

trait PropertyLens[Data, T] {

  private[dsentric] def _pathPrism: Optional[Data, T]

  def $get(data:Data):Option[T] =
    _pathPrism.getOption(data)

  def $set(value:T):Data => Data =
    _pathPrism.set(value)

  def $maybeSet(value:Option[T]):Data => Data =
    value.fold(idFunc[Data]){ v =>
      $set(v)
    }

  def $copy(p:PropertyLens[Data, T]):Data => Data =
    (d:Data) => {
      p.$get(d).fold(d){p =>
        $set(p)(d)
      }
    }
}

trait ExpectedLens[Data, T] extends PropertyLens[Data, T]{

  def $modify(f:T => T) =
    _pathPrism.modify(f)
}