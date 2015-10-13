package dsentric

import monocle.{Prism, Optional}

trait Strictness {
  def apply[Data, T](value:Data, path:Optional[Data, Data], prism:Prism[Data, T]):Option[Option[T]]
}

object MaybeOptimistic extends Strictness {
  def apply[Data, T](value: Data, path:Optional[Data, Data], prism:Prism[Data, T]): Option[Option[T]] =
    Some(path.composePrism(prism).getOption(value))
}

object MaybePessimistic extends Strictness {
  def apply[Data, T](value: Data, path:Optional[Data, Data], prism:Prism[Data, T]): Option[Option[T]] =
    path.getOption(value).fold[Option[Option[T]]](Some(None))(d => prism.getOption(d).map(Some.apply))
}