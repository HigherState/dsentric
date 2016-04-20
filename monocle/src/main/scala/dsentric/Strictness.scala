package dsentric

import monocle.{Optional, Prism}

trait Strictness {
  def apply[Data, T](value:Data, path:Optional[Data, Option[Data]], prism:Prism[Data, T]):Option[Option[T]] =
    path.getOption(value).fold[Option[Option[T]]](Some(None)){
      case None => Some(None)
      case Some(v) => apply(v, prism)
    }

  def apply[Data, T](value:Data, prism:Prism[Data, T]):Option[Option[T]]
}

/*
  An incorrect type will return a match with None
 */
object MaybeOptimistic extends Strictness {
  def apply[Data, T](value:Data, prism:Prism[Data, T]):Option[Option[T]] =
    Some(prism.getOption(value))
}

/*
  An incorrect type will fail to return a match
 */
object MaybePessimistic extends Strictness {
  def apply[Data, T](value:Data, prism:Prism[Data, T]):Option[Option[T]] =
    prism.getOption(value).map(Some.apply)
}