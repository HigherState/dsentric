package dsentric


trait Strictness {
  def apply[T](value:Map[String, Any], path:Path, codec:DCodec[T]):Option[Option[T]] =
    PathLensOps.traverse(value, path).fold[Option[Option[T]]](Some(None)){apply(_, codec)}

  def apply[T](value:Any, codec:DCodec[T]):Option[Option[T]]
}

/*
  An incorrect type will return a match with None
 */
object MaybeOptimistic extends Strictness {
  def apply[T](value:Any, codec:DCodec[T]):Option[Option[T]] =
    Some(codec.unapply(value))
}

/*
  An incorrect type will fail to return a match
 */
object MaybePessimistic extends Strictness {
  def apply[T](value:Any, codec:DCodec[T]):Option[Option[T]] =
    codec.unapply(value).map(Some.apply)
}