package dsentric

trait PropertyOps {

  private val emptyContract = new Contract {}

  def \[T](name:String)(implicit codec:JCodec[T]) =
    new Expected[T](Validator.empty, Some(name), emptyContract, codec)

  def \[T](path:Path)(implicit codec:JCodec[T]) = {
    val e = new Expected[T](Validator.empty, None, emptyContract, codec)
    e._forcePath(path)
    e
  }

  def \?[T](name:String)(implicit codec:JCodec[T], strictness: Strictness) =
    new Maybe[T](Validator.empty, Some(name), emptyContract, codec, strictness)

  def \?[T](path:Path)(implicit codec:JCodec[T], strictness: Strictness) = {
    val e = new Maybe[T](Validator.empty, None, emptyContract, codec, strictness)
    e._forcePath(path)
    e
  }

  def \![T](name:String, default:T)(implicit codec:JCodec[T], strictness: Strictness) =
    new Default[T](default, Validator.empty, Some(name), emptyContract, codec, strictness)

  def \![T](path:Path, default:T)(implicit codec:JCodec[T], strictness: Strictness) = {
    val e = new Default[T](default, Validator.empty, None, emptyContract, codec, strictness)
    e._forcePath(path)
    e
  }
}
