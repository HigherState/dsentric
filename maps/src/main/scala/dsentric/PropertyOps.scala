package dsentric

trait PropertyOps {

  private val emptyContract = new Contract {}

  def \[T](name:String)(implicit codec:DCodec[T]) =
    new Expected[DObject, T](Validators.empty, Some(name), emptyContract, codec)

  def \[T](path:Path)(implicit codec:DCodec[T]) = {
    val e = new Expected[DObject, T](Validators.empty, None, emptyContract, codec)
    e._forcePath(path)
    e
  }

  def \?[T](name:String)(implicit codec:DCodec[T], strictness: Strictness) =
    new Maybe[DObject, T](Validators.empty, Some(name), emptyContract, codec, strictness)

  def \?[T](path:Path)(implicit codec:DCodec[T], strictness: Strictness) = {
    val e = new Maybe[DObject, T](Validators.empty, None, emptyContract, codec, strictness)
    e._forcePath(path)
    e
  }

  def \![T](name:String, default:T)(implicit codec:DCodec[T], strictness: Strictness) =
    new Default[DObject, T](default, Validators.empty, Some(name), emptyContract, codec, strictness)

  def \![T](path:Path, default:T)(implicit codec:DCodec[T], strictness: Strictness) = {
    val e = new Default[DObject, T](default, Validators.empty, None, emptyContract, codec, strictness)
    e._forcePath(path)
    e
  }
}
