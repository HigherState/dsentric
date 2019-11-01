package dsentric.contracts

import dsentric.operators.{DataOperator, Validators}
import dsentric.{DCodec, DObject, StringCodec}

trait PropertyOps[D <: DObject] {

  protected def __self:BaseContract[D] =
    NothingBaseContract.asInstanceOf[BaseContract[D]]


  def \[T](implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, PropertyOps.requiredS)

  def \[T](dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, Validators.required +: dataOperators)

  def \[T](name:String, dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](Some(name), __self, codec, Validators.required +: dataOperators)



  def \?[T](implicit codec:DCodec[T]):MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, __self, codec, Seq.empty)

  def \?[T](dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T]):MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, __self, codec, dataOperators)

  def \?[T](name:String, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T]):MaybeProperty[D, T] =
    new MaybeProperty[D, T](Some(name), __self, codec, dataOperators)


  def \![T](default:T, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T]):DefaultProperty[D, T] =
    new DefaultProperty[D, T](None, default, __self, codec, dataOperators)

  def \![T](name:String, default:T, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T]):DefaultProperty[D, T] =
    new DefaultProperty[D, T](Some(name), default:T, __self, codec, dataOperators)


  def \::[T <: DObject](contract:ContractFor[T], dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]]):ObjectsProperty[D, T] =
    new ObjectsProperty[D, T](None, contract, __self, codec, dataOperators)

  def \::[T <: DObject](contract:ContractFor[T], name:String, dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]]):ObjectsProperty[D, T] =
    new ObjectsProperty[D, T](Some(name), contract, __self, codec, dataOperators)



  def \->[K, T <: DObject](contract:ContractFor[T], dataOperators: DataOperator[Option[Map[K, T]]]*)(implicit codec:DCodec[Map[K, T]], keyCodec:StringCodec[K]):MapObjectsProperty[D, K, T] =
    new MapObjectsProperty[D, K, T](None, contract, __self, codec, keyCodec, Validators.required +: dataOperators)

  def \->[K, T <: DObject](contract:ContractFor[T], name:String, dataOperators: DataOperator[Option[Map[K, T]]]*)(implicit codec:DCodec[Map[K, T]], keyCodec:StringCodec[K]):MapObjectsProperty[D, K, T] =
    new MapObjectsProperty[D, K, T](Some(name), contract, __self, codec, keyCodec, Validators.required +: dataOperators)

}

object PropertyOps {
  val requiredS = Seq(Validators.required)
}
