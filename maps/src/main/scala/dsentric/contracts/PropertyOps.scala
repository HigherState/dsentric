package dsentric.contracts

import dsentric.operators.{DataOperator, RawValidator, Validators}
import dsentric._

trait PropertyOps[D <: DObject] {

  protected def __self:BaseContract[D] = null

  def \[T](implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, PropertyOps.requiredS)

  def \[T](dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, Validators.required :: dataOperators.toList)

  def \[T](name:String, dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](Some(name), __self, codec, Validators.required :: dataOperators.toList)



  def \?[T](implicit codec:DCodec[T]):MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, __self, codec, Nil)

  def \?[T](dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T]):MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, __self, codec, dataOperators.toList)

  def \?[T](name:String, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T]):MaybeProperty[D, T] =
    new MaybeProperty[D, T](Some(name), __self, codec, dataOperators.toList)


  def \![T](default:T, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T]):DefaultProperty[D, T] =
    new DefaultProperty[D, T](None, default, __self, codec, dataOperators.toList)

  def \![T](name:String, default:T, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T]):DefaultProperty[D, T] =
    new DefaultProperty[D, T](Some(name), default:T, __self, codec, dataOperators.toList)


  def \::[T <: DObject](contract:ContractFor[T], dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DObjectCodec[T]):ObjectsProperty[D, T] =
    new ObjectsProperty[D, T](None, contract, __self, codec, dataOperators.toList)

  def \::[T <: DObject](contract:ContractFor[T], name:String, dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DObjectCodec[T]):ObjectsProperty[D, T] =
    new ObjectsProperty[D, T](Some(name), contract, __self, codec, dataOperators.toList)



  def \->[K, T <: DObject](contract:ContractFor[T], dataOperators: DataOperator[Option[Map[K, T]]]*)(implicit keyCodec:StringCodec[K], valueCodec:DObjectCodec[T]):MapObjectsProperty[D, K, T] =
    new MapObjectsProperty[D, K, T](None, contract, __self, keyCodec, valueCodec, dataOperators.toList)

  def \->[K, T <: DObject](contract:ContractFor[T], name:String, dataOperators: DataOperator[Option[Map[K, T]]]*)(implicit keyCodec:StringCodec[K], valueCodec:DObjectCodec[T]):MapObjectsProperty[D, K, T] =
    new MapObjectsProperty[D, K, T](Some(name), contract, __self, keyCodec, valueCodec, dataOperators.toList)

}

object PropertyOps {
  val requiredS: List[RawValidator[Nothing]] =
    List(Validators.required)
}
