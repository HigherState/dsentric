package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric.{DCodec, DObject, Strictness}

trait PropertyOps[D <: DObject] {

  protected def __self:BaseContract[D] =
    NothingBaseContract.asInstanceOf[BaseContract[D]]

  def \[T](implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, Seq.empty)

  def \[T](dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, dataOperators)

  def \[T](name:String, dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](Some(name), __self, codec, dataOperators)



  def \?[T](implicit codec:DCodec[T], strictness: Strictness):MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, __self, codec, strictness, Seq.empty)

  def \?[T](dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T], strictness: Strictness):MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, __self, codec, strictness, dataOperators)

  def \?[T](name:String, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T], strictness: Strictness):MaybeProperty[D, T] =
    new MaybeProperty[D, T](Some(name), __self, codec, strictness, dataOperators)


  def \![T](default:T, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T], strictness: Strictness):DefaultProperty[D, T] =
    new DefaultProperty[D, T](None, default, __self, codec, strictness, dataOperators)

  def \![T](name:String, default:T, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T], strictness: Strictness):DefaultProperty[D, T] =
    new DefaultProperty[D, T](Some(name), default:T, __self, codec, strictness, dataOperators)


  def \:[T <: DObject](contract:ContractFor[T], dataOperators: DataOperator[Vector[T]]*)(implicit codec:DCodec[Vector[T]]):ExpectedObjectsProperty[D, T] =
    new ExpectedObjectsProperty[D, T](None, contract, __self, codec, dataOperators)

  def \:[T <: DObject](contract:ContractFor[T], name:String, dataOperators: DataOperator[Vector[T]]*)(implicit codec:DCodec[Vector[T]]):ExpectedObjectsProperty[D, T] =
    new ExpectedObjectsProperty[D, T](Some(name), contract, __self, codec, dataOperators)


  def \:?[T <: DObject](contract:ContractFor[T], dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]], strictness:Strictness):MaybeObjectsProperty[D, T] =
    new MaybeObjectsProperty[D, T](None, contract, __self, codec, strictness, dataOperators)

  def \:?[T <: DObject](contract:ContractFor[T], name:String, dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]], strictness:Strictness):MaybeObjectsProperty[D, T] =
    new MaybeObjectsProperty[D, T](Some(name), contract, __self, codec, strictness, dataOperators)


  def \:![T <: DObject](contract:ContractFor[T], default:Vector[T], dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]], strictness: Strictness):DefaultObjectsProperty[D, T] =
    new DefaultObjectsProperty[D, T](None, contract, default, __self, codec, strictness, dataOperators)

  def \:![T <: DObject](contract:ContractFor[T], name:String, default:Vector[T], dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]], strictness: Strictness):DefaultObjectsProperty[D, T] =
    new DefaultObjectsProperty[D, T](Some(name), contract, default, __self, codec, strictness, dataOperators)

}
