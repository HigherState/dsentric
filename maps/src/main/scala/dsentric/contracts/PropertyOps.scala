package dsentric.contracts

import dsentric.operators.{DataOperator, Validator, Validators}
import dsentric.{DCodec, DObject, Strictness}

private[contracts] trait PropertyOps[D <: DObject] extends BaseContract[D] { self =>

  def \[T](implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, this, codec, Seq.empty)

  def \[T](dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, this, codec, dataOperators)

  def \[T](name:String, dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](Some(name), this, codec, dataOperators)



  def \?[T](implicit codec:DCodec[T], strictness: Strictness):MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, this, codec, strictness, Seq.empty)

  def \?[T](dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T], strictness: Strictness):MaybeProperty[D, T] =
    new MaybeProperty[D, T](None, this, codec, strictness, dataOperators)

  def \?[T](name:String, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T], strictness: Strictness):MaybeProperty[D, T] =
    new MaybeProperty[D, T](Some(name), this, codec, strictness, dataOperators)


  def \![T](default:T, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T], strictness: Strictness):DefaultProperty[D, T] =
    new DefaultProperty[D, T](None, default,this, codec, strictness, dataOperators)

  def \![T](name:String, default:T, dataOperators: DataOperator[Option[T]]*)(implicit codec:DCodec[T], strictness: Strictness):DefaultProperty[D, T] =
    new DefaultProperty[D, T](Some(name), default:T, this, codec, strictness, dataOperators)


  def \:[T <: DObject, C <: ContractFor[T]](contract:C, dataOperators: DataOperator[Vector[T]]*)(implicit codec:DCodec[Vector[T]]):ExpectedObjectsProperty[D, T, C] =
    new ExpectedObjectsProperty[D, T, C](None, contract, this, codec, dataOperators)

  def \:[T <: DObject, C <: ContractFor[T]](contract:C, name:String, dataOperators: DataOperator[Vector[T]]*)(implicit codec:DCodec[Vector[T]]):ExpectedObjectsProperty[D, T, C] =
    new ExpectedObjectsProperty[D, T, C](Some(name), contract, this, codec, dataOperators)


  def \:?[T <: DObject, C <: ContractFor[T]](contract:C, dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]], strictness:Strictness):MaybeObjectsProperty[D, T, C] =
    new MaybeObjectsProperty[D, T, C](None, contract, this, codec, strictness, dataOperators)

  def \:?[T <: DObject, C <: ContractFor[T]](contract:C, name:String, dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]], strictness:Strictness):MaybeObjectsProperty[D, T, C] =
    new MaybeObjectsProperty[D, T, C](Some(name), contract, this, codec, strictness, dataOperators)


  def \:![T <: DObject, C <: ContractFor[T]](contract:C, default:Vector[T], dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]], strictness: Strictness):DefaultObjectsProperty[D, T, C] =
    new DefaultObjectsProperty[D, T, C](None, contract, default, this, codec, strictness, dataOperators)

  def \:![T <: DObject, C <: ContractFor[T]](contract:C, name:String, default:Vector[T], dataOperators: DataOperator[Option[Vector[T]]]*)(implicit codec:DCodec[Vector[T]], strictness: Strictness):DefaultObjectsProperty[D, T, C] =
    new DefaultObjectsProperty[D, T, C](Some(name), contract, default, this, codec, strictness, dataOperators)

}
