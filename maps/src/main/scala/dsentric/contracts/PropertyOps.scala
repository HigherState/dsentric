package dsentric.contracts

import dsentric.operators.DataOperator
import dsentric._
import dsentric.codecs.DCodec

trait PropertyOps[D <: DObject] {

  protected def __self:BaseContract[D] = null


  def \[T](implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, Nil)

  def \[T](dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](None, __self, codec, dataOperators.toList)

  def \[T](name:String, dataOperators: DataOperator[T]*)(implicit codec:DCodec[T]):ExpectedProperty[D, T] =
    new ExpectedProperty[D, T](Some(name), __self, codec, dataOperators.toList)



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

}

