package dsentric.contracts

import dsentric.{DCodec, DObject, Strictness}
import dsentric.operators.DataOperator

trait PropertyObjectOps[D <: DObject]  {

  protected def __self:BaseContract[D] =
    NothingBaseContract.asInstanceOf[BaseContract[D]]

  class \\ private(override private[contracts] val __nameOverride:Option[String],
                   override val _codec:DCodec[DObject],
                   override val _dataOperators:Seq[DataOperator[DObject]]
                  ) extends ExpectedProperty[D, DObject](__nameOverride, __self, _codec, _dataOperators) with SubContractFor[D] {

    def this(dataOperators: DataOperator[DObject]*)(implicit codec:DCodec[DObject]) =
      this(None, codec, dataOperators)
    def this(name:String, dataOperators: DataOperator[DObject]*)(implicit codec:DCodec[DObject]) =
      this(Some(name), codec, dataOperators)

  }

  class \\? private(override private[contracts] val __nameOverride:Option[String],
                    override val _codec:DCodec[DObject],
                    override val _strictness:Strictness,
                    override val _dataOperators:Seq[DataOperator[Option[DObject]]]
                   ) extends MaybeProperty[D, DObject](__nameOverride, __self, _codec, _strictness, _dataOperators) with SubContractFor[D] {

    def this(dataOperators: DataOperator[Option[DObject]]*)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(None, codec, strictness, dataOperators)
    def this(name:String, dataOperators: DataOperator[Option[DObject]]*)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(Some(name), codec, strictness, dataOperators)

  }

  class \\! private(override private[contracts] val __nameOverride:Option[String],
                    override val _default:DObject,
                    override val _codec:DCodec[DObject],
                    override val _strictness:Strictness,
                    override val _dataOperators:Seq[DataOperator[Option[DObject]]]
                   ) extends DefaultProperty[D, DObject](__nameOverride, _default, __self, _codec, _strictness, _dataOperators) with SubContractFor[D] {


    def this(default:DObject, dataOperators: DataOperator[Option[DObject]]*)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(None, default, codec, strictness, dataOperators)
    def this(default:DObject, name:String, dataOperators: DataOperator[Option[DObject]]*)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(Some(name), default, codec, strictness, dataOperators)

  }
}
