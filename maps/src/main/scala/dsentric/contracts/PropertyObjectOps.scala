package dsentric.contracts

import dsentric.{DCodec, DObject, Strictness}
import dsentric.operators.{DataOperator, Validator, Validators}

private[contracts] trait PropertyObjectOps[D <: DObject] extends BaseContract[D] { self =>

  class \\ private(override private[contracts] val __nameOverride:Option[String],
                   override val _codec:DCodec[DObject],
                   override val _dataOperators:Seq[DataOperator[DObject]]
                  ) extends ExpectedProperty[D, DObject](__nameOverride, self, _codec) with SubContractFor[D] {

    def this()(implicit codec:DCodec[DObject]) =
      this(Validators.empty, None, codec)
    def this(validator:Validator[DObject])(implicit codec:DCodec[DObject]) =
      this(validator, None, codec)
    def this(name:String, validator:Validator[DObject] = Validators.empty)(implicit codec:DCodec[DObject]) =
      this(Validators.empty, Some(name), codec)

  }

  class \\? private(override private[contracts] val __nameOverride:Option[String],
                    override val _codec:DCodec[DObject],
                    override val _strictness:Strictness,
                    override val _dataOperators:Seq[DataOperator[Option[DObject]]]
                   ) extends MaybeProperty[D, DObject](__nameOverride, self, _codec, _strictness, _dataOperators) with SubContractFor[D] {

    def this()(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(Validators.empty, None, strictness, codec)
    def this(validator:Validator[Option[DObject]])(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(validator, None, strictness, codec)
    def this(name:String, validator:Validator[DObject] = Validators.empty)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(Validators.empty, Some(name), strictness, codec)
  }

  class \\! private(override private[contracts] val __nameOverride:Option[String],
                    override val _default:DObject,
                    override val _codec:DCodec[DObject],
                    override val _strictness:Strictness,
                    override val _dataOperators:Seq[DataOperator[Option[DObject]]]
                   ) extends DefaultProperty[D, DObject](__nameOverride, _default, self, _codec, _strictness, _dataOperators) with SubContractFor[D] {

    def this(default:DObject)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(default, Validators.empty, None, strictness, codec)
    def this(default:DObject, validator:Validator[Option[DObject]])(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(default, validator, None, strictness, codec)
    def this(default:DObject, name:String, validator:Validator[Option[DObject]] = Validators.empty)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(default, Validators.empty, Some(name), strictness, codec)
  }
}
