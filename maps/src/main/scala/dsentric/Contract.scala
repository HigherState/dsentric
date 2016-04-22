package dsentric

private[dsentric] sealed trait Struct {

  def apply[R](f:this.type => R):R = f(this)
  def _path:Path

}

private[dsentric] sealed trait BaseContract extends Struct {
  private var __fields:Vector[(String, Property[Any])] = _
  @volatile
  private var _bitmap0:Boolean = false

  protected implicit def selfRef:BaseContract = this

  private[dsentric] def _fields =
    if (_bitmap0) __fields
    else {
      this.synchronized{
        __fields = this.getClass.getMethods.flatMap { m =>
          if (classOf[Struct].isAssignableFrom(m.getReturnType) && m.getTypeParameters.isEmpty)
            m.invoke(this) match {
              case prop:Property[Any]@unchecked =>
                Some(m.getName -> prop)
              case _ =>
                None
            }
          else None
        }.toVector
        _bitmap0 = true
      }
      __fields
    }

  def \[T](implicit codec:JCodec[T]) =
    new Expected[T](Validator.empty, None, this, codec)

  def \[T](validator:Validator[T])(implicit codec:JCodec[T]) =
    new Expected[T](validator, None, this, codec)

  def \[T](name:String, validator:Validator[T] = Validator.empty)(implicit codec:JCodec[T]) =
    new Expected[T](validator, Some(name), this, codec)

  def \?[T](implicit codec:JCodec[T], strictness: Strictness) =
    new Maybe[T](Validator.empty, None, this, codec, strictness)

  def \?[T](validator:Validator[T])(implicit codec:JCodec[T], strictness: Strictness) =
    new Maybe[T](validator, None, this, codec, strictness)

  def \?[T](name:String, validator:Validator[T] = Validator.empty)(implicit codec:JCodec[T], strictness: Strictness) =
    new Maybe[T](validator, Some(name), this, codec, strictness)

  def \![T](default:T)(implicit codec:JCodec[T], strictness: Strictness) =
    new Default[T](default, Validator.empty, None, this, codec, strictness)

  def \![T](default:T, validator:Validator[T])(implicit codec:JCodec[T], strictness: Strictness) =
    new Default[T](default:T, validator, None, this, codec, strictness)

  def \![T](name:String,default:T, validator:Validator[T] = Validator.empty)(implicit codec:JCodec[T], strictness: Strictness) =
    new Default[T](default:T, validator, Some(name), this, codec, strictness)



//  private[dsentric] def _validateFields(path:Path, value:Option[Data], currentState:Option[Data]) =
//    value.flatMap(_prism.getOption).fold(Failures.empty){ indexedData =>
//      val cs = currentState.flatMap(_prism.getOption)
//      _fields.flatMap{kv =>
//        val i = _index.index(kv._1)
//        val v = i.getOption(indexedData)
//        val c = cs.flatMap(i.getOption)
//        kv._2._validate(path :+ Right(kv._1), v, c)
//      }
//    }
//
//  lazy val $sanitize:Data => Data =
//    __fields.foldLeft[Data => Data](Predef.identity[Data]) {
//      case (f, (_, prop:Maybe[Data, IndexedData, _]@unchecked)) if prop._pathValidator.isInternal =>
//        prop.$drop.compose(f)
//      case (f, (_, prop:ContractBase[Data, IndexedData]@unchecked)) =>
//        prop.$sanitize.compose(f)
//      case (f, _) =>
//        f
//    }
//
//  def $create(f:this.type => Data => Data):Data =
//    f(this)(_prism.reverseGet(_empty.empty.reverseGet(())))
//
//  def $dynamic[T](field:String)(implicit prism:Prism[Data, T], strictness:Strictness) = {
//    val prop = new Maybe[Data, IndexedData, T](Validator.empty, None)(prism, strictness)
//    prop._path = _pathPrism.composeLens(at[IndexedData, String, Option[Data]](field))
//    prop
//  }
}


sealed trait Property[T <: Any] extends Struct {
  private[dsentric] def _codec: JCodec[T]
  private var __path: Path = _

  @volatile
  private var _bitmap1:Boolean = false
  private[dsentric] def _nameOverride:Option[String]
  private[dsentric] def _parent: BaseContract

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures

  def _path:Path =
    if (_bitmap1) __path
    else {
      this.synchronized{
        __path = _parent._path :+ Right(_nameOverride.getOrElse {
          _parent._fields.find(p => p._2 == this).get._1
        })
        _bitmap1 = true
      }
      __path
    }
}
trait SubContract extends BaseContract


trait Contract extends BaseContract {
  def _path = Path.empty
}

class Expected[T] private[dsentric]
  (private[dsentric] val _pathValidator:Validator[T],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract,
   private[dsentric] val _codec:JCodec[T])
  extends Property[T] with ExpectedLens[T] {

  private[dsentric] def _isValidType(j:Any) =
    _codec.unapply(j).isDefined

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    value -> currentState match {
      case (None, None) =>
        Failures(path -> ValidationText.EXPECTED_VALUE)
      case (Some(v), c) =>
        _codec.unapply(v).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(_codec.unapply))
        }
      case (None, c) =>
        _pathValidator(path, None, c.flatMap(_codec.unapply))
    }

  def unapply(j: JObject): Option[T] =
    strictGet(j).get

}

class Maybe[T] private[dsentric]
  (private[dsentric] val _pathValidator:Validator[T],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract,
   private[dsentric] val _codec:JCodec[T],
   private[dsentric] val _strictness:Strictness)
  extends Property[T] with MaybeLens[T] {

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:JObject):Option[Option[T]] =
    strictGet(j)

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    value -> currentState match {
      case (Some(v), c)  =>
        _strictness(v, _codec).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(_strictness(_, _codec)))
        }
      case (None, c) =>
        _pathValidator(path, None, c.flatMap(_strictness(_, _codec)))
    }
}

class Default[T] private[dsentric]
  (val _default:T,
   private[dsentric] val _pathValidator:Validator[T],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract,
   private[dsentric] val _codec:JCodec[T],
   private[dsentric] val _strictness:Strictness)
  extends Property[T] with DefaultLens[T] {

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:JObject):Option[T] =
    strictGet(j).map(_.get)


  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    value -> currentState match {
      case (Some(v), c) =>
        _strictness(v, _codec).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(_strictness(_, _codec)))
        }
      case (None, c) =>
        _pathValidator(path, Some(_default), c.flatMap(_strictness(_, _codec)))
    }
}

class \\ private(override private[dsentric] val _pathValidator:Validator[Map[String, Any]],
                override private[dsentric] val _nameOverride:Option[String],
                override private[dsentric] val _parent:BaseContract
               ) extends Expected[Map[String, Any]](_pathValidator, _nameOverride, _parent, DefaultCodecs.mapCodec) with SubContract {

  def this()(implicit parent:BaseContract) =
    this(Validator.empty, None, parent)
  def this(validator:Validator[Map[String, Any]])(implicit parent:BaseContract) =
    this(validator, None, parent)
  def this(name:String, validator:Validator[Map[String, Any]])(implicit parent:BaseContract) =
    this(Validator.empty, Some(name), parent)

}

class \\? private(override private[dsentric] val _pathValidator:Validator[Map[String, Any]],
                  override private[dsentric] val _nameOverride:Option[String],
                  override private[dsentric] val _parent:BaseContract,
                  override private[dsentric] val _strictness:Strictness
                 ) extends Maybe[Map[String, Any]](_pathValidator, _nameOverride, _parent, DefaultCodecs.mapCodec, _strictness) with SubContract {

  def this()(implicit parent:BaseContract, strictness:Strictness) =
    this(Validator.empty, None, parent, strictness)
  def this(validator:Validator[Map[String, Any]])(implicit parent:BaseContract, strictness:Strictness) =
    this(validator, None, parent, strictness)
  def this(name:String, validator:Validator[Map[String, Any]])(implicit parent:BaseContract, strictness:Strictness) =
    this(Validator.empty, Some(name), parent, strictness)
}

class \\! private(override val _default:Map[String, Any],
                  override private[dsentric] val _pathValidator:Validator[Map[String, Any]],
                  override private[dsentric] val _nameOverride:Option[String],
                  override private[dsentric] val _parent:BaseContract,
                  override private[dsentric] val _strictness:Strictness
                 ) extends Default[Map[String, Any]](_default, _pathValidator, _nameOverride, _parent, DefaultCodecs.mapCodec, _strictness) with SubContract {

  def this(default:JObject)(implicit parent:BaseContract, strictness:Strictness) =
    this(default.value, Validator.empty, None, parent, strictness)
  def this(default:JObject, validator:Validator[Map[String, Any]])(implicit parent:BaseContract, strictness:Strictness) =
    this(default.value, validator, None, parent, strictness)
  def this(default:JObject, name:String, validator:Validator[Map[String, Any]])(implicit parent:BaseContract, strictness:Strictness) =
    this(default.value, Validator.empty, Some(name), parent, strictness)
}



//
//abstract class ValueContract[Data, IndexedData, T] private[dsentric](val _pathValidator: Validator[T] = Validator.empty)
//                                                       (implicit private[dsentric] val __prism: Prism[Data, T])
//  extends Property[Data, IndexedData, T] with MapPrism[Data, IndexedData, T]{
//
//  private[dsentric] def _isValidType(j:Data) =
//    __prism.getOption(j).isDefined
//
//  def unapply(j:Data):Option[T] =
//    _getValue(j)
//}

//
//class EmptyProperty[Data, IndexedData, T](implicit val _codec: CodecJson[T]) extends Property[T] {
//
//  private[jsentric] def _nameOverride: Option[String] = None
//  def _pathValidator: Validator[T] = ???
//  def _isValidType(j:Json) = false
//}

