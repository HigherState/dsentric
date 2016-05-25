package dsentric

import cats.data.{NonEmptyList, Xor}

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
          if (classOf[Property[_]].isAssignableFrom(m.getReturnType) && m.getTypeParameters.isEmpty && m.getParameterTypes.isEmpty) {
            m.invoke(this) match {
              case prop: Property[Any]@unchecked =>
                Some(m.getName -> prop)
              case _ =>
                None
            }
          }
          else None
        }.toVector
        _bitmap0 = true
      }
      __fields
    }

  def \[T](implicit codec:DCodec[T]):Expected[T] =
    new Expected[T](Validators.empty, None, this, codec)

  def \[T](validator:Validator[T])(implicit codec:DCodec[T]):Expected[T] =
    new Expected[T](validator, None, this, codec)

  def \[T](name:String)(implicit codec:DCodec[T]):Expected[T] =
    new Expected[T](Validators.empty, Some(name), this, codec)

  def \[T](name:String, validator:Validator[T])(implicit codec:DCodec[T]):Expected[T] =
    new Expected[T](validator, Some(name), this, codec)

  def \[T](path:Path)(implicit codec:DCodec[T]):Expected[T] = {
    val e = new Expected[T](Validators.empty, None, this, codec)
    e._forceLocalPath(path)
    e
  }

  def \[T](path:Path, validator:Validator[T])(implicit codec:DCodec[T]):Expected[T] = {
    val e = new Expected[T](validator, None, this, codec)
    e._forcePath(_path ++ path)
    e
  }

  def \?[T](implicit codec:DCodec[T], strictness: Strictness):Maybe[T] =
    new Maybe[T](Validators.empty, None, this, codec, strictness)

  def \?[T](validator:Validator[Option[T]])(implicit codec:DCodec[T], strictness: Strictness):Maybe[T] =
    new Maybe[T](validator, None, this, codec, strictness)

  def \?[T](name:String, validator:Validator[Option[T]] = Validators.empty)(implicit codec:DCodec[T], strictness: Strictness):Maybe[T] =
    new Maybe[T](validator, Some(name), this, codec, strictness)

  def \![T](default:T)(implicit codec:DCodec[T], strictness: Strictness):Default[T] =
    new Default[T](default, Validators.empty, None, this, codec, strictness)

  def \![T](default:T, validator:Validator[Option[T]])(implicit codec:DCodec[T], strictness: Strictness):Default[T] =
    new Default[T](default:T, validator, None, this, codec, strictness)

  def \![T](name:String, default:T, validator:Validator[Option[T]] = Validators.empty)(implicit codec:DCodec[T], strictness: Strictness):Default[T] =
    new Default[T](default:T, validator, Some(name), this, codec, strictness)

  def \:[T <: Contract](contract:T)(implicit codec:DCodec[Vector[DObject]]):ExpectedObjectArray[T] =
    new ExpectedObjectArray[T](contract, Validators.empty, None, this, codec)

  def \:[T <: Contract](contract:T, validator:Validator[Vector[DObject]])(implicit codec:DCodec[Vector[DObject]]):ExpectedObjectArray[T] =
    new ExpectedObjectArray[T](contract, validator, None, this, codec)

  def \:[T <: Contract](contract:T, name:String, validator:Validator[Vector[DObject]] = Validators.empty)(implicit codec:DCodec[Vector[DObject]]):ExpectedObjectArray[T]=
    new ExpectedObjectArray[T](contract, validator, Some(name), this, codec)

  def \:?[T <: Contract](contract:T)(implicit codec:DCodec[Vector[DObject]], strictness:Strictness):MaybeObjectArray[T] =
    new MaybeObjectArray[T](contract, Validators.empty, None, this, strictness, codec)

  def \:?[T <: Contract](contract:T, validator:Validator[Option[Vector[DObject]]])(implicit codec:DCodec[Vector[DObject]], strictness:Strictness):MaybeObjectArray[T] =
    new MaybeObjectArray[T](contract, validator, None, this, strictness, codec)

  def \:?[T <: Contract](contract:T, name:String, validator:Validator[Option[Vector[DObject]]] = Validators.empty)(implicit codec:DCodec[Vector[DObject]], strictness:Strictness):MaybeObjectArray[T] =
    new MaybeObjectArray[T](contract, validator, Some(name), this, strictness, codec)



  private[dsentric] def _validateFields(path:Path, value:Map[String, Any], currentState:Option[Map[String, Any]]) =
    _fields.flatMap{kv =>
      val i = PathLensOps.traverse(value, kv._2._localPath)
      val c = currentState.flatMap(c => PathLensOps.traverse(c, kv._2._localPath))
      kv._2._validate(path ++ kv._2._localPath, i, c)
    }


  lazy val $sanitize:DObject => DObject =
    _fields.foldLeft[DObject=> DObject](Predef.identity[DObject]) {
      case (f, (_, prop:Maybe[_]@unchecked)) if prop._pathValidator.isInternal =>
        prop.$drop.compose(f)
      case (f, (_, prop:BaseContract@unchecked)) =>
        prop.$sanitize.compose(f)
      case (f, _) =>
        f
    }

  def $create(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)

  def $dynamic[T](field:String)(implicit codec:DCodec[ T], strictness:Strictness) = {
    val prop = new Maybe[T](Validators.empty, Some(field), this, codec, strictness)
    prop
  }
}


sealed trait Property[T <: Any] extends Struct {
  private[dsentric] def _codec: DCodec[T]
  private var __path: Path = _
  private var __localPath: Path = _

  @volatile
  private var _bitmap1:Boolean = false
  private[dsentric] def _nameOverride:Option[String]
  private[dsentric] def _parent: BaseContract

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures

  private[dsentric] def _forcePath(path:Path) = {
    __path = path
    __localPath = path
    _bitmap1 = true
  }

  private[dsentric] def _forceLocalPath(path:Path) = {
    __path = _parent._path ++ path
    __localPath = path
    _bitmap1 = true
  }

  def _path:Path =
    if (_bitmap1) __path
    else {
      sync
      __path
    }


  def _localPath:Path =
    if (_bitmap1) __localPath
    else {
      sync
      __localPath
    }

  private def sync =
    this.synchronized{
      __localPath =
        Path(_nameOverride.getOrElse {
          _parent._fields.find(p => p._2 == this).get._1
        })
      __path = _parent._path ++ __localPath
      _bitmap1 = true
    }


  def $:DProjection =
    new DProjection(PathLensOps.pathToMap(_path, 1))

}
trait SubContract extends BaseContract


trait Contract extends BaseContract {
  def _path = Path.empty

  def $validate(value:DObject, currentState:DObject):NonEmptyList[(Path, String)] Xor DObject =
    $validate(value, Some(currentState))

  def $validate(value:DObject, currentState:Option[DObject] = None):NonEmptyList[(Path, String)] Xor DObject =
    _validateFields(Path.empty, value.value, currentState.map(_.value)) match {
      case head +: tail =>
        Xor.left(NonEmptyList(head, tail.toList))
      case _ =>
        Xor.right(value)
    }

}

class MatcherUnapply private[dsentric](key: String, matcher:Matcher) extends ApplicativeMatcher[DObject] {
  def unapply(j:DObject):Boolean = {
    j.value
      .get(key)
      .fold(false) { v => matcher(v) }
  }
}

abstract class ContractType(val $typeKey:String, val $keyMatcher:Matcher = ExistenceMatcher) extends Contract {
  val isType = new MatcherUnapply($typeKey, $keyMatcher)

  //TODO create has matcher if possible.
}

class Expected[T] private[dsentric]
  (private[dsentric] val _pathValidator:Validator[T],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract,
   private[dsentric] val _codec:DCodec[T])
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

  def unapply(j: DObject): Option[T] =
    _strictGet(j).map(_.get)

}

class Maybe[T] private[dsentric]
  (private[dsentric] val _pathValidator:Validator[Option[T]],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract,
   private[dsentric] val _codec:DCodec[T],
   private[dsentric] val _strictness:Strictness)
  extends Property[T] with MaybeLens[T] {

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:DObject):Option[Option[T]] =
    _strictGet(j)

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
   private[dsentric] val _pathValidator:Validator[Option[T]],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract,
   private[dsentric] val _codec:DCodec[T],
   private[dsentric] val _strictness:Strictness)
  extends Property[T] with DefaultLens[T] {

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:DObject):Option[T] =
    _strictGet(j).map(_.get)


  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    value -> currentState match {
      case (Some(v), c) =>
        _strictness(v, _codec).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(_strictness(_, _codec)))
        }
      //We assume default is a guaranteed validation success.
      case (None, c) =>
        Vector.empty
    }
}

class EmptyProperty[T](implicit private[dsentric] val _codec:DCodec[T]) extends Property[T] {

  _forcePath(Path.empty)

  def _nameOverride: Option[String] = None

  def _validate(path: Path, value: Option[Any], currentState: Option[Any]): Failures = ???

  def _parent: BaseContract = ???
}

class \\ private(override private[dsentric] val _pathValidator:Validator[DObject],
                 override private[dsentric] val _nameOverride:Option[String],
                 override private[dsentric] val _parent:BaseContract
               ) extends Expected[DObject](_pathValidator, _nameOverride, _parent, DefaultCodecs.dObjectCodec) with SubContract {

  def this()(implicit parent:BaseContract) =
    this(Validators.empty, None, parent)
  def this(validator:Validator[DObject])(implicit parent:BaseContract) =
    this(validator, None, parent)
  def this(name:String, validator:Validator[DObject] = Validators.empty)(implicit parent:BaseContract) =
    this(Validators.empty, Some(name), parent)

  override private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    super._validate(path, value, currentState) match {
      case Failures.empty =>
        value.collect{ case m:Map[String, Any]@unchecked => m }.fold(Failures.empty){v =>
          _validateFields(path, v, currentState.collect{ case m:Map[String, Any]@unchecked => m })
        }
      case failures =>
        failures
    }
}

class \\? private(override private[dsentric] val _pathValidator:Validator[Option[DObject]],
                  override private[dsentric] val _nameOverride:Option[String],
                  override private[dsentric] val _parent:BaseContract,
                  override private[dsentric] val _strictness:Strictness
                 ) extends Maybe[DObject](_pathValidator, _nameOverride, _parent, DefaultCodecs.dObjectCodec, _strictness) with SubContract {

  def this()(implicit parent:BaseContract, strictness:Strictness) =
    this(Validators.empty, None, parent, strictness)
  def this(validator:Validator[Option[DObject]])(implicit parent:BaseContract, strictness:Strictness) =
    this(validator, None, parent, strictness)
  def this(name:String, validator:Validator[DObject] = Validators.empty)(implicit parent:BaseContract, strictness:Strictness) =
    this(Validators.empty, Some(name), parent, strictness)

  override private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    super._validate(path, value, currentState) match {
      case Failures.empty =>
        value.collect{ case m:Map[String, Any]@unchecked => m }.fold(Failures.empty){v =>
          _validateFields(path, v, currentState.collect{ case m:Map[String, Any]@unchecked => m })
        }
      case failures =>
        failures
    }
}

class \\! private(override val _default:DObject,
                  override private[dsentric] val _pathValidator:Validator[Option[DObject]],
                  override private[dsentric] val _nameOverride:Option[String],
                  override private[dsentric] val _parent:BaseContract,
                  override private[dsentric] val _strictness:Strictness
                 ) extends Default[DObject](_default, _pathValidator, _nameOverride, _parent, DefaultCodecs.dObjectCodec, _strictness) with SubContract {

  def this(default:DObject)(implicit parent:BaseContract, strictness:Strictness) =
    this(default, Validators.empty, None, parent, strictness)
  def this(default:DObject, validator:Validator[Option[DObject]])(implicit parent:BaseContract, strictness:Strictness) =
    this(default, validator, None, parent, strictness)
  def this(default:DObject, name:String, validator:Validator[Option[DObject]] = Validators.empty)(implicit parent:BaseContract, strictness:Strictness) =
    this(default, Validators.empty, Some(name), parent, strictness)

  override private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    super._validate(path, value, currentState) match {
      case Failures.empty =>
        value.collect{ case m:Map[String, Any]@unchecked => m }.fold(Failures.empty){v =>
          _validateFields(path, v, currentState.collect{ case m:Map[String, Any]@unchecked => m })
        }
      case failures =>
        failures
    }
}

class ExpectedObjectArray[T <: Contract](private[dsentric] val contract:T,
                                         private[dsentric] val _pathValidator:Validator[Vector[DObject]],
                                         private[dsentric] val _nameOverride:Option[String],
                                         private[dsentric] val _parent:BaseContract,
                                         private[dsentric] val _codec:DCodec[Vector[DObject]]
                         ) extends Property[Vector[DObject]] with ExpectedLens[Vector[DObject]] {
  import Dsentric._

  private[dsentric] def _isValidType(j:Any) =
    _codec.unapply(j).isDefined

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    value -> currentState match {
      case (None, None) =>
        Failures(path -> ValidationText.EXPECTED_VALUE)
      case (Some(v), c) =>
        _codec.unapply(v).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(_codec.unapply)) ++
          p.zipWithIndex.flatMap{case (obj, ind) => contract._validateFields(path \ ind, obj.value, None)}
        }
      case (None, c) =>
        _pathValidator(path, None, c.flatMap(_codec.unapply))
    }

  def unapply(j: DObject): Option[Vector[DObject]] =
    _strictGet(j).map(_.get)
}

class MaybeObjectArray[T <: Contract](private[dsentric] val contract:T,
                                      private[dsentric] val _pathValidator:Validator[Option[Vector[DObject]]],
                                      private[dsentric] val _nameOverride:Option[String],
                                      private[dsentric] val _parent:BaseContract,
                                      private[dsentric] val _strictness:Strictness,
                                      private[dsentric] val _codec:DCodec[Vector[DObject]]
                                      ) extends Property[Vector[DObject]] with MaybeLens[Vector[DObject]] {

  import Dsentric._

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:DObject):Option[Option[Vector[DObject]]] =
    _strictGet(j)

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    value -> currentState match {
      case (Some(v), c)  =>
        _strictness(v, _codec).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
          _pathValidator(path, Some(p), c.flatMap(_strictness(_, _codec))) ++
            p.fold(Failures.empty) {
              _.zipWithIndex.flatMap { case (obj, ind) => contract._validateFields(path \ ind, obj.value, None) }
            }
        }
      case (None, c) =>
        _pathValidator(path, None, c.flatMap(_strictness(_, _codec)))
    }
}
