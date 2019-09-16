package dsentric

import cats.data.NonEmptyList

private[dsentric] sealed trait Struct {

  def apply[R](f:this.type => R):R = f(this)
  def _path:Path

}

private[dsentric] sealed trait BaseContract[D <: DObject] extends Struct { self =>
  private var __fields:Vector[(String, Property[D, Any])] = _
  @volatile
  private var _bitmap0:Boolean = false

  //Used for nested new object
  protected implicit def selfRef:BaseContract[DObject] =
    this.asInstanceOf[BaseContract[DObject]]

  def _fields: Vector[(String, Property[D, Any])] =
    if (_bitmap0) __fields
    else {
      this.synchronized{
        __fields = this.getClass.getMethods.flatMap { m =>
          if (classOf[Property[D, _]].isAssignableFrom(m.getReturnType) && m.getTypeParameters.isEmpty && m.getParameterTypes.isEmpty) {
            m.invoke(this) match {
              case prop: Property[D, Any]@unchecked =>
                Some(m.getName -> prop)
              case result =>
                None
            }
          }
          else None
        }.toVector
        _bitmap0 = true
      }
      __fields
    }

  def injectDefaults(d: D): DObject = {
    val dataToInject: Map[String, Data] =
      _fields.collect[Option[(String, Data)], Vector[Option[(String, Data)]]]{
        case (n, p:Default[D, _]@unchecked) =>
          val trueName = p._nameOverride.getOrElse(n)
          Some(trueName -> ForceWrapper.data(d.get(trueName).flatMap{_.decode(p._codec)}.getOrElse(p._default)))
        case (n, p:SubContractFor[_]@unchecked) =>
          val trueName = p._nameOverride.getOrElse(n)
          p.injectDefaults(d.get(trueName).getOrElse(DObject.empty).asInstanceOf[p.Out]) match {
            case DObject.empty => None
            case x => Some(trueName -> x)
          }
      }.flatten.toMap
    d ++ dataToInject
  }

  def _keys:Set[String] = _fields.map(_._1).toSet

  def \[T](implicit codec:DCodec[T]):Expected[D, T] =
    new Expected[D, T](Validators.empty, None, this, codec)

  def \[T](validator:Validator[T])(implicit codec:DCodec[T]):Expected[D, T] =
    new Expected[D, T](validator, None, this, codec)

  def \[T](name:String)(implicit codec:DCodec[T]):Expected[D, T] =
    new Expected[D, T](Validators.empty, Some(name), this, codec)

  def \[T](name:String, validator:Validator[T])(implicit codec:DCodec[T]):Expected[D, T] =
    new Expected[D, T](validator, Some(name), this, codec)

  def \[T](path:Path)(implicit codec:DCodec[T]):Expected[D, T] = {
    val e = new Expected[D, T](Validators.empty, None, this, codec)
    e._forceLocalPath(path)
    e
  }

  def \[T](path:Path, validator:Validator[T])(implicit codec:DCodec[T]):Expected[D, T] = {
    val e = new Expected[D, T](validator, None, this, codec)
    e._forcePath(_path ++ path)
    e
  }

  def \?[T](implicit codec:DCodec[T], strictness: Strictness):Maybe[D, T] =
    new Maybe[D, T](Validators.empty, None, this, codec, strictness)

  def \?[T](validator:Validator[Option[T]])(implicit codec:DCodec[T], strictness: Strictness):Maybe[D, T] =
    new Maybe[D, T](validator, None, this, codec, strictness)

  def \?[T](name:String, validator:Validator[Option[T]] = Validators.empty)(implicit codec:DCodec[T], strictness: Strictness):Maybe[D, T] =
    new Maybe[D, T](validator, Some(name), this, codec, strictness)

  def \![T](default:T)(implicit codec:DCodec[T], strictness: Strictness):Default[D, T] =
    new Default[D, T](default, Validators.empty, None, this, codec, strictness)

  def \![T](default:T, validator:Validator[Option[T]])(implicit codec:DCodec[T], strictness: Strictness):Default[D, T] =
    new Default[D, T](default:T, validator, None, this, codec, strictness)

  def \![T](name:String, default:T, validator:Validator[Option[T]] = Validators.empty)(implicit codec:DCodec[T], strictness: Strictness):Default[D, T] =
    new Default[D, T](default:T, validator, Some(name), this, codec, strictness)

  //TODO Contract for support
  def \:[T <: Contract](contract:T)(implicit codec:DCodec[Vector[DObject]]):ExpectedObjectArray[DObject, T] =
    new ExpectedObjectArray[DObject, T](contract, Validators.empty, None, this.asInstanceOf[BaseContract[DObject]], codec)

  def \:[T <: Contract](contract:T, validator:Validator[Vector[DObject]])(implicit codec:DCodec[Vector[DObject]]):ExpectedObjectArray[DObject, T] =
    new ExpectedObjectArray[DObject, T](contract, validator, None, this.asInstanceOf[BaseContract[DObject]], codec)

  def \:[T <: Contract](contract:T, name:String, validator:Validator[Vector[DObject]] = Validators.empty)(implicit codec:DCodec[Vector[DObject]]):ExpectedObjectArray[DObject, T]=
    new ExpectedObjectArray[DObject, T](contract, validator, Some(name), this.asInstanceOf[BaseContract[DObject]], codec)

  def \:?[T <: Contract](contract:T)(implicit codec:DCodec[Vector[DObject]], strictness:Strictness):MaybeObjectArray[DObject, T] =
    new MaybeObjectArray[DObject, T](contract, Validators.empty, None, this.asInstanceOf[BaseContract[DObject]], strictness, codec)

  def \:?[T <: Contract](contract:T, validator:Validator[Option[Vector[DObject]]])(implicit codec:DCodec[Vector[DObject]], strictness:Strictness):MaybeObjectArray[DObject, T] =
    new MaybeObjectArray[DObject, T](contract, validator, None, this.asInstanceOf[BaseContract[DObject]], strictness, codec)

  def \:?[T <: Contract](contract:T, name:String, validator:Validator[Option[Vector[DObject]]] = Validators.empty)(implicit codec:DCodec[Vector[DObject]], strictness:Strictness):MaybeObjectArray[DObject, T] =
    new MaybeObjectArray[DObject, T](contract, validator, Some(name), this.asInstanceOf[BaseContract[DObject]], strictness, codec)

  def \:![T <: Contract](contract:T, default:Vector[DObject])(implicit codec:DCodec[Vector[DObject]], strictness: Strictness):DefaultObjectArray[DObject, T] =
    new DefaultObjectArray[DObject, T](default, contract, Validators.empty, None, this.asInstanceOf[BaseContract[DObject]], strictness, codec)

  def \:![T <: Contract](contract:T, default:Vector[DObject], validator:Validator[Option[Vector[DObject]]])(implicit codec:DCodec[Vector[DObject]], strictness: Strictness):DefaultObjectArray[DObject, T] =
    new DefaultObjectArray[DObject, T](default, contract, validator, None, this.asInstanceOf[BaseContract[DObject]], strictness, codec)

  def \:![T <: Contract](contract:T, name:String, default:Vector[DObject], validator:Validator[Option[Vector[DObject]]] = Validators.empty)(implicit codec:DCodec[Vector[DObject]], strictness: Strictness):DefaultObjectArray[DObject, T] =
    new DefaultObjectArray[DObject, T](default, contract, validator, Some(name), this.asInstanceOf[BaseContract[DObject]], strictness, codec)


  private[dsentric] def _validateFields(path:Path, value:Map[String, Any], currentState:Option[Map[String, Any]]) =
    _fields.flatMap{kv =>
      val i = PathLensOps.traverse(value, kv._2._localPath)
      val c = currentState.flatMap(c => PathLensOps.traverse(c, kv._2._localPath))
      kv._2._validate(path ++ kv._2._localPath, i, c)
    }


  lazy val $sanitize:D => D =
    _fields.foldLeft[D => D](Predef.identity[D]) {
      case (f, (_, prop:Maybe[D, _]@unchecked)) if prop._pathValidator.mask.nonEmpty =>
        d:D =>
          if (d.contains(prop._path))
            d.+\(prop._path -> ForceWrapper.data(prop._pathValidator.mask.get)).asInstanceOf[D]
          else
            d
      case (f, (_, prop:Expected[D, _]@unchecked)) if prop._pathValidator.mask.nonEmpty =>
        d:D => d.+\(prop._path -> ForceWrapper.data(prop._pathValidator.mask.get)).asInstanceOf[D]
      case (f, (_, prop:Default[D, _]@unchecked)) if prop._pathValidator.mask.nonEmpty =>
        d:D => d.+\(prop._path -> ForceWrapper.data(prop._pathValidator.mask.get)).asInstanceOf[D]
      case (f, (_, prop:Maybe[D, _]@unchecked)) if prop._pathValidator.isInternal =>
        prop.$drop.compose(f)
      case (f, (_, prop:BaseContract[D]@unchecked)) =>
        prop.$sanitize.compose(f)
      case (f, _) =>
        f
    }

  def $modify(d:D)(f:this.type => D => D):D =
    f(this)(d)

  def $dynamic[T](field:String)(implicit codec:DCodec[T], strictness:Strictness):Maybe[D, T] = {
    val prop = new Maybe[D, T](Validators.empty, Some(field), this, codec, strictness)
    prop
  }

  def $$(projection:DProjection):DProjection =
    projection.nest(this._path)

  def $$(paths:Path*):DProjection =
    DProjection(paths:_*).nest(this._path)


  class \\ private(override private[dsentric] val _pathValidator:Validator[DObject],
                   override private[dsentric] val _nameOverride:Option[String],
                   override private[dsentric] val _codec:DCodec[DObject]
                  ) extends Expected[D, DObject](_pathValidator, _nameOverride, self, _codec) with SubContractFor[D] {

    def this()(implicit codec:DCodec[DObject]) =
      this(Validators.empty, None, codec)
    def this(validator:Validator[DObject])(implicit codec:DCodec[DObject]) =
      this(validator, None, codec)
    def this(name:String, validator:Validator[DObject] = Validators.empty)(implicit codec:DCodec[DObject]) =
      this(Validators.empty, Some(name), codec)

    override private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
      super._validate(path, value, currentState) match {
        case Failures.empty =>
          val valueMap = value.collect{ case m:Map[String, Any]@unchecked => m }
          val stateMap =
            currentState.collect{ case m:Map[String, Any]@unchecked => m }
          if (stateMap.isEmpty && valueMap.isEmpty)
            Failures.empty
          else
            _validateFields(path, valueMap.getOrElse(Map.empty), stateMap)
        case failures =>
          failures
      }
  }

  class \\? private(override private[dsentric] val _pathValidator:Validator[Option[DObject]],
                    override private[dsentric] val _nameOverride:Option[String],
                    override private[dsentric] val _strictness:Strictness,
                    override private[dsentric] val _codec:DCodec[DObject]
                   ) extends Maybe[D, DObject](_pathValidator, _nameOverride, self, _codec, _strictness) with SubContractFor[D] {

    def this()(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(Validators.empty, None, strictness, codec)
    def this(validator:Validator[Option[DObject]])(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(validator, None, strictness, codec)
    def this(name:String, validator:Validator[DObject] = Validators.empty)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(Validators.empty, Some(name), strictness, codec)

    override private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
      super._validate(path, value, currentState) match {
        case Failures.empty =>
          //TODO code duplicate tidy
          val valueMap = value.collect{ case m:Map[String, Any]@unchecked => m }
          val stateMap =
            currentState.collect{ case m:Map[String, Any]@unchecked => m }
          if (stateMap.isEmpty && valueMap.isEmpty)
            Failures.empty
          else
            _validateFields(path, valueMap.getOrElse(Map.empty), stateMap)
        case failures =>
          failures
      }
  }

  class \\! private(override val _default:DObject,
                    override private[dsentric] val _pathValidator:Validator[Option[DObject]],
                    override private[dsentric] val _nameOverride:Option[String],
                    override private[dsentric] val _strictness:Strictness,
                    override private[dsentric] val _codec:DCodec[DObject]
                   ) extends Default[D, DObject](_default, _pathValidator, _nameOverride, self, _codec, _strictness) with SubContractFor[D] {

    def this(default:DObject)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(default, Validators.empty, None, strictness, codec)
    def this(default:DObject, validator:Validator[Option[DObject]])(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(default, validator, None, strictness, codec)
    def this(default:DObject, name:String, validator:Validator[Option[DObject]] = Validators.empty)(implicit strictness:Strictness, codec:DCodec[DObject]) =
      this(default, Validators.empty, Some(name), strictness, codec)

    override private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
      super._validate(path, value, currentState) match {
        case Failures.empty =>
          val valueMap = value.collect{ case m:Map[String, Any]@unchecked => m }
          val stateMap =
            currentState.collect{ case m:Map[String, Any]@unchecked => m }
          if (stateMap.isEmpty && valueMap.isEmpty)
            Failures.empty
          else
            _validateFields(path, valueMap.getOrElse(Map.empty), stateMap)
        case failures =>
          failures
      }
  }

}


sealed trait Property[D <: DObject, T <: Any] extends Struct {
  private[dsentric] def _codec: DCodec[T]
  private var __path: Path = _
  private var __localPath: Path = _

  @volatile
  private var _bitmap1:Boolean = false
  private[dsentric] def _nameOverride:Option[String]
  private[dsentric] def _parent: BaseContract[D]
  private[dsentric] def _pathValidator:Validator[_]

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures

  @inline
  protected def _validateCheck[T](path:Path, value:Option[Any], currentState:Option[Any], validator:Validator[T], failures: => Failures):Failures =
    if (validator.removalDenied)
      value.toVector.flatMap {
        case m:Map[String, Any]@unchecked =>
          m.collect {
            case (key, _:DNull) =>
              (path \ key, "Removing element is not allowed")
          }.toVector
        case _ =>
          None
      } ++ failures
    else failures

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
          _parent._fields.find(p => p._2 == this).getOrElse{
            throw UninitializedFieldError(s"Unable to initialize property field from fields: ${_parent._fields.map(_._1).mkString(",")}")
          }._1
        })
      __path = _parent._path ++ __localPath
      _bitmap1 = true
    }


  def $:DProjection =
    new DProjection(PathLensOps.pathToMap(_path, 1))
}

trait SubContractFor[D <: DObject] extends BaseContract[D] { type Out = D }

trait ContractFor[D <: DObject] extends BaseContract[D] { self =>

  def _path:Path = Path.empty

  def $validate(value:D):NonEmptyList[(Path, String)] Either D =
    _validateFields(Path.empty, value.value, None) match {
      case head +: tail =>
        Left(NonEmptyList(head, tail.toList))
      case _ =>
        Right(value)
    }

  def $validate(value:DObject, currentState:D):NonEmptyList[(Path, String)] Either DObject =
    _validateFields(Path.empty, value.value, Some(currentState.value)) match {
      case head +: tail =>
        Left(NonEmptyList(head, tail.toList))
      case _ =>
        Right(value)
    }

  def $validate(value:D, maybeState:Option[D]):NonEmptyList[(Path, String)] Either D =
    _validateFields(Path.empty, value.value, maybeState.map(_.value)) match {
      case head +: tail =>
        Left(NonEmptyList(head, tail.toList))
      case _ =>
        Right(value)
    }

}

trait SubContract extends SubContractFor[DObject]

trait Contract extends ContractFor[DObject] {
  def $create(f:this.type => DObject => DObject):DObject =
    f(this)(DObject.empty)
}

class MatcherUnapply private[dsentric](key: String, matcher:Matcher) extends ApplicativeMatcher[DObject] {
  def unapply(j:DObject):Boolean = {
    j.value
      .get(key)
      .fold(false) { v => matcher(v) }
  }
}

abstract class ContractTypeFor[D <: DObject](val $typeKey:String, val $keyMatcher:Matcher = ExistenceMatcher) extends ContractFor[D] {
  val isType = new MatcherUnapply($typeKey, $keyMatcher)
}

abstract class ContractType(override val $typeKey:String, override val $keyMatcher:Matcher = ExistenceMatcher) extends ContractTypeFor[DObject]($typeKey, $keyMatcher) {
  //TODO create has matcher if possible.
  def $create():DObject = {
    val seed = $keyMatcher match {
      case ExistenceMatcher =>
        true
      case v:ValueMatcher[_]@unchecked =>
        v.default
    }
    new DObjectInst(Map($typeKey -> seed))
  }

  def $create(f:this.type => DObject => DObject):DObject =
    f(this)($create())
}

class Expected[D <: DObject, T] private[dsentric]
  (private[dsentric] val _pathValidator:Validator[T],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract[D],
   private[dsentric] val _codec:DCodec[T])
  extends Property[D, T] with ExpectedLens[D, T] {

  private[dsentric] def _isValidType(j:Any) =
    _codec.unapply(j).isDefined

  def $validateValue(value:T):Vector[String] =
    _codec.unapply(value).fold(Vector(ValidationText.EXPECTED_VALUE)){ p =>
      _pathValidator(Path.empty, Some(p), None).map(_._2)
    }

  val $delta:PatternMatcher[D, Option[T]] =
    new PatternMatcher(_strictDeltaGet)

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    _validateCheck(path, value, currentState, _pathValidator,
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
    )

  def unapply(j: D): Option[T] =
    _strictGet(j).flatten
}

class Maybe[D <: DObject, T] private[dsentric]
  (private[dsentric] val _pathValidator:Validator[Option[T]],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract[D],
   private[dsentric] val _codec:DCodec[T],
   private[dsentric] val _strictness:Strictness)
  extends Property[D, T] with MaybeLens[D, T] {

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:D):Option[Option[T]] =
    _strictGet(j)

  def $validateValue(value:T):Vector[String] =
    _strictness(value, _codec).fold(Vector(ValidationText.EXPECTED_VALUE)){ p =>
      _pathValidator(Path.empty, Some(p), None).map(_._2)
    }

  val $delta:PatternMatcher[D, Option[Option[T]]] =
    new PatternMatcher(_strictDeltaGet(_).map(_.map(_.toOption)))

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    _validateCheck(path, value, currentState, _pathValidator,
      value -> currentState match {
        case (Some(_:DNull), Some(_)) =>
          Vector.empty
        case (Some(v), c)  =>
          _strictness(v, _codec).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
            _pathValidator(path, Some(p), c.flatMap(_strictness(_, _codec)))
          }
        case (None, c) =>
          _pathValidator(path, None, c.flatMap(_strictness(_, _codec)))
      }
    )
}

class Default[D <: DObject, T] private[dsentric]
  (val _default:T,
   private[dsentric] val _pathValidator:Validator[Option[T]],
   private[dsentric] val _nameOverride:Option[String],
   private[dsentric] val _parent:BaseContract[D],
   private[dsentric] val _codec:DCodec[T],
   private[dsentric] val _strictness:Strictness)
  extends Property[D, T] with DefaultLens[D, T] {

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:D):Option[T] =
    _strictGet(j).flatten

  def $validateValue(value:T):Vector[String] =
    _strictness(value, _codec).fold(Vector(ValidationText.EXPECTED_VALUE)){ p =>
      _pathValidator(Path.empty, Some(p), None).map(_._2)
    }

  val $delta:PatternMatcher[D, Option[Option[T]]] =
    new PatternMatcher(_strictDeltaGet(_).map(_.map(_.toOption)))

  val $deltaDefault:PatternMatcher[D, Option[T]] =
    new PatternMatcher(_strictDeltaGet(_).map(_.map(_.getValue)))

  private[dsentric] def _validate(path:Path, value:Option[Any], currentState:Option[Any]):Failures =
    _validateCheck(path, value, currentState, _pathValidator,
      value -> currentState match {
        //Supports null for delta
        case (Some(_:DNull), Some(_)) =>
          Vector.empty
        case (Some(v), c) =>
          _strictness(v, _codec).fold(Failures(path -> ValidationText.UNEXPECTED_TYPE)){ p =>
            _pathValidator(path, Some(p), c.flatMap(_strictness(_, _codec)))
          }
        //We assume default is a guaranteed validation success.
        case (None, c) =>
          Vector.empty
      }
    )
}

class EmptyProperty[T](implicit private[dsentric] val _codec:DCodec[T]) extends Property[Nothing, T] {

  _forcePath(Path.empty)


  private[dsentric] def _pathValidator = Validators.empty

  def _nameOverride: Option[String] = None

  def _validate(path: Path, value: Option[Any], currentState: Option[Any]): Failures = ???

  def _parent: BaseContract[Nothing] = ???
}

class ExpectedObjectArray[D <: DObject, T <: ContractFor[D]](private[dsentric] val contract:T,
                                                             private[dsentric] val _pathValidator:Validator[Vector[D]],
                                                             private[dsentric] val _nameOverride:Option[String],
                                                             private[dsentric] val _parent:BaseContract[DObject],
                                                             private[dsentric] val _codec:DCodec[Vector[D]]
                                                            ) extends Property[DObject, Vector[D]] with ExpectedLens[DObject, Vector[D]] {
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

  def unapply(j: DObject): Option[Vector[D]] =
    _strictGet(j).map(_.get)
}

class MaybeObjectArray[D <: DObject, T <: ContractFor[D]](private[dsentric] val contract:T,
                                                          private[dsentric] val _pathValidator:Validator[Option[Vector[D]]],
                                                          private[dsentric] val _nameOverride:Option[String],
                                                          private[dsentric] val _parent:BaseContract[DObject],
                                                          private[dsentric] val _strictness:Strictness,
                                                          private[dsentric] val _codec:DCodec[Vector[D]]
                                                         ) extends Property[DObject, Vector[D]] with MaybeLens[DObject, Vector[D]] {

  import Dsentric._

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:DObject):Option[Option[Vector[D]]] =
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

class DefaultObjectArray[D <: DObject, T <: ContractFor[D]](override val _default:Vector[D],
                                                            private[dsentric] val contract:T,
                                                            private[dsentric] val _pathValidator:Validator[Option[Vector[D]]],
                                                            private[dsentric] val _nameOverride:Option[String],
                                                            private[dsentric] val _parent:BaseContract[DObject],
                                                            private[dsentric] val _strictness:Strictness,
                                                            private[dsentric] val _codec:DCodec[Vector[D]]
                                                           ) extends Property[DObject, Vector[D]] with DefaultLens[DObject, Vector[D]] {

  import Dsentric._

  private[dsentric] def _isValidType(j:Any) =
    _strictness(j, _codec).isDefined

  def unapply(j:DObject):Option[Vector[D]] =
    _strictGet(j).flatten

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
        Vector.empty
    }
}

class PatternMatcher[D <: DObject, T](unapplyFunction:Function[D, Option[T]]) {
  def unapply(j:D):Option[T] =
    unapplyFunction(j)
}
