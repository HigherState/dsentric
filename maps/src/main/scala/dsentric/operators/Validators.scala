//package dsentric.operators
//
//import dsentric.contracts.ContractFor
//import dsentric.failure._
//import dsentric.schema._
//import dsentric.{DArray, DCodec, DNull, DObject, Path, Raw, RawObject, StringCodec}
//
//import scala.util.matching.Regex
//
//trait Validators extends ValidatorOps {
//
//
//
//  def >(x:Long): ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//    def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path: Path, value: S, currentState: => Option[S]): ValidationFailures =
//      for {
//        (r, a, b) <- compare(x, value).toList
//        if r >= 0
//      } yield NumericalFailure(contract, path, b, a, "greater than")
//
//    override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//      case n:IntegerDefinition =>
//        n.copy(exclusiveMinimum = Some(x), minimum = None).asInstanceOf[D]
//      case m:MultipleTypeDefinition =>
//        m.remap(definition).asInstanceOf[D]
//    }
//  }
//
//  def >(x:Double): ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          (r, a, b) <- compare(x, value).toList
//          if r >= 0
//        } yield NumericalFailure(contract, path, b, a, "greater than")
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:NumberDefinition =>
//          n.copy(exclusiveMinimum = Some(x), minimum = None).asInstanceOf[D]
//        case m:MultipleTypeDefinition =>
//          m.remap(definition).asInstanceOf[D]
//      }
//    }
//
//  def >=(x:Long): ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          (r, a, b) <- compare(x, value).toList
//          if r > 0
//        } yield NumericalFailure(contract, path, b, a, "greater than or equal to")
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:IntegerDefinition => n.copy(minimum = Some(x), exclusiveMinimum = None).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//
//    }
//
//  def >=(x:Double): ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          (r, a, b) <- compare(x, value).toList
//          if r > 0
//        } yield NumericalFailure(contract, path, b, a, "greater than or equal to")
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:NumberDefinition => n.copy(minimum = Some(x), exclusiveMinimum = None).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//
//    }
//
//  def <(x:Long): ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          (r, a, b) <- compare(x, value).toList
//          if r <= 0
//        } yield NumericalFailure(contract, path, b, a, "less than")
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:IntegerDefinition => n.copy(exclusiveMaximum = Some(x), maximum = None).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//
//    }
//
//  def <(x:Double): ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          (r, a, b) <- compare(x, value).toList
//          if r <= 0
//        } yield NumericalFailure(contract, path, b, a, "less than")
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:NumberDefinition => n.copy(exclusiveMaximum = Some(x), maximum = None).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//    }
//
//  def <=(x:Long): ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          (r, a, b) <- compare(x, value).toList
//          if r < 0
//        } yield NumericalFailure(contract, path, b, a, "less than or equal to")
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:IntegerDefinition => n.copy(maximum = Some(x), exclusiveMaximum = None).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//    }
//
//  def <=(x:Double): ValueValidator[Numeric] =
//    new ValueValidator[Numeric] {
//      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          (r, a, b) <- compare(x, value).toList
//          if r < 0
//        } yield NumericalFailure(contract, path, b, a, "less than or equal to")
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:NumberDefinition => n.copy(maximum = Some(x), exclusiveMaximum = None).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//    }
//  //TODO Doesnt handle delta removal or addition
//  def minLength(x: Int): ValueValidator[Optionable[Length]] =
//    new ValueValidator[Optionable[Length]] {
//      def apply[S >: Optionable[Length], D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures = {
//        getLength(value, currentState)
//          .filter(_ < x)
//          .map(length => MinimumLengthFailure(contract, path, x, length))
//          .toList
//      }
//
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:StringDefinition => n.copy(minLength = Some(x)).asInstanceOf[D]
//        case n:ArrayDefinition => n.copy(minLength = Some(x)).asInstanceOf[D]
//        case o:ObjectDefinition => o.copy(minProperties = Some(x)).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//
//    }
//
//  def maxLength(x: Int): ValueValidator[Optionable[Length]] =
//    new ValueValidator[Optionable[Length]] {
//      def apply[S >: Optionable[Length], D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        getLength(value, currentState)
//          .filter(_ > x)
//          .map(length => MaximumLengthFailure(contract, path, x, length))
//          .toList
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:StringDefinition => n.copy(maxLength = Some(x)).asInstanceOf[D]
//        case n:ArrayDefinition => n.copy(maxLength = Some(x)).asInstanceOf[D]
//        case o:ObjectDefinition => o.copy(maxProperties = Some(x)).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//    }
//
//  def in[T](values:T*)(implicit codec:DCodec[T]): ValueValidator[Optionable[T]] =
//    new ValueValidator[Optionable[T]] {
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:StringDefinition =>
//          n.copy(values.map(codec.apply).map(_.value).toList).asInstanceOf[D]
//        case n:IntegerDefinition =>
//          n.copy(values.map(codec.apply).map(_.value).toList).asInstanceOf[D]
//        case n:NumberDefinition =>
//          n.copy(values.map(codec.apply).map(_.value).toList).asInstanceOf[D]
//        case m:MultipleTypeDefinition =>
//          m.remap(definition).asInstanceOf[D]
//      }
//
//    def apply[S >: Optionable[T], D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//      for {
//        t <- getT[T, S](value).toList
//        if !values.contains(t)
//      } yield InvalidValueFailure(contract, path, t)
//  }
//
//  def nin[T](values:T*)(implicit codec:DCodec[T]): ValueValidator[Optionable[T]] =
//    new ValueValidator[Optionable[T]] {
//
//      def apply[S >: Optionable[T], D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          t <- getT[T, S](value).toList
//          if values.contains(t)
//        } yield InvalidValueFailure(contract, path, t)
//    }
//
//  //maybe change to generic equality
//  def inCaseInsensitive(values:String*): ValueValidator[Optionable[String]] =
//    new ValueValidator[Optionable[String]] {
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case n:StringDefinition => n.copy(values.toList).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//
//      def apply[S >: Optionable[String], D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          t <- getString(value).toList
//          if !values.exists(_.equalsIgnoreCase(t))
//        } yield InvalidValueFailure(contract, path, t)
//  }
//
//  def ninCaseInsensitive(values:String*): ValueValidator[Optionable[String]] =
//    new ValueValidator[Optionable[String]] {
//
//      def apply[S >: Optionable[String], D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          t <- getString(value).toList
//          if values.exists(_.equalsIgnoreCase(t))
//        } yield InvalidValueFailure(contract, path, t)
//    }
//
//  val nonEmpty: ValueValidator[Optionable[Length]] =
//    minLength(1)
//
//  val nonEmptyOrWhiteSpace: ValueValidator[Optionable[String]] =
//    new ValueValidator[Optionable[String]] {
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case s:StringDefinition =>
//          s.copy(minLength = s.minLength.orElse(Some(1)), pattern = s.pattern.orElse(Some("[^\\s]"))).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//
//      def apply[S >: Optionable[String], D <: DObject](contract:ContractFor[D], path: Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          t <- getString(value).toList
//          if t.trim().isEmpty
//        } yield NonEmptyOrWhitespaceFailure(contract, path)
//    }
//
//  def custom[T](f: T => Boolean, message:T => String): ValueValidator[Optionable[T]] =
//    new ValueValidator[Optionable[T]] {
//
//      def apply[S >: Optionable[T], D <: DObject](contract:ContractFor[D], path: Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          t <- getT[T, S](value).toList
//          if !f(t)
//        } yield CustomValidationFailure(contract, path, t, message(t))
//    }
//
//  def regex(r:Regex):ValueValidator[Optionable[String]] =
//    regex(r, s => s"String '$s' fails to match pattern '$r'.")
//
//  def regex(r:Regex, message:String => String):ValueValidator[Optionable[String]] =
//    new ValueValidator[Optionable[String]] {
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case s:StringDefinition =>
//          s.copy(pattern = Some(r.pattern.pattern())).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//
//      def apply[S >: Optionable[String], D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          t <- getString(value).toList
//          if !r.pattern.matcher(t).matches
//        } yield RegexFailure(contract, path, r, t, message(t))
//    }
//
//  val noKeyRemoval:RawValidator[Optionable[Keyable]] =
//    new RawValidator[Optionable[Keyable]] {
//
//      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
//        currentState.fold(ValidationFailures.empty){
//          case r:RawObject@unchecked =>
//            val removed = value.fold(Set.empty[String]){
//              case r2:RawObject@unchecked =>
//                r2.collect{ case (k, DNull) => k}.toSet
//              case _ => Set.empty
//            }.intersect(r.keySet)
//            removed.map(k => KeyRemovalFailure(contract, path, k)).toList
//        }
//    }
//
//
//  def noKeyRemoval[T](implicit D:StringCodec[T]):RawValidator[Optionable[Map[T, Nothing]]] =
//    new RawValidator[Optionable[Map[T, Nothing]]] {
//
//      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures = {
//        currentState.fold(ValidationFailures.empty){
//          case r:RawObject@unchecked =>
//            val removed = value.fold(Set.empty[String]){
//              case r2:RawObject@unchecked =>
//                r2.collect{ case (k, DNull) => k}.toSet
//              case _ => Set.empty
//            }.intersect(r.keySet)
//            removed.map(k => KeyRemovalFailure(contract, path, k)).toList
//        }
//      }
//    }
//
//  def valueValidator[K, V](validators:Validator[V]*)(implicit CV:DCodec[V]):RawValidator[Optionable[Map[K, V]]] =
//    new RawValidator[Optionable[Map[K, V]]] {
//      override def apply[D <: DObject](contract: ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures = {
//        val maybeCurrentState = currentState.collect{ case r:RawObject@unchecked => r}
//        value.fold(ValidationFailures.empty){
//          case r:RawObject@unchecked =>
//            validators.flatMap {
//              case validator:ValueValidator[V]@unchecked =>
//                r.flatMap {
//                  case (k, CV(v)) =>
//                    validator(contract, path \ k, v, maybeCurrentState.flatMap(_.get(k)).flatMap(CV.unapply))
//                  case _ =>
//                    ValidationFailures.empty
//                }
//              case validator:RawValidator[V]@unchecked =>
//                r.flatMap { case (k, v) =>
//                  validator(contract, path \ k, Some(v), maybeCurrentState.flatMap(_.get(k)))
//                }
//            }.toList
//          case _ =>
//            Nil
//        }
//      }
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case s:ObjectDefinition =>
//          val newAdditionalProperties = s.additionalProperties.map{td =>
//            validators.foldLeft(td)((a, e) => e.definition.lift(a).getOrElse(a))
//          }
//          s.copy(additionalProperties = newAdditionalProperties).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//    }
//
//  def keyValidator(r:Regex, message:String):ValueValidator[Optionable[Keyable]] =
//    new ValueValidator[Optionable[Keyable]] {
//
//      override def definition[D <: TypeDefinition]:PartialFunction[D, D] = {
//        case s:ObjectDefinition =>
//          s.copy(propertyNames = Some(StringDefinition(pattern = Some(r.pattern.pattern())))).asInstanceOf[D]
//        case m:MultipleTypeDefinition => m.remap(definition).asInstanceOf[D]
//      }
//
//      def apply[S >: Optionable[Keyable], D <: DObject](contract:ContractFor[D], path:Path, value: S, currentState: => Option[S]): ValidationFailures =
//        for {
//          ct <- getKeyable(value).toList
//          key <- ct.keys.toList if !r.pattern.matcher(key).matches()
//        } yield RegexFailure(contract, path, r, key, message)
//    }
//
//}
//
//trait ValidatorOps {
//
//  protected def getLength[S >: Optionable[Length]](x:S, state:Option[S]): Option[Int] =
//    x match {
//      case Some(v) =>
//        getLength(v, state)
//      case None =>
//        None
//      case v =>
//        getKeyable(v) match {
//          case Some(c) =>
//            Some {
//              state.flatMap(getKeyable)
//                .fold(c.keys.size)(s => getLengthDif(c, s))
//            }
//          case None =>
//            x match {
//              case s: Seq[Any]@unchecked =>
//                Some(s.size)
//              case a: Iterable[?] =>
//                Some(a.size)
//              case s: String =>
//                Some(s.size)
//              case d: DObject =>
//                Some(d.size)
//              case d: DArray =>
//                Some(d.value.size)
//              case _ =>
//                None
//            }
//        }
//    }
//
//  protected def getLengthDif[T](c:Map[String, T], v:Map[String, T]):Int = {
//    val (remove, add) = c.partition(_._2 == DNull)
//    ((v.keySet -- remove.keySet) ++ add.keySet).size
//  }
//
//  protected def getString[S >: Optionable[String]](x:S):Option[String] =
//    x match {
//      case Some(s:String) => Some(s)
//      case s:String => Some(s)
//      case _ =>  None
//    }
//
//  protected def getT[T, S >: Optionable[T]](t:S):Option[T] =
//    t match {
//      case Some(s: T@unchecked) => Some(s)
//      case None => None
//      case s: T@unchecked => Some(s)
//    }
//
//  protected def getKeyable[S >: Optionable[Keyable]](t:S):Option[Map[String, Any]] =
//    t match {
//      case Some(s: Map[String, Any]@unchecked) =>
//        Some(s)
//      case Some(d:DObject) =>
//        Some(d.value)
//      case None =>
//        None
//      case s: Map[String, Any]@unchecked =>
//        Some(s)
//      case d:DObject =>
//        Some(d.value)
//      case _ =>
//        None
//    }
//
//  protected def resolve[S >: Numeric](value:S, target:S):Option[(Int, Number, Number)] =
//    value match {
//      case i:Int =>
//        compare(i, target)
//      case l:Long =>
//        compare(l, target)
//      case f:Float =>
//        compare(f, target)
//      case d:Double =>
//        compare(d, target)
//      case Some(n) =>
//        resolve(value, n)
//      case _ =>
//        None
//    }
//
//  protected def compare[S >: Numeric](value:Long, target:S):Option[(Int, Number, Number)] =
//    target match {
//      case i:Int =>
//        Some((value.compare(i), value, i))
//      case l:Long =>
//        Some((value.compare(l), value, l))
//      case f:Float =>
//        Some((value.toDouble.compare(f), value, f))
//      case d:Double =>
//        Some((value.toDouble.compare(d), value, d))
//      case Some(n) =>
//        compare(value, n)
//      case _ =>
//        None
//    }
//
//  protected def compare[S >: Numeric](value:Double, target:S):Option[(Int, Number, Number)] =
//    target match {
//      case i:Int =>
//        Some((value.compare(i), value, i))
//      case l:Long =>
//        Some((value.compare(l), value, l))
//      case f:Float =>
//        Some((value.compare(f), value, f))
//      case d:Double =>
//        Some((value.compare(d), value, d))
//      case Some(n) =>
//        compare(value, n)
//      case _ =>
//        None
//    }
//}
//
//object Validators extends Validators with ValidatorSanitizers
//
