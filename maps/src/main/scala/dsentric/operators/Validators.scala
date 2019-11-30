package dsentric.operators

import dsentric.contracts.ContractFor
import dsentric.failure._
import dsentric.schema._
import dsentric.{DArray, DCodec, DNull, DObject, Path, Raw, RawObject, StringCodec}

import scala.util.matching.Regex

trait Validators extends ValidatorOps {

  val reserved: RawValidator[Option[Nothing]] =
    new RawValidator[Option[Nothing]] {

      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
        value.fold(ValidationFailures.empty)(_ => ValidationFailures(ReservedFailure(contract, path)))
    }

  val required: RawValidator[Nothing] =
    new RawValidator[Nothing] {
      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
        if ((value.isEmpty && currentState.isEmpty) || value.contains(DNull))
          ValidationFailures(ExpectedFailure(contract, path))
        else ValidationFailures.empty
    }

  val immutable: RawValidator[Nothing] =
    new RawValidator[Nothing] {
      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
        for {
          v <- value.toList
          cs <- currentState
          if v != cs
        } yield
          ImmutableFailure(contract, path)
    }

  val increment: ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          c <- currentState
          (r, a, b) <- resolve(c, v)
          if r > 0
        } yield NumericalFailure(contract, path, a, b, "greater than or equal to")
    }

  val decrement: ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          c <- currentState
          (r, a, b) <- resolve(c,v)
          if r < 0
        } yield NumericalFailure(contract, path, a, b, "less than or equal to")

    }

  def >(x:Long): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
    def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path: Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
      for {
        v <- value.toList
        (r, a, b) <- compare(x, v)
        if r >= 0
      } yield NumericalFailure(contract, path, b, a, "greater than")

    override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
      case n:IntegerDefinition => n.copy(exclusiveMaximum = Some(x))
      case n:NumberDefinition => n.copy(exclusiveMaximum = Some(x))
      case m:MultipleTypeDefinition => m.remap(definition)
    }
  }

  def >(x:Double): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          (r, a, b) <- compare(x, v)
          if r >= 0
        } yield NumericalFailure(contract, path, b, a, "greater than")

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:NumberDefinition => n.copy(exclusiveMaximum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def >=(x:Long): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          (r, a, b) <- compare(x, v)
          if r > 0
        } yield NumericalFailure(contract, path, b, a, "greater than or equal to")

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:IntegerDefinition => n.copy(maximum = Some(x))
        case n:NumberDefinition => n.copy(maximum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

    }

  def >=(x:Double): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          (r, a, b) <- compare(x, v)
          if r > 0
        } yield NumericalFailure(contract, path, b, a, "greater than or equal to")

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:NumberDefinition => n.copy(maximum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

    }

  def <(x:Long): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          (r, a, b) <- compare(x, v)
          if r <= 0
        } yield NumericalFailure(contract, path, b, a, "less than")

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:IntegerDefinition => n.copy(exclusiveMinimum = Some(x))
        case n:NumberDefinition => n.copy(exclusiveMinimum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

    }

  def <(x:Double): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          (r, a, b) <- compare(x, v)
          if r <= 0
        } yield NumericalFailure(contract, path, b, a, "less than")

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:NumberDefinition => n.copy(exclusiveMinimum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def <=(x:Long): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          (r, a, b) <- compare(x, v)
          if r < 0
        } yield NumericalFailure(contract, path, b, a, "less than or equal to")

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:IntegerDefinition => n.copy(minimum = Some(x))
        case n:NumberDefinition => n.copy(minimum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def <=(x:Double): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric, D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          v <- value.toList
          (r, a, b) <- compare(x, v)
          if r < 0
        } yield NumericalFailure(contract, path, b, a, "less than or equal to")

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:NumberDefinition => n.copy(minimum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def minLength(x: Int): ValueValidator[Optionable[Length]] =
    new ValueValidator[Optionable[Length]] {
      def apply[S >: Optionable[Length], D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        value -> currentState match {
          case (Some(c:DObject), Some(s:DObject)) =>
            val l = getLengthDif(c.value, s.value)
            if (l < x)
              List(MinimumLengthFailure(contract, path, x, l))
            else
              ValidationFailures.empty
          case (Some(c:Map[String, Nothing]@unchecked), Some(s:Map[String, Nothing]@unchecked)) =>
            val l = getLengthDif(c, s)
            if (l < x)
              List(MinimumLengthFailure(contract, path, x, l))
            else
              ValidationFailures.empty
          case (c, _) =>
            for {
              v <- c.toList
              l <- getLength(v)
              if l < x
            } yield MinimumLengthFailure(contract, path, x, l)


        }


      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:StringDefinition => n.copy(minLength = Some(x))
        case n:ArrayDefinition => n.copy(minLength = Some(x))
        case o:ObjectDefinition => o.copy(minProperties = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

    }

  def maxLength(x: Int): ValueValidator[Optionable[Length]] =
    new ValueValidator[Optionable[Length]] {
      def apply[S >: Optionable[Length], D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        value -> currentState match {
          case (Some(c: DObject), Some(s: DObject)) =>
            val l = getLengthDif(c.value, s.value)
            if (l > x)
              List(MaximumLengthFailure(contract, path, x, l))
            else
              ValidationFailures.empty
          case (Some(c: Map[String, Nothing]@unchecked), Some(s: Map[String, Nothing]@unchecked)) =>
            val l = getLengthDif(c, s)
            if (l > x)
              List(MaximumLengthFailure(contract, path, x, l))
            else
              ValidationFailures.empty
          case (c, _) =>
            for {
              v <- c.toList
              l <- getLength(v)
              if l > x
            } yield MaximumLengthFailure(contract, path, x, l)
        }

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:StringDefinition => n.copy(maxLength = Some(x))
        case n:ArrayDefinition => n.copy(maxLength = Some(x))
        case o:ObjectDefinition => o.copy(maxProperties = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def in[T](values:T*)(implicit codec:DCodec[T]): ValueValidator[Optionable[T]] =
    new ValueValidator[Optionable[T]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:StringDefinition =>
          n.copy(values.map(codec.apply).map(_.value).toList)
        case n:IntegerDefinition =>
          n.copy(values.map(codec.apply).map(_.value).toList)
        case n:NumberDefinition =>
          n.copy(values.map(codec.apply).map(_.value).toList)
        case m:MultipleTypeDefinition =>
          m.remap(definition)
      }

    def apply[S >: Optionable[T], D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
      for {
        s <- value.toList
        t <- getT[T, S](s)
        if !values.contains(t)
      } yield InvalidValueFailure(contract, path, t)
  }

  def nin[T](values:T*)(implicit codec:DCodec[T]): ValueValidator[Optionable[T]] =
    new ValueValidator[Optionable[T]] {

      def apply[S >: Optionable[T], D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          s <- value.toList
          t <- getT[T, S](s)
          if values.contains(t)
        } yield InvalidValueFailure(contract, path, t)
    }

  //maybe change to generic equality
  def inCaseInsensitive(values:String*): ValueValidator[Optionable[String]] =
    new ValueValidator[Optionable[String]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:StringDefinition => n.copy(values.toList)
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[String], D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          s <- value.toList
          t <- getString(s)
          if !values.exists(_.equalsIgnoreCase(t))
        } yield InvalidValueFailure(contract, path, t)
  }

  def ninCaseInsensitive(values:String*): ValueValidator[Optionable[String]] =
    new ValueValidator[Optionable[String]] {

      def apply[S >: Optionable[String], D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          s <- value.toList
          t <- getString(s)
          if values.exists(_.equalsIgnoreCase(t))
        } yield InvalidValueFailure(contract, path, t)
    }

  val nonEmpty: ValueValidator[Optionable[Length]] =
    minLength(1)

  val nonEmptyOrWhiteSpace: ValueValidator[Optionable[String]] =
    new ValueValidator[Optionable[String]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:StringDefinition =>
          s.copy(minLength = s.minLength.orElse(Some(1)), pattern = s.pattern.orElse(Some("[^\\s]")))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[String], D <: DObject](contract:ContractFor[D], path: Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          s <- value.toList
          t <- getString(s)
          if t.trim().isEmpty
        } yield NonEmptyOrWhitespaceFailure(contract, path)
    }

  def custom[T](f: T => Boolean, message:T => String): ValueValidator[Optionable[T]] =
    new ValueValidator[Optionable[T]] {

      def apply[S >: Optionable[T], D <: DObject](contract:ContractFor[D], path: Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          s <- value.toList
          t <- getT[T, S](s)
          if !f(t)
        } yield CustomValidationFailure(contract, path, t, message(t))
    }

  def regex(r:Regex):ValueValidator[Optionable[String]] =
    regex(r, s => s"String '$s' fails to match pattern '$r'.")

  def regex(r:Regex, message:String => String):ValueValidator[Optionable[String]] =
    new ValueValidator[Optionable[String]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:StringDefinition =>
          s.copy(pattern = Some(r.pattern.pattern()))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[String], D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          s <- value.toList
          t <- getString(s)
          if !r.pattern.matcher(t).matches
        } yield RegexFailure(contract, path, r, t, message(t))
    }

  val noKeyRemoval:RawValidator[Optionable[Keyable]] =
    new RawValidator[Optionable[Keyable]] {

      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures =
        currentState.fold(ValidationFailures.empty){
          case r:RawObject@unchecked =>
            val removed = value.fold(Set.empty[String]){
              case r2:RawObject@unchecked =>
                r2.collect{ case (k, DNull) => k}.toSet
              case _ => Set.empty
            }.intersect(r.keySet)
            removed.map(k => KeyRemovalFailure(contract, path, k)).toList
        }
    }


  def noKeyRemoval[T](implicit D:StringCodec[T]):RawValidator[Optionable[Map[T, Nothing]]] =
    new RawValidator[Optionable[Map[T, Nothing]]] {

      def apply[D <: DObject](contract:ContractFor[D], path: Path, value: Option[Raw], currentState: Option[Raw]): ValidationFailures = {
        currentState.fold(ValidationFailures.empty){
          case r:RawObject@unchecked =>
            val removed = value.fold(Set.empty[String]){
              case r2:RawObject@unchecked =>
                r2.collect{ case (k, DNull) => k}.toSet
              case _ => Set.empty
            }.intersect(r.keySet)
            removed.map(k => KeyRemovalFailure(contract, path, k)).toList
        }
      }
    }

  def keyValidator(r:Regex, message:String):ValueValidator[Optionable[Keyable]] =
    new ValueValidator[Optionable[Keyable]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:ObjectDefinition =>
          s.copy(propertyNames = Some(StringDefinition(pattern = Some(r.pattern.pattern()))))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[Keyable], D <: DObject](contract:ContractFor[D], path:Path, value: Option[S], currentState: => Option[S]): ValidationFailures =
        for {
          co <- value.toList
          ct <- getKeyable(co).toList
          key <- ct.keys.toList if !r.pattern.matcher(key).matches()
        } yield RegexFailure(contract, path, r, key, message)
    }
}

trait ValidatorOps {

  protected def getLength[S >: Optionable[Length]](x:S): Option[Int] =
    x match {
      case s:Seq[Any] @unchecked =>
        Some(s.size)
      case a:Iterable[_] =>
        Some(a.size)
      case s:String =>
        Some(s.size)
      case d:DObject =>
        Some(d.size)
      case d:DArray =>
        Some(d.value.size)
      case Some(s) =>
        getLength(s)
      case _ =>
        None
    }

  protected def getLengthDif[T](c:Map[String, T], v:Map[String, T]):Int = {
    val (remove, add) = c.partition(_._2 == DNull)
    ((v.keySet -- remove.keySet) ++ add.keySet).size
  }

  protected def getString[S >: Optionable[String]](x:S):Option[String] =
    x match {
      case Some(s:String) => Some(s)
      case s:String => Some(s)
      case _ =>  None
    }

  protected def getT[T, S >: Optionable[T]](t:S):Option[T] =
    t match {
      case Some(s: T@unchecked) => Some(s)
      case None => None
      case s: T@unchecked => Some(s)
    }

  protected def getKeyable[S >: Optionable[Keyable]](t:S):Option[Map[String, Any]] =
    t match {
      case Some(s: Map[String, Any]@unchecked) =>
        Some(s)
      case Some(d:DObject) =>
        Some(d.value)
      case None =>
        None
      case s: Map[String, Any]@unchecked =>
        Some(s)
      case d:DObject =>
        Some(d.value)
    }

  protected def resolve[S >: Numeric](value:S, target:S):Option[(Int, Number, Number)] =
    value match {
      case i:Int =>
        compare(i, target)
      case l:Long =>
        compare(l, target)
      case f:Float =>
        compare(f, target)
      case d:Double =>
        compare(d, target)
      case Some(n) =>
        resolve(value, n)
      case _ =>
        None
    }

  protected def compare[S >: Numeric](value:Long, target:S):Option[(Int, Number, Number)] =
    target match {
      case i:Int =>
        Some((value.compare(i), value, i))
      case l:Long =>
        Some((value.compare(l), value, l))
      case f:Float =>
        Some((value.toDouble.compare(f), value, f))
      case d:Double =>
        Some((value.toDouble.compare(d), value, d))
      case Some(n) =>
        compare(value, n)
      case _ =>
        None
    }

  protected def compare[S >: Numeric](value:Double, target:S):Option[(Int, Number, Number)] =
    target match {
      case i:Int =>
        Some((value.compare(i), value, i))
      case l:Long =>
        Some((value.compare(l), value, l))
      case f:Float =>
        Some((value.compare(f), value, f))
      case d:Double =>
        Some((value.compare(d), value, d))
      case Some(n) =>
        compare(value, n)
      case _ =>
        None
    }
}

object Validators extends Validators with ValidatorSanitizers

