package dsentric.operators

import dsentric.schema._
import dsentric.{DCodec, DNull, DObject, Path, PathFailures, Raw, RawObject}

import scala.util.matching.Regex

trait Validators extends ValidatorOps{

  val reserved: RawValidator[Option[Nothing]] =
    new RawValidator[Option[Nothing]] {

      def apply(path: Path, value: Option[Raw], currentState: Option[Raw]): PathFailures =
        value.fold(PathFailures.empty)(_ => PathFailures(path -> "Value is reserved and cannot be provided."))
    }

  val required: RawValidator[Nothing] =
    new RawValidator[Nothing] {
      def apply(path: Path, value: Option[Raw], currentState: Option[Raw]): PathFailures =
        if ((value.isEmpty && currentState.isEmpty) || value.exists(_.isInstanceOf[DNull]))
          PathFailures(path -> "Value is required.")
        else PathFailures.empty
    }

  val immutable: RawValidator[Nothing] =
    new RawValidator[Nothing] {

      def apply(path: Path, value: Option[Raw], currentState: Option[Raw]): PathFailures =
        (for {
          v <- value
          cs <- currentState
          if v != cs
        } yield
          path -> "Immutable value cannot be changed."
          ).toVector

    }

  val increment: ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- currentState
          r <- resolve(c,v)
          a <- if (r >= 0) Some(path -> "Value must increase.") else None
        } yield a).toVector
    }

  val decrement: ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- currentState
          r <- resolve(c,v)
          a <- if (r <= 0) Some(path -> "Value must decrease.") else None
        } yield a).toVector

    }

  def >(x:Long): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
    def apply[S >: Numeric](path: Path, value: Option[S], currentState: => Option[S]): PathFailures =
      (for {
        v <- value
        c <- compare(x, v)
        if c >= 0
      } yield path -> s"Value $v is not greater than $x.")
      .toVector

    override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
      case n:IntegerDefinition => n.copy(exclusiveMaximum = Some(x))
      case n:NumberDefinition => n.copy(exclusiveMaximum = Some(x))
      case m:MultipleTypeDefinition => m.remap(definition)
    }
  }

  def >(x:Double): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- compare(x, v)
          if c >= 0
        } yield path -> s"Value $v is not greater than $x.")
          .toVector

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:NumberDefinition => n.copy(exclusiveMaximum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def >=(x:Long): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- compare(x, v)
          if c > 0
        } yield path -> s"Value $v is not greater or equal to $x.")
          .toVector

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:IntegerDefinition => n.copy(maximum = Some(x))
        case n:NumberDefinition => n.copy(maximum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

    }

  def >=(x:Double): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- compare(x, v)
          if c > 0
        } yield path -> s"Value $v is not greater or equal to $x.")
          .toVector

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:NumberDefinition => n.copy(maximum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

    }

  def <(x:Long): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- compare(x, v)
          if c <= 0
        } yield path -> s"Value $v is not less than $x.")
          .toVector

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:IntegerDefinition => n.copy(exclusiveMinimum = Some(x))
        case n:NumberDefinition => n.copy(exclusiveMinimum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

    }

  def <(x:Double): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- compare(x, v)
          if c <= 0
        } yield path -> s"Value $v is not less than $x.")
          .toVector

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:NumberDefinition => n.copy(exclusiveMinimum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def <=(x:Long): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- compare(x, v)
          if c < 0
        } yield path -> s"Value $v is not less than or equal to $x.")
          .toVector

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:IntegerDefinition => n.copy(minimum = Some(x))
        case n:NumberDefinition => n.copy(minimum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def <=(x:Double): ValueValidator[Numeric] =
    new ValueValidator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        (for {
          v <- value
          c <- compare(x, v)
          if c < 0
        } yield path -> s"Value $v is not less than or equal to $x.")
          .toVector

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:NumberDefinition => n.copy(minimum = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }
    }

  def minLength(x: Int): ValueValidator[Optionable[Length]] =
    new ValueValidator[Optionable[Length]] {
      def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value.flatMap(getLength)
          .filter(_ < x)
          .map(v => path -> s"Value must have a length of at least $x.")
          .toVector

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:StringDefinition => n.copy(minLength = Some(x))
        case n:ArrayDefinition => n.copy(minLength = Some(x))
        case o:ObjectDefinition => o.copy(minProperties = Some(x))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

    }

  def maxLength(x: Int): ValueValidator[Optionable[Length]] =
    new ValueValidator[Optionable[Length]] {
      def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value.flatMap(getLength)
          .filter(_ > x)
          .map(v => path -> s"Value must have a length of at most $x.")
          .toVector

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

    def apply[S >: Optionable[T]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
      value
        .flatMap(getT[T, S])
        .filterNot(values.contains)
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  def nin[T](values:T*)(implicit codec:DCodec[T]): ValueValidator[Optionable[T]] =
    new ValueValidator[Optionable[T]] {

      def apply[S >: Optionable[T]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value
          .flatMap(getT[T, S])
          .filter(values.contains)
          .map(v => path -> s"'$v' is not an allowed value.")
          .toVector
    }

  //maybe change to generic equality
  def inCaseInsensitive(values:String*): ValueValidator[Optionable[String]] =
    new ValueValidator[Optionable[String]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case n:StringDefinition => n.copy(values.toList)
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value
          .flatMap(getString)
          .filterNot(v => values.exists(_.equalsIgnoreCase(v.toString)))
          .map(v => path -> s"'$v' is not an allowed value.")
          .toVector
  }

  def ninCaseInsensitive(values:String*): ValueValidator[Optionable[String]] =
    new ValueValidator[Optionable[String]] {

      def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value
          .flatMap(getString)
          .filter(v => values.exists(_.equalsIgnoreCase(v.toString)))
          .map(v => path -> s"'$v' is not an allowed value.")
          .toVector
    }

  // TODO: Rewrite as regex?
  val nonEmpty: ValueValidator[Optionable[Length]] =
    new ValueValidator[Optionable[Length]] {

      val message = "Value must not be empty."
      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case o:ObjectDefinition if o.minProperties.isEmpty => o.copy(minProperties = Some(1))
        case a:ArrayDefinition if a.minLength.isEmpty => a.copy(minLength = Some(1))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value
          .flatMap(getLength)
          .filter(_ == 0)
          .map(v => path -> message)
          .toVector
    }

  // TODO: Rewrite as regex?
  val nonEmptyOrWhiteSpace: ValueValidator[Optionable[String]] =
    new ValueValidator[Optionable[String]] {

      val message = "String must not be empty or whitespace."
      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:StringDefinition =>
          s.copy(minLength = s.minLength.orElse(Some(1)), pattern = s.pattern.orElse(Some("[^\\s]")))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[String]](path: Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value
          .collect {
            case s: String => s
            case Some(s: String) => s
          }
          .filter(_.trim().isEmpty)
          .map(v => path -> message)
          .toVector
    }

  def custom[T](f: T => Boolean, message:String): ValueValidator[Optionable[T]] =
    new ValueValidator[Optionable[T]] {

      def apply[S >: Optionable[T]](path: Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value
          .flatMap(getT[T, S])
          .toVector.flatMap{ s =>
          if (f(s)) Vector.empty
          else Vector(path -> message)
        }
    }

  def regex(r:Regex):ValueValidator[Optionable[String]] =
    regex(r, s"String fails to match pattern '$r'.")

  def regex(r:Regex, message:String):ValueValidator[Optionable[String]] =
    new ValueValidator[Optionable[String]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:StringDefinition =>
          s.copy(pattern = Some(r.pattern.pattern()))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        value
          .flatMap(getString)
          .toVector
          .flatMap{ s =>
            if (r.pattern.matcher(s).matches) Vector.empty
            else Vector(path -> message)
          }
    }

  val noKeyRemoval:RawValidator[Optionable[Map[String,Nothing]]] =
    new RawValidator[Optionable[Map[String, Nothing]]] {
      def apply(path: Path, value: Option[Raw], currentState: Option[Raw]): PathFailures = {
        currentState.fold(PathFailures.empty){
          case r:RawObject@unchecked =>
            val removed = value.fold(Set.empty[String]){
              case r2:RawObject@unchecked =>
                r2.collect{ case (k, _:DNull) => k}.toSet
              case _ => Set.empty
            }.intersect(r.keySet)
            removed.map(k => path \ k -> "Key value cannot be removed.").toVector
        }
      }
    }

  def keyValidator(r:Regex, message:String):ValueValidator[Optionable[DObject]] =
    new ValueValidator[Optionable[DObject]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:ObjectDefinition =>
          s.copy(propertyNames = Some(StringDefinition(pattern = Some(r.pattern.pattern()))))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[DObject]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        for {
          co <- value.toVector
          ct <- getT[DObject, S](co).toVector
          key <- ct.keys.toVector if !r.pattern.matcher(key).matches()
        } yield path \ key -> message
    }

  def keyValidator[T](message:String)(implicit D:DCodec[T]):ValueValidator[Optionable[DObject]] =
    new ValueValidator[Optionable[DObject]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:ObjectDefinition if D.typeDefinition.isInstanceOf[StringDefinition] =>
          s.copy(propertyNames = Some(D.typeDefinition.asInstanceOf[StringDefinition]))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[DObject]](path:Path, value: Option[S], currentState: => Option[S]): PathFailures =
        for {
          co <- value.toVector
          ct <- getT[DObject, S](co).toVector
          key <- ct.keys.toVector if D.unapply(key).isEmpty
        } yield path \ key -> message
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
      case Some(s:Seq[Any] @unchecked) =>
        Some(s.size)
      case Some(a:Iterable[_]) =>
        Some(a.size)
      case Some(s:String) =>
        Some(s.length)
      case _ =>
        None
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

  protected def resolve[S >: Numeric](value:S, target:S):Option[Int] =
    value match {
      case i:Int =>
        compare(i, target)
      case l:Long =>
        compare(l, target)
      case f:Float =>
        compare(f, target)
      case d:Double =>
        compare(d, target)
      case Some(i:Int) =>
        compare(i, target)
      case Some(l:Long) =>
        compare(l, target)
      case Some(f:Float) =>
        compare(f, target)
      case Some(d:Double) =>
        compare(d, target)
      case _ =>
        None
    }

  protected def compare[S >: Numeric](value:Long, target:S):Option[Int] =
    target match {
      case i:Int =>
        Some(value.compare(i))
      case l:Long =>
        Some(value.compare(l))
      case f:Float =>
        Some(value.toDouble.compare(f))
      case d:Double =>
        Some(value.toDouble.compare(d))
      case Some(i:Int) =>
        Some(value.compare(i))
      case Some(l:Long) =>
        Some(value.compare(l))
      case Some(f:Float) =>
        Some(value.toDouble.compare(f))
      case Some(d:Double) =>
        Some(value.toDouble.compare(d))
      case _ =>
        None
    }

  protected def compare[S >: Numeric](value:Double, target:S):Option[Int] =
    target match {
      case i:Int =>
        Some(value.compare(i))
      case l:Long =>
        Some(value.compare(l))
      case f:Float =>
        Some(value.compare(f))
      case d:Double =>
        Some(value.compare(d))
      case Some(i:Int) =>
        Some(value.compare(i))
      case Some(l:Long) =>
        Some(value.compare(l))
      case Some(f:Float) =>
        Some(value.compare(f))
      case Some(d:Double) =>
        Some(value.compare(d))
      case _ =>
        None
    }
}

object Validators extends Validators with ValidatorSanitizers with Sanitizers

