package dsentric

import scala.util.matching.Regex
import Dsentric._
import PessimisticCodecs._
import dsentric.schema.{ArrayDefinition, ByRefDefinition, IntegerDefinition, MultipleTypeDefinition, NumberDefinition, ObjectDefinition, Schema, StringDefinition, TypeDefinition}

trait Validator[+T] {

  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]): Failures

  def &&[S >: T] (v:Validator[S]):Validator[S] = AndValidator(this, v)

  def ||[S >: T] (v:Validator[S]):Validator[S] = OrValidator(this, v)

  def apply[A <: TypeDefinition](t:A, objDefs:Vector[ObjectDefinition], forceNested:Boolean):(A, Vector[ObjectDefinition]) =
    withObjsDefinition(forceNested).lift(t -> objDefs).getOrElse(t -> objDefs).asInstanceOf[(A, Vector[ObjectDefinition])]

  def withObjsDefinition(forceNested:Boolean):PartialFunction[(TypeDefinition, Vector[ObjectDefinition]), (TypeDefinition, Vector[ObjectDefinition])] = {
    case (t, o) if definition.isDefinedAt(t) => definition(t) -> o
  }

  def definition:PartialFunction[TypeDefinition, TypeDefinition] = {
    case t => t
  }

  private[dsentric] def isInternal:Boolean = false

  private[dsentric] def mask:Option[String] = None

  private[dsentric] def removalDenied:Boolean = false

  private[dsentric] def isEmpty:Boolean = false
}

case class AndValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]):Failures =
    left(path, value, currentState) ++ right(path, value, currentState)

  private[dsentric] override def isInternal:Boolean = left.isInternal || right.isInternal

  private[dsentric] override def removalDenied:Boolean = left.removalDenied || right.removalDenied

  private[dsentric] override def mask:Option[String] = left.mask.orElse(right.mask)

  override def definition:PartialFunction[TypeDefinition, TypeDefinition] =
    left.definition.andThen(right.definition)
}

case class OrValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]):Failures =
    left(path, value, currentState) match {
      case Failures.empty =>
        Failures.empty
      case list =>
        right(path, value, currentState) match {
          case Failures.empty =>
            Failures.empty
          case list2 if list2.size < list.size =>
            list2
          case _ =>
            list
        }
    }

  private[dsentric] override def mask:Option[String] = left.mask.orElse(right.mask)

  private[dsentric] override def removalDenied:Boolean = left.removalDenied || right.removalDenied

  private[dsentric] override def isInternal:Boolean = left.isInternal || right.isInternal

}

//TODO separate definition for internal/reserved etc such that the or operator is not supported
trait Validators extends ValidatorOps{

  val empty =
    new Validator[Nothing] {
      def apply[S >: Nothing](path:Path, value: Option[S], currentState: => Option[S]):Failures =
        Failures.empty

      override def isEmpty:Boolean = true
    }

  val internal =
    new Validator[Option[Nothing]] {
      def apply[S >: Option[Nothing]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        value.fold(Failures.empty)(_ => Failures(path -> "Value is reserved and cannot be provided."))

      private[dsentric] override def isInternal:Boolean = true

    }

  def mask(masking:String): Validator[Nothing] =
    new Validator[Nothing] {

      def apply[S >: Nothing](path:Path, value: Option[S], currentState: => Option[S]):Failures =
        Failures.empty

      override def mask:Option[String] = Some(masking)

    }

  val reserved =
    new Validator[Option[Nothing]] {
      def apply[S >: Option[Nothing]](path: Path, value: Option[S], currentState: => Option[S]): Failures =
        value.fold(Failures.empty)(_ => Failures(path -> "Value is reserved and cannot be provided."))
    }

  val immutable =
    new Validator[Nothing] {
      def apply[S >: Nothing](path:Path, value: Option[S], currentState: => Option[S]):Failures =
        (for {
          v <- value
          cs <- currentState
          if v != cs
        } yield
          path -> "Immutable value cannot be changed."
          ).toVector

    }




  val increment =
    new Validator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        (for {
          v <- value
          c <- currentState
          r <- resolve(c,v)
          a <- if (r >= 0) Some(path -> "Value must increase.") else None
        } yield a).toVector
    }

  val decrement =
    new Validator[Numeric] {
      def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        (for {
          v <- value
          c <- currentState
          r <- resolve(c,v)
          a <- if (r <= 0) Some(path -> "Value must decrease.") else None
        } yield a).toVector

    }

  def >(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path: Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def >(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def >=(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def >=(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def <(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def <(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def <=(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def <=(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def minLength[T](x: Int) = new Validator[Optionable[Length[T]]] {
    def apply[S >: Optionable[Length[T]]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def maxLength[T](x: Int) = new Validator[Optionable[Length[T]]] {
    def apply[S >: Optionable[Length[T]]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
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

  def in[T](values:T*)(implicit codec:DCodec[T]) = new Validator[Collectable[T]] {

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

    def apply[S >: Collectable[T]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value
        .toIterable
        .flatMap(getI[T, S])
        .filterNot(values.contains)
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  def nin[T](values:T*)(implicit codec:DCodec[T]) = new Validator[Collectable[T]] {

    def apply[S >: Collectable[T]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value
        .toIterable
        .flatMap(getI[T, S])
        .filter(values.contains)
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  //maybe change to generic equality
  def inCaseInsensitive(values:String*) = new Validator[Collectable[String]] {

    override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
      case n:StringDefinition => n.copy(values.toList)
      case m:MultipleTypeDefinition => m.remap(definition)
    }

    def apply[S >: Collectable[String]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value
        .toIterable
        .flatMap(getStringI)
        .filterNot(v => values.exists(_.equalsIgnoreCase(v.toString)))
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  def ninCaseInsensitive(values:String*) = new Validator[Collectable[String]] {

    def apply[S >: Collectable[String]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value
        .toIterable
        .flatMap(getStringI)
        .filter(v => values.exists(_.equalsIgnoreCase(v.toString)))
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  // TODO: Rewrite as regex?
  def nonEmpty[T]: Validator[Optionable[Length[T]]] =
    new Validator[Optionable[Length[T]]] {

      val message = "Value must not be empty."
      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case o:ObjectDefinition if o.minProperties.isEmpty => o.copy(minProperties = Some(1))
        case a:ArrayDefinition if a.minLength.isEmpty => a.copy(minLength = Some(1))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[Length[T]]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        value
          .flatMap(getLength)
          .filter(_ == 0)
          .map(v => path -> message)
          .toVector
    }

  // TODO: Rewrite as regex?
  val nonEmptyOrWhiteSpace =
    new Validator[Optionable[String]] {

      val message = "String must not be empty or whitespace."
      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:StringDefinition =>
          s.copy(minLength = s.minLength.orElse(Some(1)), pattern = s.pattern.orElse(Some("[^\\s]")))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[String]](path: Path, value: Option[S], currentState: => Option[S]): Failures =
        value
          .collect {
            case s: String => s
            case Some(s: String) => s
          }
          .filter(_.trim().isEmpty)
          .map(v => path -> message)
          .toVector
    }

  def custom[T](f: T => Boolean, message:String) =
    new Validator[Optionable[T]] {

      def apply[S >: Optionable[T]](path: Path, value: Option[S], currentState: => Option[S]): Failures =
        value
          .flatMap(getT[T, S])
          .toVector.flatMap{ s =>
          if (f(s)) Vector.empty
          else Vector(path -> message)
        }
    }

  def regex(r:Regex):Validator[Optionable[String]] =
    regex(r, s"String fails to match pattern '$r'.")

  def regex(r:Regex, message:String):Validator[Optionable[String]] =
    new Validator[Optionable[String]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:StringDefinition =>
          s.copy(pattern = Some(r.pattern.pattern()))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        value
          .flatMap(getString)
          .toVector
          .flatMap{ s =>
            if (r.pattern.matcher(s).matches) Vector.empty
            else Vector(path -> message)
          }
    }



  def noKeyRemoval:Validator[Optionable[Map[String,Nothing]]] =
    new Validator[Optionable[Map[String, Nothing]]] {
      def apply[S >: Optionable[Map[String, Nothing]]](path: Path, value: Option[S], currentState: => Option[S]): Failures = {
        Vector.empty
      }

      private[dsentric] override def removalDenied:Boolean = true
    }


  def mapContract[D <: DObject](contract:ContractFor[D]): Validator[Optionable[Map[String, D]]] =
    mapContractK[String, D](contract)

  def mapContractK[K, D <: DObject](contract:ContractFor[D]): Validator[Optionable[Map[K, D]]] =
    new Validator[Optionable[Map[K,D]]] {
      //TODO change to additional Properties
      override def withObjsDefinition(forceNested:Boolean): PartialFunction[(TypeDefinition, Vector[ObjectDefinition]), (TypeDefinition, Vector[ObjectDefinition])] = {
        case (s:ObjectDefinition, objs) if !forceNested =>
          val (ref, newObjs) = Schema.contractObjectDefinitionRef(contract, objs)
          s.copy(additionalProperties = Right(ByRefDefinition(ref))) -> newObjs

        case (s:ObjectDefinition, objs) =>
          val obj = Schema.nestedContractObjectDefinition(contract)
          s.copy(additionalProperties = Right(obj)) -> objs

        case (m:MultipleTypeDefinition, objs) =>
          m.withRemap(objs)(withObjsDefinition(forceNested))
      }

      def apply[S >: Optionable[Map[K, D]]](path: Path, value: Option[S], currentState: => Option[S]): Failures = {
        val c =
          for {
            co <- value
            ct <- getT[Map[K, D], S](co)
          } yield ct

        val cs =
          for {
            cso <- currentState
            cst <- getT[Map[K, D], S](cso)
          } yield cst

        val failures =
          for {
            o <- value.toIterator
            t <- getT[Map[K, D], S](o).toIterator
            kv <- t.toIterator
            f <- contract._validateFields(path \ kv._1.toString, kv._2.value, cs.flatMap(_.get(kv._1).map(_.value)))
          } yield f

        failures.toVector
      }

    }

  def mapContractByType[K, D <: DObject](
                                      contractMatch: (D => Boolean, ContractFor[D])*
                                    ): Validator[Optionable[Map[K, DObject]]] =
    new Validator[Optionable[Map[K, DObject]]] {

      def apply[S >: Optionable[Map[K, DObject]]](
                                                        path: Path,
                                                        value: Option[S],
                                                        currentState: => Option[S]
                                                      ): Failures = {
        val cs =
          (for {
            cso <- currentState
            cst <- getT[Map[K, D], S](cso)
          } yield cst).getOrElse(Map.empty[K, D])

        for {
          co       <- value.toVector
          ct       <- getT[Map[K, D], S](co).toVector
          pair     <- ct.toVector
          failures <- (cs.get(pair._1) match {
            case Some(cks) =>
              contractMatch
                .find(_._1(cks))
                .map(C =>
                  C._2
                    .$validate(pair._2, cks)
                    .fold(_.toList.toVector.map(p => path \ pair._1.toString ++ p._1 -> p._2), _ => Vector.empty)
                )
                .getOrElse(Vector(path \ pair._1.toString -> "Could not match type."))
            case None      =>
              contractMatch
                .find(_._1(pair._2))
                .map(C =>
                  C._2
                    .$validate(pair._2)
                    .fold(_.toList.toVector.map(p => path \ pair._1.toString ++ p._1 -> p._2), _ => Vector.empty)
                )
                .getOrElse(Vector(path \ pair._1.toString -> "Could not match type."))
          })
        } yield failures
      }
    }
  def keyValidator(r:Regex, message:String):Validator[Optionable[DObject]] =
    new Validator[Optionable[DObject]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:ObjectDefinition =>
          s.copy(propertyNames = Some(StringDefinition(pattern = Some(r.pattern.pattern()))))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[DObject]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        for {
          co <- value.toVector
          ct <- getT[DObject, S](co).toVector
          key <- ct.keys.toVector if !r.pattern.matcher(key).matches()
        } yield path \ key -> message
    }

  def keyValidator[T](message:String)(implicit D:DCodec[T]):Validator[Optionable[DObject]] =
    new Validator[Optionable[DObject]] {

      override def definition: PartialFunction[TypeDefinition, TypeDefinition] = {
        case s:ObjectDefinition if D.typeDefinition.isInstanceOf[StringDefinition] =>
          s.copy(propertyNames = Some(D.typeDefinition.asInstanceOf[StringDefinition]))
        case m:MultipleTypeDefinition => m.remap(definition)
      }

      def apply[S >: Optionable[DObject]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        for {
          co <- value.toVector
          ct <- getT[DObject, S](co).toVector
          key <- ct.keys.toVector if D.unapply(key).isEmpty
        } yield path \ key -> message
    }
}

trait ValidatorOps {

  protected def getLength[T, S >: Optionable[Length[T]]](x:S) =
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

  protected def getStringI[S >: Collectable[String]](x:S):Iterable[String] =
    x match {
      case s:String => Some(s)
      case s:Iterable[String@unchecked] => s
      case Some(s:String) => Some(s)
      case _ =>  None
    }

  protected def getT[T, S >: Optionable[T]](t:S):Option[T] =
    t match {
      case Some(s: T@unchecked) => Some(s)
      case None => None
      case s: T@unchecked => Some(s)
    }

  protected def getI[T, S >: Collectable[T]](t:S):Iterable[T] =
    t match {
      case s:String => Some(s.asInstanceOf[T])
      case s:Iterable[T@unchecked] => s
      case Some(s:T@unchecked) => Some(s)
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

object Validators extends Validators

object ValidationText {

  val UNEXPECTED_TYPE = "Value is not of the expected type."
  val EXPECTED_VALUE = "Value was expected."

}