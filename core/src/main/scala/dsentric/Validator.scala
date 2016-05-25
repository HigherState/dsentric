package dsentric

import scala.util.matching.Regex

trait Validator[+T] {

  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]): Failures

  def &&[S >: T] (v:Validator[S]):Validator[S] = AndValidator(this, v)

  def ||[S >: T] (v:Validator[S]):Validator[S] = OrValidator(this, v)

  private[dsentric] def isInternal:Boolean = false
}

case class AndValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState: => Option[S]):Failures =
    left(path, value, currentState) ++ right(path, value, currentState)

  private[dsentric] override def isInternal:Boolean = left.isInternal || right.isInternal
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


  private[dsentric] override def isInternal:Boolean = left.isInternal || right.isInternal
}

//TODO separate definition for internal/reserved etc such that the or operator is not supported
trait Validators {

  val empty =
    new Validator[Nothing] {
      def apply[S >: Nothing](path:Path, value: Option[S], currentState: => Option[S]):Failures =
        Failures.empty
    }

  val internal =
    new Validator[Option[Nothing]] {
      def apply[S >: Option[Nothing]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        value.fold(Failures.empty)(_ => Failures(path -> "Value is reserved and cannot be provided."))

      private[dsentric] override def isInternal:Boolean = true
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
  }

  def >(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c >= 0
      } yield path -> s"Value $v is not greater than $x.")
        .toVector
  }

  def >=(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c > 0
      } yield path -> s"Value $v is not greater or equal to $x.")
        .toVector
  }

  def >=(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c > 0
      } yield path -> s"Value $v is not greater or equal to $x.")
        .toVector
  }

  def <(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c <= 0
      } yield path -> s"Value $v is not less than $x.")
        .toVector
  }

  def <(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c <= 0
      } yield path -> s"Value $v is not less than $x.")
        .toVector
  }

  def <=(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c < 0
      } yield path -> s"Value $v is not less than or equal to $x.")
        .toVector
  }

  def <=(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c < 0
      } yield path -> s"Value $v is not less than or equal to $x.")
        .toVector
  }

  def minLength(x: Int) = new Validator[Optionable[Length]] {
    def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value.flatMap(getLength)
        .filter(_ < x)
        .map(v => path -> s"Value must have a length of at least $x.")
        .toVector
  }

  def maxLength(x: Int) = new Validator[Optionable[Length]] {
    def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value.flatMap(getLength)
        .filter(_ > x)
        .map(v => path -> s"Value must have a length of at most $x.")
        .toVector
  }

  def in[T](values:T*) = new Validator[Optionable[T]] {
    def apply[S >: Optionable[T]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value
        .flatMap(getT[T, S])
        .filterNot(values.contains)
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  def nin[T](values:T*) = new Validator[Optionable[T]] {
    def apply[S >: Optionable[T]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value
        .flatMap(getT[T, S])
        .filter(values.contains)
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  //maybe change to generic equality
  def inCaseInsensitive(values:String*) = new Validator[Optionable[String]] {
    def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value
        .flatMap(getString)
        .filterNot(v => values.exists(_.equalsIgnoreCase(v.toString)))
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  def ninCaseInsensitive(values:String*) = new Validator[Optionable[String]] {
    def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
      value
        .flatMap(getString)
        .filter(v => values.exists(_.equalsIgnoreCase(v.toString)))
        .map(v => path -> s"'$v' is not an allowed value.")
        .toVector
  }

  val nonEmpty =
    new Validator[Optionable[Length]] {
      def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        value
          .flatMap(getLength)
          .filter(_ == 0)
          .map(v => path -> "Value must not be empty.")
          .toVector
    }

  val nonEmptyOrWhiteSpace =
    new Validator[Optionable[String]] {
      def apply[S >: Optionable[String]](path: Path, value: Option[S], currentState: => Option[S]): Failures =
        value
          .collect {
            case s: String => s
            case Some(s: String) => s
          }
          .filter(_.trim().isEmpty)
          .map(v => path -> "String must not empty or whitespace.")
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
      def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: => Option[S]): Failures =
        value
          .flatMap(getString)
          .toVector
          .flatMap{ s =>
            if (r.pattern.matcher(s).matches) Vector.empty
            else Vector(path -> message)
          }
    }

  private def getLength[S >: Optionable[Length]](x:S) =
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

  private def getString[S >: Optionable[String]](x:S):Option[String] =
    x match {
      case Some(s:String) => Some(s)
      case s:String => Some(s)
      case _ =>  None
    }

  private def getT[T, S >: Optionable[T]](t:S):Option[T] =
    t match {
      case Some(s: T@unchecked) => Some(s)
      case s: T@unchecked => Some(s)
    }

  private def resolve[S >: Numeric](value:S, target:S):Option[Int] =
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

  private def compare[S >: Numeric](value:Long, target:S):Option[Int] =
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

  private def compare[S >: Numeric](value:Double, target:S):Option[Int] =
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