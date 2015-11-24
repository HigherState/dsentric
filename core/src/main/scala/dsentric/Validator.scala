package dsentric

import scalaz.Scalaz._

trait Validator[+T] {

  def apply[S >: T](path:Path, value:Option[S], currentState:Option[S]): Failures

  def &&[S >: T] (v:Validator[S]):Validator[S] = AndValidator(this, v)

  def ||[S >: T] (v:Validator[S]):Validator[S] = OrValidator(this, v)
}

case class AndValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState:Option[S]):Failures =
    left(path, value, currentState) ++ right(path, value, currentState)
}

case class OrValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](path:Path, value:Option[S], currentState:Option[S]):Failures = {
    left(path, value, currentState) match {
      case Failures.empty =>
        Failures.empty
      case list => right(path, value, currentState) match {
        case Failures.empty =>
          Failures.empty
        case list2 if list2.size < list.size =>
          list2
        case _ =>
          list
      }
    }
  }
}

//TODO separate definition for internal/reserved etc such that the or operator is not supported
object Validator {

  val empty = new Validator[Nothing] {
    def apply[S >: Nothing](path:Path, value: Option[S], currentState: Option[S]):Failures =
      Failures.empty
  }

  val internal = new Validator[Option[Nothing]] {

    def apply[S >: Option[Nothing]](path:Path, value: Option[S], currentState: Option[S]): Failures =
      value.fold(Failures.empty)(_ => Failures(path -> "Value is reserved and cannot be provided."))
  }

  val reserved = new Validator[Option[Nothing]] {
    def apply[S >: Option[Nothing]](path: Path, value: Option[S], currentState: Option[S]): Failures =
      value.fold(Failures.empty)(_ => Failures(path -> "Value is reserved and cannot be provided."))
  }

  val increment = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- currentState
        r <- resolve(c,v)
        a <- (r >= 0).option(path -> "Value must increase.")
      } yield a).toVector
  }

  val decrement = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- currentState
        r <- resolve(c,v)
        a <- (r <= 0).option(path -> "Value must decrease.")
      } yield a).toVector
  }

  def >(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path: Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c >= 0
      } yield path -> s"Value $v is not greater than $x.")
      .toVector
  }

  def >(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c >= 0
      } yield path -> s"Value $v is not greater than $x.")
        .toVector
  }

  def >=(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c > 0
      } yield path -> s"Value $v is not greater or equal to $x.")
        .toVector
  }

  def >=(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c > 0
      } yield path -> s"Value $v is not greater or equal to $x.")
        .toVector
  }

  def <(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c <= 0
      } yield path -> s"Value $v is not less than $x.")
        .toVector
  }

  def <(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c <= 0
      } yield path -> s"Value $v is not less than $x.")
        .toVector
  }

  def <=(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c < 0
      } yield path -> s"Value $v is not less than or equal to $x.")
        .toVector
  }

  def <=(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](path:Path, value: Option[S], currentState: Option[S]): Failures =
      (for {
        v <- value
        c <- compare(x, v)
        if c < 0
      } yield path -> s"Value $v is not less than or equal to $x.")
        .toVector
  }

  def minLength(x: Int) = new Validator[Optionable[Length]] {
    def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: Option[S]): Failures =
      value.flatMap(getLength)
        .filter(_ < x)
        .map(v => path -> s"Value must have a length of at least $x.")
        .toVector
  }

  def maxLength(x: Int) = new Validator[Optionable[Length]] {
    def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: Option[S]): Failures =
      value.flatMap(getLength)
        .filter(_ > x)
        .map(v => path -> s"Value must have a length of at most $x.")
        .toVector
  }

  def in[T](values:T*) = new Validator[Optionable[T]] {
    def apply[S >: Optionable[T]](path:Path, value: Option[S], currentState: Option[S]): Failures =
      value
        .filterNot(values.contains)
        .map(v => path -> s"'$v is not an allowed value.")
        .toVector
  }

  def nin[T](values:T*) = new Validator[Optionable[T]] {
    def apply[S >: Optionable[T]](path:Path, value: Option[S], currentState: Option[S]): Failures =
      value
        .filter(values.contains)
        .map(v => path -> s"'$v is not an allowed value.")
        .toVector
  }

  //maybe change to generic equality
  def inCaseInsensitive(values:String*) = new Validator[Optionable[String]] {
    def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: Option[S]): Failures =
      value
        .filterNot(v => values.exists(_.equalsIgnoreCase(v.toString)))
        .map(v => path -> s"'$v is not an allowed value.")
        .toVector
  }

  def ninCaseInsensitive(values:String*) = new Validator[Optionable[String]] {
    def apply[S >: Optionable[String]](path:Path, value: Option[S], currentState: Option[S]): Failures =
      value
        .filter(v => values.exists(_.equalsIgnoreCase(v.toString)))
        .map(v => path -> s"'$v is not an allowed value.")
        .toVector
  }

  val nonEmpty = new Validator[Optionable[Length]] {
    def apply[S >: Optionable[Length]](path:Path, value: Option[S], currentState: Option[S]): Failures =
      value
        .flatMap(getLength)
        .filter(_ == 0)
        .map(v => path -> s"Value must not be empty.")
        .toVector
  }

  val nonEmptyOrWhiteSpace = new Validator[Optionable[String]] {
    def apply[S >: Optionable[String]](path: Path, value: Option[S], currentState: Option[S]): Failures =
      value
        .collect {
          case s: String => s
          case Some(s: String) => s
        }
        .filter(_.trim().isEmpty)
        .map(v => path -> "String must not empty or whitespace")
        .toVector
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
        Some(s.size)
      case _ =>
        None
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

object ValidationText {

  val UNEXPECTED_TYPE = "Value is not of the expected type."
  val EXPECTED_VALUE = "Value was expected."

}