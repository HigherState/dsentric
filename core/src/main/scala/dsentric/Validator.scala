package dsentric

import scalaz.Scalaz._

trait Validator[+T] {

  def apply[S >: T](value:Option[S], currentState:Option[S]): Seq[String]

  def &&[S >: T] (v:Validator[S]):Validator[S] = AndValidator(this, v)

  def ||[S >: T] (v:Validator[S]):Validator[S] = OrValidator(this, v)
}

case class AndValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](value:Option[S], currentState:Option[S]):Seq[String] =
    left(value, currentState) ++ right(value, currentState)
}

case class OrValidator[+T, A <: T, B <: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def apply[S >: T](value:Option[S], currentState:Option[S]):Seq[String] = {
    left(value, currentState) match {
      case Seq() => Seq()
      case list => right(value, currentState) match {
        case Seq() => Seq()
        case list2 if list2.size < list.size => list2
        case _ => list
      }
    }
  }
}

object Validator {

  val empty = new Validator[Nothing] {
    def apply[S >: Nothing](value: Option[S], currentState: Option[S]): Seq[String] = Seq.empty
  }

  val internal = new Validator[Option[Nothing]] {

    def apply[S >: Option[Nothing]](value: Option[S], currentState: Option[S]): Seq[String] =
      if (value.nonEmpty) Seq("Value is reserved and cannot be provided.")
      else Seq.empty
  }

  val reserved = new Validator[Option[Nothing]] {
    def apply[S >: Option[Nothing]](value: Option[S], currentState: Option[S]): Seq[String] =
      if (value.nonEmpty) Seq("Value is reserved and cannot be provided.")
      else Seq.empty
  }

  val increment = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      (for {
        v <- value
        c <- currentState
        r <- resolve(c,v)
        a <- (r >= 0).option("Value must increase.")
      } yield a).toSeq
  }

  val decrement = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      (for {
        v <- value
        c <- currentState
        r <- resolve(c,v)
        a <- (r <= 0).option("Value must decrease.")
      } yield a).toSeq
  }

  def >(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(t => compare(x, t)).fold[Seq[String]](Seq.empty){v =>
        if (v < 0) Seq.empty
        else Seq(s"Value $value is not greater than $x")
      }
  }

  def >(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(t => compare(x, t)).fold[Seq[String]](Seq.empty){v =>
        if (v < 0) Seq.empty
        else Seq(s"Value $value is not greater than $x")
      }
  }

  def >=(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(t => compare(x, t)).fold[Seq[String]](Seq.empty){v =>
        if (v <= 0) Seq.empty
        else Seq(s"Value $value is not greater than $x")
      }
  }

  def >=(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(t => compare(x, t)).fold[Seq[String]](Seq.empty){v =>
        if (v <= 0) Seq.empty
        else Seq(s"Value $value is not greater than $x")
      }
  }

  def <(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(t => compare(x, t)).fold[Seq[String]](Seq.empty){v =>
        if (v > 0) Seq.empty
        else Seq(s"Value $value is not greater than $x")
      }
  }

  def <(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(t => compare(x, t)).fold[Seq[String]](Seq.empty){v =>
        if (v > 0) Seq.empty
        else Seq(s"Value $value is not greater than $x")
      }
  }

  def <=(x:Long) = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(t => compare(x, t)).fold[Seq[String]](Seq.empty){v =>
        if (v >= 0) Seq.empty
        else Seq(s"Value $value is not greater than $x")
      }
  }

  def <=(x:Double) = new Validator[Numeric] {
    def apply[S >: Numeric](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(t => compare(x, t)).fold[Seq[String]](Seq.empty){v =>
        if (v >= 0) Seq.empty
        else Seq(s"Value $value is not greater than $x")
      }
  }

  def minLength(x: Int) = new Validator[Optionable[Length]] {
    def apply[S >: Optionable[Length]](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(getLength).flatMap(v => (v < x).option(s"Value must have a length of at least $x.")).toSeq
  }

  def maxLength(x: Int) = new Validator[Optionable[Length]] {
    def apply[S >: Optionable[Length]](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(getLength).flatMap(v => (v > x).option(s"Value must have a length of at most $x.")).toSeq
  }

  def in[T](values:T*) = new Validator[Optionable[T]] {
    def apply[S >: Optionable[T]](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(v => (!values.contains(v)).option(s"'$v is not an allowed value.")).toSeq
  }

  def nin[T](values:T*) = new Validator[Optionable[T]] {
    def apply[S >: Optionable[T]](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(v => values.contains(v).option(s"'$v is not an allowed value.")).toSeq
  }

  def inCaseInsensitive(values:String*) = new Validator[Optionable[String]] {
    def apply[S >: Optionable[String]](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(v => (!values.exists(_.equalsIgnoreCase(v.toString))).option(s"'$v is not an allowed value.")).toSeq
  }

  def ninCaseInsensitive(values:String*) = new Validator[Optionable[String]] {
    def apply[S >: Optionable[String]](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(v => values.exists(_.equalsIgnoreCase(v.toString)).option(s"'$v is not an allowed value.")).toSeq
  }

  val nonEmpty = new Validator[Optionable[Length]] {
    def apply[S >: Optionable[Length]](value: Option[S], currentState: Option[S]): Seq[String] =
      value.flatMap(getLength).flatMap(v => (v == 0).option(s"Value must not be empty.")).toSeq
  }

  val nonEmptyOrWhiteSpace:Validator[String] = new Validator[Optionable[String]] {
    def apply[S >: Optionable[String]](value: Option[S], currentState: Option[S]): Seq[String] =
      value.collect {
        case s:String => s
        case Some(s:String) => s
      }.flatMap(_.trim().isEmpty.option("String must not empty or whitespace")).toSeq
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
