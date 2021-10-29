package dsentric

import cats.data.NonEmptyList
import dsentric.codecs.DCodec
import dsentric.contracts.ValidPathSetter
import dsentric.failure.{Failure, ValidResult}

final class StringOps(val self: String) extends AnyVal {
  def \(part: String): Path                              =
    self :: part :: PathEnd
  def \(part: Int): Path                                 =
    self :: part :: PathEnd
  def :=[T](t: T)(implicit D: DCodec[T]): (String, Data) =
    self -> Data(t)

  def ::=(pairs: (String, Data)*): (String, DObject) =
    self -> DObject(pairs: _*)

  def p: Path = Path(self)
}

final class IntOps(val self: Int) extends AnyVal {
  def \(part: String): Path =
    self :: part :: PathEnd
  def \(part: Int): Path    =
    self :: part :: PathEnd
}

final class PathOps(val self: Path) extends AnyVal {
  def :=[T](v: T)(implicit D: DCodec[T]): (Path, Data) =
    self -> Data(v)

}

final class FunctionOps[D <: DObjectOps[D] with DObject](val f: D => D) extends AnyVal {
  def ~+(kv: (String, Data)): D => D =
    f.andThen(_ + kv)

  def ~++(kv: Seq[(String, Data)]): D => D =
    f.andThen(_ ++ kv)

  def ~(f2: D => D): D => D =
    d => f2(f(d))

  def |>(d: D): D =
    f(d)
}

final class ValidFunctionOps[D <: DObjectOps[D] with DObject](val f: D => ValidResult[D]) extends AnyVal {
  def ~+(kv: (String, Data)): D => ValidResult[D] =
    f.andThen(_.map(_ + kv))

  def ~++(kv: Seq[(String, Data)]): D => ValidResult[D] =
    f.andThen(_.map(_ ++ kv))

  def ~[T <: D with ValidResult[D]](f2: D => T): D => ValidResult[D] =
    (j: D) =>
      f(j).flatMap { d =>
        val r = f2(d)
        if (r.isInstanceOf[Left[NonEmptyList[Failure], D]])
          r.asInstanceOf[Left[NonEmptyList[Failure], D]]
        else if (r.isInstanceOf[Right[NonEmptyList[Failure], D]])
          r.asInstanceOf[Right[NonEmptyList[Failure], D]]
        else Right(r.asInstanceOf[D])
      }

  def ~(f2: ValidPathSetter[D]): D => ValidResult[D] =
    (j: D) => f(j).flatMap(f2)

  def |>(d: D): ValidResult[D] = f(d)
}

trait Syntax {

  implicit def toStringOps(s: String): StringOps =
    new StringOps(s)

  implicit def toIntOps(i: Int): IntOps =
    new IntOps(i)

  implicit def toFunctionOps[D <: DObjectOps[D] with DObject](f: D => D): FunctionOps[D] =
    new FunctionOps[D](f)

  implicit def toValidFunctionOps[D <: DObjectOps[D] with DObject](f: D => ValidResult[D]): ValidFunctionOps[D] =
    new ValidFunctionOps[D](f)

  implicit def pathOps(p: Path): PathOps =
    new PathOps(p)

  def ~+[D <: DObjectOps[D] with DObject](kv: (String, Data)): D => D =
    _ + kv

  def ~++[D <: DObjectOps[D] with DObject](kv: Seq[(String, Data)]): D => D =
    _ ++ kv
}
