package dsentric

import cats.data.NonEmptyList
import dsentric.contracts.{PathSetter, ValidPathSetter}
import dsentric.failure.{Failure, ValidResult}

final class StringOps(val self:String) extends AnyVal {
  def \(part:String):Path =
    Path(self, part)
  def \(part:Int):Path =
    Path(self, part)

  def :=[T](t:T)(implicit D: DCodec[T]):(String, Data) =
    self -> D(t)

  def ::=(pairs:(String, Data)*):(String, Data) =
    self -> DObject(pairs:_*)

  def p:Path = Path(self)
}

final class IntOps(val self:Int) extends AnyVal {
  def \(part:String):Path =
    Path(self, part)
  def \(part:Int):Path =
    Path(self, part)
}

final class PathOps(val self:Path) extends AnyVal {
  def :=[T](v:T)(implicit D:DCodec[T]):(Path, Data) =
    self -> D(v)

}

final class FunctionOps[D <: DObjectLike[D] with DObject](val f:D => D) extends AnyVal with LensCompositor[D] {
  def ~+(kv:(String, Data)):D => D =
    f andThen (_ + kv)

  def ~++(kv:Seq[(String, Data)]):D => D =
    f andThen (_ ++ kv)
}

final class ValidFunctionOps[D <: DObjectLike[D] with DObject](val f:D => ValidResult[D]) extends AnyVal  {
  def ~+(kv:(String, Data)):D => ValidResult[D] =
    f andThen (_.map(_ + kv))

  def ~++(kv:Seq[(String, Data)]):D => ValidResult[D] =
    f andThen (_.map(_ ++ kv))

  def ~[T <: D with ValidResult[D]](f2:D => T):D => ValidResult[D] =
    (j: D) => f(j).flatMap {
      f(_) match {
        case Left(failures: NonEmptyList[Failure]) =>
          Left(failures)
        case Right(d: D@unchecked) =>
          Right(d)
        case d: D@unchecked =>
          Right(d)
      }
    }

  def ~(f2:ValidPathSetter[D]):D => ValidResult[D] =
    (j: D) => f(j).flatMap(f2)


  def |>(d:D):ValidResult[D] = f(d)
}

trait ToExtensionOps {

  implicit def toStringOps(s: String): StringOps =
    new StringOps(s)

  implicit def toIntOps(i: Int): IntOps =
    new IntOps(i)

  implicit def toFunctionOps[D <: DObjectLike[D] with DObject](f: D => D): FunctionOps[D] =
    new FunctionOps[D](f)

  implicit def toValidFunctionOps[D <: DObjectLike[D] with DObject](f: D => ValidResult[D]): ValidFunctionOps[D] =
    new ValidFunctionOps[D](f)

  implicit def pathOps(p:Path): PathOps =
    new PathOps(p)

  def ~+[D <: DObjectLike[D] with DObject](kv:(String, Data)):D => D =
    _ + kv

  def ~++[D <: DObjectLike[D] with DObject](kv:Seq[(String, Data)]):D => D =
    _ ++ kv
}