package dsentric.contracts

import dsentric.{DObject, Data, Path}
import dsentric.codecs.{DMapCodec, DataCodec}

trait PropertySyntax {

  implicit def toMapFilterSyntax[R <: DObject, K, V, C[_, _] <: Map[_, _]](prop: Property[R, C[K, V]]):MapFilterOps[R, K, V, C] =
    MapFilterOps(prop)

  implicit def toDObjectFilterSyntax[R <: DObject, D <: DObject](prop: Property[R, D]):DObjectFilterOps[R, D] =
    DObjectFilterOps(prop)

  implicit def property2Path[V](prop:Property[_, V]):Path =
    prop._path
}

final case class MapFilterOps[R <: DObject, K, V, C[_, _] <: Map[_, _]](prop: Property[R, C[K, V]]) extends AnyVal  {

  def \(key:K):Property[R, V] = {
    val codec = prop._codec.asInstanceOf[DMapCodec[C[K, V], K, V]]
    val keyString = codec.keyCodec(key)
    EmptyProperty(codec.valueCodec, keyString, prop._path \ keyString, prop._root)
  }

}

final case class DObjectFilterOps[R <: DObject, D <: DObject](prop: Property[R, D]) extends AnyVal  {

  def \(key:String):Property[R, Data] =
    EmptyProperty(DataCodec, key, prop._path \ key, prop._root)

  def \\(path:Path):Property[R, Data] = {
    path.tailKeyOption match {
      case Some(key) =>
        EmptyProperty(DataCodec, key, prop._path ++ path, prop._root)
      case _ =>
        EmptyProperty(DataCodec, "{Index Key}", prop._path ++ path, prop._root)
    }
  }

  def \\[T](property:Property[D, T]):Property[R, T] = {
    property._path.tailKeyOption match {
      case Some(key) =>
        EmptyProperty(property._codec, key, prop._path ++ property._path, prop._root)
      case _ =>
        EmptyProperty(property._codec, "{Index Key}", prop._path ++ property._path, prop._root)
    }
  }
}

object PropertySyntax extends PropertySyntax
