package dsentric.contracts

import dsentric.{DObject, Data, Path}
import dsentric.codecs.{DMapCodec, DataCodec}

trait PropertySyntax {

  implicit def toMapFilterOps[R <: DObject, K, V, C[_, _] <: Map[?, ?]](
    prop: Property[R, C[K, V]]
  ): MapFilterOps[R, K, V, C] =
    MapFilterOps(prop)

  implicit def toContractAdditionalOps[R <: DObject, K, V](
    contract: ContractFor[R] & AdditionalProperties[K, V]
  ): ContractAdditionalOps[R, K, V] =
    ContractAdditionalOps(contract)

  implicit def toPropertyAdditionalOps[R <: DObject, K, V](
    property: ObjectProperty[R] & AdditionalProperties[K, V]
  ): PropertyAdditionalOps[R, K, V] =
    PropertyAdditionalOps(property)

  implicit def toDObjectFilterOps[R <: DObject, D <: DObject](prop: Property[R, D]): DObjectFilterOps[R, D] =
    DObjectFilterOps(prop)

  implicit def property2Path[V](prop: Property[?, V]): Path =
    prop._path
}

final case class ContractAdditionalOps[R <: DObject, K, V](contract: ContractFor[R] & AdditionalProperties[K, V])
    extends AnyVal {

  def \(key: K): DynamicProperty[R, V] = {
    val codec     = contract._additionalValueCodec
    val keyString = contract._additionalKeyCodec(key)
    DynamicProperty(codec, keyString, Path(keyString), contract)
  }

}

final case class PropertyAdditionalOps[R <: DObject, K, V](property: ObjectProperty[R] & AdditionalProperties[K, V])
    extends AnyVal {

  def \(key: K): DynamicProperty[R, V] = {
    val codec     = property._additionalValueCodec
    val keyString = property._additionalKeyCodec(key)
    DynamicProperty(codec, keyString, property._path \ keyString, property._root)
  }

}

final case class MapFilterOps[R <: DObject, K, V, C[_, _] <: Map[?, ?]](prop: Property[R, C[K, V]]) extends AnyVal {

  def \(key: K): DynamicProperty[R, V] = {
    val codec     = prop._codec.asInstanceOf[DMapCodec[C[K, V], K, V]]
    val keyString = codec.keyCodec(key)
    DynamicProperty(codec.valueCodec, keyString, prop._path \ keyString, prop._root)
  }

}

final case class DObjectFilterOps[R <: DObject, D <: DObject](prop: Property[R, D]) extends AnyVal {

  def \(key: String): DynamicProperty[R, Data] =
    DynamicProperty(DataCodec, key, prop._path \ key, prop._root)

  def \\(path: Path): DynamicProperty[R, Data] =
    path.tailKeyOption match {
      case Some(key) =>
        DynamicProperty(DataCodec, key, prop._path ++ path, prop._root)
      case _         =>
        DynamicProperty(DataCodec, "{Index Key}", prop._path ++ path, prop._root)
    }

  def \\[T](property: Property[?, T]): DynamicProperty[R, T] =
    property._path.tailKeyOption match {
      case Some(key) =>
        DynamicProperty(property._codec, key, prop._path ++ property._path, prop._root)
      case _         =>
        DynamicProperty(property._codec, "{Index Key}", prop._path ++ property._path, prop._root)
    }
}

object PropertySyntax extends PropertySyntax
