package object dsentric {

  type Raw = Any
  type RawValue = Any
  type RawObject = Map[String, Any]
  type RawArray = Vector[Any]

  type Id[+T] = T

  type Const[C] = {
    type λ[T] = C
  }
}
