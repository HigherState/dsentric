package dsentric

import monocle.Prism

trait ExpectedDsl[Data, IndexedData] {
  def apply[T](implicit prism: Prism[Data, T]) =
    new Expected[Data, IndexedData, T](Validator.empty, None)

  def apply[T](validator:Validator[T])(implicit prism: Prism[Data, T]) =
    new Expected[Data, IndexedData, T](validator, None)

  def apply[T](name:String)(implicit prism: Prism[Data, T]) =
    new Expected[Data, IndexedData, T](Validator.empty, Some(name))

  def apply[T](name:String, validator:Validator[T])(implicit prism: Prism[Data, T]) =
    new Expected[Data, IndexedData, T](validator, Some(name))
}

trait MaybeDsl[Data, IndexedData] {
  def apply[T](implicit prism: Prism[Data, T], strictness: Strictness) =
    new Maybe[Data, IndexedData, T](Validator.empty, None)

  def apply[T](validator:Validator[Option[T]])(implicit prism: Prism[Data, T], strictness: Strictness) =
    new Maybe[Data, IndexedData, T](validator, None)

  def apply[T](name:String)(implicit prism: Prism[Data, T], strictness: Strictness) =
    new Maybe[Data, IndexedData, T](Validator.empty, Some(name))

  def apply[T](name:String, validator:Validator[Option[T]])(implicit prism: Prism[Data, T], strictness: Strictness) =
    new Maybe[Data, IndexedData, T](validator, Some(name))
}

trait DefaultDsl[Data, IndexedData] {

  def apply[T](default:T)(implicit prism: Prism[Data, T], strictness:Strictness) =
    new Default[Data, IndexedData, T](default, Validator.empty, None)

  def apply[T](default:T, validator:Validator[Option[T]])(implicit prism: Prism[Data, T], strictness:Strictness) =
    new Default[Data, IndexedData, T](default, validator, None)

  def apply[T](name:String, default:T)(implicit prism: Prism[Data, T], strictness:Strictness) =
    new Default[Data, IndexedData, T](default, Validator.empty, Some(name))

  def apply[T](name:String, default:T, validator:Validator[Option[T]])(implicit prism: Prism[Data, T], strictness:Strictness) =
    new Default[Data, IndexedData, T](default, validator, Some(name))
}



