package dsentric.operators

import dsentric.{DCodec, Data, ForceWrapper, Raw}

trait Sanitizers {

  def mask(mask:String):Sanitizer[Nothing] =
    new Sanitizer[Nothing] {
      def sanitize[S >: Nothing](value: Option[S]): Option[Data] =
        Some(ForceWrapper.data(mask))
    }

  def mask[T, U](f:Option[T] => Option[U])(implicit D:DCodec[U]):Sanitizer[T] =
    new Sanitizer[T] {
      def sanitize[S >: T](value: Option[S]): Option[Data] =
        f(value.map(_.asInstanceOf[T])).map(D.apply)
    }
}
