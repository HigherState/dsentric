package dsentric.operators

import dsentric.{DCodec, Raw}

trait Sanitizers {

  def mask(mask:String):Sanitizer[Nothing] =
    new Sanitizer[Nothing] {
      def sanitize[S >: Nothing](value: Option[S]): Option[String] =
        Some(mask)
    }

  def mask[T, U](f:Option[T] => Option[U])(implicit D:DCodec[U]):Sanitizer[T] =
    new Sanitizer[T] {
      def sanitize[S >: T](value: Option[S]): Option[Raw] =
        f(value.map(_.asInstanceOf[T])).map(D.apply)
    }
}
