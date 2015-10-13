package dsentric

trait Validator[Data, +T] {

  def validate(value:Option[Data], currentState:Option[Data]):Seq[String]
}

object Validator {

  def empty[Data] = new Validator[Data, Nothing] {
    def validate(value: Option[Data], currentState: Option[Data]): Seq[String] =
      Nil
  }
}
