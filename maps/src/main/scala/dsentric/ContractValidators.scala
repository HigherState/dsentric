package dsentric

import Dsentric._

trait ContractValidators extends ValidatorOps {

  def mapContract(contract:Contract) = new Validator[Optionable[Map[String,DObject]]] {
    def apply[S >: Optionable[Map[String, DObject]]](path: Path, value: Option[S], currentState: => Option[S]): Failures = {
      val c =
        for {
          co <- value
          ct <- getT[Map[String, DObject], S](co)
        } yield ct

      val failures =
        for {
          o <- value.toIterator
          t <- getT[Map[String, DObject], S](o).toIterator
          kv <- t.toIterator
          f <- contract._validateFields(path \ kv._1, kv._2.value, c.flatMap(_.get(kv._1).map(_.value)))
        } yield f

      failures.toVector
    }

  }
}
