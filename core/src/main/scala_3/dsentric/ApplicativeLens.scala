package dsentric

private[dsentric] trait ApplicativeMatcher[Data] {
  def unapply(data: Data): Boolean
}

private[dsentric] trait ApplicativeLens[Data, P] {

  def unapply(data: Data): Option[P]

  def @:[P1, O](prev: ApplicativeLens[Data, P1])(implicit
    ev: ApplicativeLens.Aux[Data, ApplicativeLens[Data, P1] *: ApplicativeLens[Data, P] *: EmptyTuple, P1 *: P *: EmptyTuple]
  ) =
    LensList(prev *: this.asInstanceOf[ApplicativeLens[Data, P]] *: EmptyTuple, Nil, ev)

  def @:(prev: ApplicativeMatcher[Data]) =
    MatcherSingle(List(prev), this)
}

trait Evaluator[Data, L <: Tuple] {
  type OutP <: Tuple
  def patternMatch(data: Data, l: L): Option[OutP]
}

object ApplicativeLens {

  type Aux[Data, L <: Tuple, OP <: Tuple] =
    Evaluator[Data, L] {
      type OutP = OP
    }

  type E[Data, P] = ApplicativeLens[Data, P]

  implicit def evaluatorHNil[Data]: Aux[Data, EmptyTuple, EmptyTuple] =
    new Evaluator[Data, EmptyTuple] {
      type OutP = EmptyTuple

      def patternMatch(data: Data, lp: EmptyTuple) = Some(EmptyTuple)
    }

  implicit def evalHCons[Data, P, L <: Tuple](implicit
    evalT: Evaluator[Data, L]
  ): Aux[Data, E[Data, P] *: L, P *: evalT.OutP] =
    new Evaluator[Data, E[Data, P] *: L] {
      type OutP = P *: evalT.OutP

      def patternMatch(data: Data, l: E[Data, P] *: L): Option[OutP] =
        for {
          h <- l.head.unapply(data)
          t <- evalT.patternMatch(data, l.tail)
        } yield h *: t
    }

  implicit class HListExt[Data, T <: Tuple](val t: T) extends AnyVal {
    def @:[P](prev: ApplicativeLens[Data, P]): ApplicativeLens[Data, P] *: T =
      prev *: t
  }
}

case class LensList[Data, L <: Tuple, OP <: Tuple](
  maybes: L,
  matchers: List[ApplicativeMatcher[Data]],
  ev: ApplicativeLens.Aux[Data, L, OP]
) {
  def unapply(data: Data): Option[OP] =
    if (matchers.forall(_.unapply(data)))
      ev.patternMatch(data, maybes)
    else
      None

  def unapply(data: Validated[Data]): Some[OP] =
    Some(
      ev.patternMatch(data.validObject, maybes).get
    )

  final def @:[P, TP2, TS2](prev: ApplicativeLens[Data, P]) =
    LensList(prev *: maybes, matchers, ApplicativeLens.evalHCons[Data, P, L](ev))

  final def @:(prev: ApplicativeMatcher[Data]) =
    LensList[Data, L, OP](maybes, prev :: matchers, ev)
}

case class MatcherSingle[Data, P](matchers: List[ApplicativeMatcher[Data]], lens: ApplicativeLens[Data, P]) {

  def unapplySeq(data: Data): Option[P] =
    if (matchers.forall(_.unapply(data)))
      lens.unapply(data)
    else
      None

  def unapplySeq(data: Validated[Data]): Some[P] =
    Some(lens.unapply(data.validObject).get)

  final def @:[P1, O](prev: ApplicativeLens[Data, P1])(implicit
    ev: ApplicativeLens.Aux[Data, ApplicativeLens[Data, P1] *: ApplicativeLens[Data, P] *: EmptyTuple, P1 *: P *: EmptyTuple]
  ) =
    LensList(prev *: lens *: EmptyTuple, matchers, ev)

  final def @:[O](prev: ApplicativeMatcher[Data]) =
    MatcherSingle(prev :: matchers, lens)
}
