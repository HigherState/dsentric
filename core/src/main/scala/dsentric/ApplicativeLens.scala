package dsentric

import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

private[dsentric] trait ApplicativeMatcher[Data] {
  def unapply(data: Data): Boolean
}

private[dsentric] trait ApplicativeLens[Data, P] {

  def unapply(data: Data): Option[P]

  def @:[P1, O](prev: ApplicativeLens[Data, P1])(implicit
    ev: ApplicativeLens.Aux[Data, ApplicativeLens[Data, P1] :: ApplicativeLens[Data, P] :: HNil, P1 :: P :: HNil],
    tplP: Tupler.Aux[P1 :: P :: HNil, O],
    genP: Generic.Aux[O, P1 :: P :: HNil]
  ) =
    LensList(prev :: this.asInstanceOf[ApplicativeLens[Data, P]] :: HNil, Nil, ev, tplP, genP)

  def @:(prev: ApplicativeMatcher[Data]) =
    MatcherSingle(List(prev), this)
}

trait Evaluator[Data, L <: HList] {
  type OutP <: HList

  def patternMatch(data: Data, l: L): Option[OutP]
}

object ApplicativeLens {

  type Aux[Data, L <: HList, OP <: HList] =
    Evaluator[Data, L] {
      type OutP = OP
    }

  type E[Data, P] = ApplicativeLens[Data, P]

  implicit def evaluatorHNil[Data]: Aux[Data, HNil, HNil] =
    new Evaluator[Data, HNil] {
      type OutP = HNil

      def patternMatch(data: Data, lp: HNil) = Some(HNil)
    }

  implicit def evalHCons[Data, P, L <: HList](implicit
    evalT: Evaluator[Data, L]
  ): Aux[Data, E[Data, P] :: L, P :: evalT.OutP] =
    new Evaluator[Data, E[Data, P] :: L] {
      type OutP = P :: evalT.OutP

      def patternMatch(data: Data, l: E[Data, P] :: L): Option[OutP] =
        for {
          h <- l.head.unapply(data)
          t <- evalT.patternMatch(data, l.tail)
        } yield h :: t
    }

  implicit class HListExt[Data, T <: HList](val t: T) extends AnyVal {
    def @:[P](prev: ApplicativeLens[Data, P]): ApplicativeLens[Data, P] :: T =
      prev :: t
  }
}

case class LensList[Data, L <: HList, OP <: HList, TP1](
  maybes: L,
  matchers: List[ApplicativeMatcher[Data]],
  ev: ApplicativeLens.Aux[Data, L, OP],
  tplP: Tupler.Aux[OP, TP1],
  genP: Generic.Aux[TP1, OP]
) {

  import shapeless.syntax.std.tuple._

  def unapply(data: Data): Option[TP1] =
    if (matchers.forall(_.unapply(data)))
      ev.patternMatch(data, maybes).map { hl =>
        tplP.apply(hl)
      }
    else
      None

  def unapply(data: Validated[Data]): Some[TP1] =
    Some(
      ev.patternMatch(data.validObject, maybes)
        .map { hl =>
          tplP.apply(hl)
        }
        .get
    )

  final def @:[P, TP2, TS2](
    prev: ApplicativeLens[Data, P]
  )(implicit tplP2: Tupler.Aux[P :: OP, TP2], genP2: Generic.Aux[TP2, P :: OP]) =
    LensList(prev :: maybes, matchers, ApplicativeLens.evalHCons[Data, P, L](ev), tplP2, genP2)

  final def @:(prev: ApplicativeMatcher[Data]) =
    LensList[Data, L, OP, TP1](maybes, prev :: matchers, ev, tplP, genP)
}

case class MatcherSingle[Data, P](matchers: List[ApplicativeMatcher[Data]], lens: ApplicativeLens[Data, P]) {

  def unapply(data: Data): Option[P] =
    if (matchers.forall(_.unapply(data)))
      lens.unapply(data)
    else
      None

  def unapply(data: Validated[Data]): Some[P] =
    Some(lens.unapply(data.validObject).get)

  final def @:[P1, O](prev: ApplicativeLens[Data, P1])(implicit
    ev: ApplicativeLens.Aux[Data, ApplicativeLens[Data, P1] :: ApplicativeLens[Data, P] :: HNil, P1 :: P :: HNil],
    tplP: Tupler.Aux[P1 :: P :: HNil, O],
    genP: Generic.Aux[O, P1 :: P :: HNil]
  ) =
    LensList(prev :: lens :: HNil, matchers, ev, tplP, genP)

  final def @:[O](prev: ApplicativeMatcher[Data]) =
    MatcherSingle(prev :: matchers, lens)
}
