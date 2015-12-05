package dsentric

import shapeless.ops.hlist.Tupler
import shapeless.syntax.std.TupleOps
import shapeless.{Generic, ::, HNil, HList}

private [dsentric] trait ApplicativeLens[Data, P, S] {

  def unapply(data:Data):Option[P]

  def $set(value:S):Data => Data

  def @:[P1, S1, O](prev:ApplicativeLens[Data, P1, S1])(implicit
                                                       ev: ApplicativeLens.Aux[Data, ApplicativeLens[Data, P1, S1] :: ApplicativeLens[Data, P, S] :: HNil,
                                                       P1 :: P :: HNil, S1 :: S :: HNil],
                                                       tplP:Tupler.Aux[P1 :: P :: HNil, O],
                                                       tplS:Tupler.Aux[S1 :: S :: HNil, O],
                                                       gen: Generic.Aux[O, S1 :: S :: HNil]) =
    LensList(prev :: this.asInstanceOf[ApplicativeLens[Data, P, S]] :: HNil, ev, tplP, tplS, gen)

}

trait Evaluator[Data, L <: HList] {
  type OutP <: HList
  type InS <: HList

  def patternMatch(data:Data, l:L):Option[OutP]

  def set(l:L, s:InS):Data => Data
}

object ApplicativeLens {

  type Aux[Data, L <: HList, OP <: HList, IS <: HList] =
    Evaluator[Data, L]{
      type OutP = OP
      type InS = IS
    }

  type E[Data, P, S] = ApplicativeLens[Data, P, S]

  implicit def evaluatorHNil[Data]:Aux[Data, HNil, HNil, HNil] = new Evaluator[Data, HNil] {
    type OutP = HNil
    type InS = HNil

    def patternMatch(data:Data, lp:HNil) = Some(HNil)

    def set(l:HNil, s:HNil) = Predef.identity[Data]
  }

  implicit def evalHCons[Data, P, S, L <: HList](implicit evalT: Evaluator[Data, L]): Aux[Data, E[Data, P, S] :: L, P :: evalT.OutP, S :: evalT.InS] =
    new Evaluator[Data, E[Data, P, S] :: L] {
      type OutP = P :: evalT.OutP
      type InS = S :: evalT.InS

      def patternMatch(data:Data, l: E[Data, P, S] :: L):Option[OutP] =
        for {
          h <- l.head.unapply(data)
          t <- evalT.patternMatch(data, l.tail)
        } yield h :: t

      def set(l: E[Data, P, S] :: L, s: InS):Data => Data = {
        val h = l.head.$set(s.head)
        val t = evalT.set(l.tail, s.tail)
        t.compose(h)
      }
    }

  implicit class HListExt[Data, T <: HList](val t:T) extends AnyVal {
    def @:[P, S](prev:ApplicativeLens[Data, P, S]):ApplicativeLens[Data, P, S] :: T =
      prev :: t
  }
}

case class LensList[Data, L <: HList, OP <: HList, IS <: HList, TP1, TS1](maybes: L, ev: ApplicativeLens.Aux[Data, L, OP, IS], tplP:Tupler.Aux[OP, TP1], tplS:Tupler.Aux[IS, TS1], gen: Generic.Aux[TS1, IS]) {

  import shapeless.syntax.std.tuple._

  def unapply(data:Data):Option[TP1] = {
    ev.patternMatch(data, maybes).map{hl =>
      tplP.apply(hl)
    }
  }

  def $set(values:TS1):Data => Data = {
    ev.set(maybes, new TupleOps(values).productElements(gen))
  }

  def @:[P, S, TP2, TS2](prev:ApplicativeLens[Data, P, S])(implicit tplP2:Tupler.Aux[P :: OP, TP2], tplS2:Tupler.Aux[S :: IS, TS2], gen2:Generic.Aux[TS2, S :: IS]) =
    LensList(prev :: maybes, ApplicativeLens.evalHCons[Data, P, S, L](ev), tplP2, tplS2, gen2)
}
