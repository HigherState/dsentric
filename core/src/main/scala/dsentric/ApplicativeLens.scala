package dsentric

import shapeless.ops.hlist.Tupler
import shapeless.syntax.std.TupleOps
import shapeless.{Generic, ::, HNil, HList}

private [dsentric] trait ApplicativeMatcher[Data] {
  def unapply(data:Data):Boolean
}

//private [dsentric] trait ApplicativeLens[Data, P, S] {
//
//  def unapply(data:Data):Option[P]
//
//  def $set(value:S):Data => Data
//
//  def @:[P1, S1, O](prev:ApplicativeLens[Data, P1, S1])(implicit
//                                                       ev: ApplicativeLens.Aux[Data, ApplicativeLens[Data, P1, S1] :: ApplicativeLens[Data, P, S] :: HNil, P1 :: P :: HNil, S1 :: S :: HNil],
//                                                       tplP:Tupler.Aux[P1 :: P :: HNil, O],
//                                                       tplS:Tupler.Aux[S1 :: S :: HNil, O],
//                                                       genP: Generic.Aux[O, P1 :: P :: HNil],
//                                                       genS: Generic.Aux[O, S1 :: S :: HNil]) =
//    LensList(prev :: this.asInstanceOf[ApplicativeLens[Data, P, S]] :: HNil, Nil, ev, tplP, tplS, genP, genS)
//
//  def @:[O](prev:ApplicativeMatcher[Data]) =
//    MatcherSingle(List(prev),this)
//}

private [dsentric] trait ApplicativeLens[Data, P] {

  def unapply(data:Data):Option[P]


  def @:[P1, O](prev:ApplicativeLens[Data, P1])(implicit
                                                        ev: ApplicativeLens.Aux[Data, ApplicativeLens[Data, P1] :: ApplicativeLens[Data, P] :: HNil, P1 :: P :: HNil],
                                                        tplP:Tupler.Aux[P1 :: P :: HNil, O],
                                                        genP: Generic.Aux[O, P1 :: P :: HNil]) =
    LensList(prev :: this.asInstanceOf[ApplicativeLens[Data, P]] :: HNil, Nil, ev, tplP, genP)

  def @:(prev:ApplicativeMatcher[Data]) =
    MatcherSingle(List(prev),this)
}

//trait Evaluator[Data, L <: HList] {
//  type OutP <: HList
//  type InS <: HList
//
//  def patternMatch(data:Data, l:L):Option[OutP]
//
//  def set(l:L, s:InS):Data => Data
//}

trait Evaluator[Data, L <: HList] {
  type OutP <: HList

  def patternMatch(data:Data, l:L):Option[OutP]
}

//object ApplicativeLens {
//
//  type Aux[Data, L <: HList, OP <: HList, IS <: HList] =
//  Evaluator[Data, L]{
//    type OutP = OP
//    type InS = IS
//  }
//
//  type E[Data, P, S] = ApplicativeLens[Data, P, S]
//
//  implicit def evaluatorHNil[Data]:Aux[Data, HNil, HNil, HNil] = new Evaluator[Data, HNil] {
//    type OutP = HNil
//    type InS = HNil
//
//    def patternMatch(data:Data, lp:HNil) = Some(HNil)
//
//    def set(l:HNil, s:HNil) = Predef.identity[Data]
//  }
//
//  implicit def evalHCons[Data, P, S, L <: HList](implicit evalT: Evaluator[Data, L]): Aux[Data, E[Data, P, S] :: L, P :: evalT.OutP, S :: evalT.InS] =
//    new Evaluator[Data, E[Data, P, S] :: L] {
//      type OutP = P :: evalT.OutP
//      type InS = S :: evalT.InS
//
//      def patternMatch(data:Data, l: E[Data, P, S] :: L):Option[OutP] =
//        for {
//          h <- l.head.unapply(data)
//          t <- evalT.patternMatch(data, l.tail)
//        } yield h :: t
//
//      def set(l: E[Data, P, S] :: L, s: InS):Data => Data = {
//        val h = l.head.$set(s.head)
//        val t = evalT.set(l.tail, s.tail)
//        t.compose(h)
//      }
//    }
//
//  implicit class HListExt[Data, T <: HList](val t:T) extends AnyVal {
//    def @:[P, S](prev:ApplicativeLens[Data, P, S]):ApplicativeLens[Data, P, S] :: T =
//      prev :: t
//  }
//}

object ApplicativeLens {

  type Aux[Data, L <: HList, OP <: HList] =
  Evaluator[Data, L]{
    type OutP = OP
  }

  type E[Data, P] = ApplicativeLens[Data, P]

  implicit def evaluatorHNil[Data]:Aux[Data, HNil, HNil] =
    new Evaluator[Data, HNil] {
      type OutP = HNil

      def patternMatch(data:Data, lp:HNil) = Some(HNil)
    }

  implicit def evalHCons[Data, P, L <: HList](implicit evalT: Evaluator[Data, L]): Aux[Data, E[Data, P] :: L, P :: evalT.OutP] =
    new Evaluator[Data, E[Data, P] :: L] {
      type OutP = P :: evalT.OutP

      def patternMatch(data:Data, l: E[Data, P] :: L):Option[OutP] =
        for {
          h <- l.head.unapply(data)
          t <- evalT.patternMatch(data, l.tail)
        } yield h :: t
    }

  implicit class HListExt[Data, T <: HList](val t:T) extends AnyVal {
    def @:[P](prev:ApplicativeLens[Data, P]):ApplicativeLens[Data, P] :: T =
      prev :: t
  }
}

//case class LensList[Data, L <: HList, OP <: HList, IS <: HList, TP1, TS1](maybes: L, matchers:List[ApplicativeMatcher[Data]], ev: ApplicativeLens.Aux[Data, L, OP, IS], tplP:Tupler.Aux[OP, TP1], tplS:Tupler.Aux[IS, TS1], genP: Generic.Aux[TP1, OP], genS: Generic.Aux[TS1, IS]) {
//
//  import shapeless.syntax.std.tuple._
//
//  def unapply(data:Data):Option[TP1] =
//    if (matchers.forall(_.unapply(data)))
//      ev.patternMatch(data, maybes).map{hl =>
//        tplP.apply(hl)
//      }
//    else
//      None
//
//  def $set(values:TS1):Data => Data = {
//    ev.set(maybes, new TupleOps(values).productElements(genS))
//  }
//
//  def @:[P, S, TP2, TS2](prev:ApplicativeLens[Data, P, S])(implicit tplP2:Tupler.Aux[P :: OP, TP2], tplS2:Tupler.Aux[S :: IS, TS2], genP2:Generic.Aux[TP2, P :: OP], genS2:Generic.Aux[TS2, S :: IS]) =
//    LensList(prev :: maybes, matchers, ApplicativeLens.evalHCons[Data, P, S, L](ev), tplP2, tplS2, genP2, genS2)
//
//  def @:(prev:ApplicativeMatcher[Data]) =
//    LensList[Data, L, OP, IS, TP1, TS1](maybes, prev :: matchers, ev, tplP, tplS, genP, genS)
//}

case class LensList[Data, L <: HList, OP <: HList, TP1](maybes: L, matchers:List[ApplicativeMatcher[Data]], ev: ApplicativeLens.Aux[Data, L, OP], tplP:Tupler.Aux[OP, TP1], genP: Generic.Aux[TP1, OP]) {

  import shapeless.syntax.std.tuple._

  def unapply(data:Data):Option[TP1] =
    if (matchers.forall(_.unapply(data)))
      ev.patternMatch(data, maybes).map{hl =>
        tplP.apply(hl)
      }
    else
      None


  def @:[P, TP2, TS2](prev:ApplicativeLens[Data, P])(implicit tplP2:Tupler.Aux[P :: OP, TP2], genP2:Generic.Aux[TP2, P :: OP]) =
    LensList(prev :: maybes, matchers, ApplicativeLens.evalHCons[Data, P, L](ev), tplP2, genP2)

  def @:(prev:ApplicativeMatcher[Data]) =
    LensList[Data, L, OP, TP1](maybes, prev :: matchers, ev, tplP, genP)
}



case class MatcherSingle[Data, P](matchers:List[ApplicativeMatcher[Data]], lens:ApplicativeLens[Data, P]) {

  def unapply(data:Data):Option[P] =
    if (matchers.forall(_.unapply(data)))
      lens.unapply(data)
    else
      None

  def @:[P1, O](prev:ApplicativeLens[Data, P1])(implicit
                                                        ev: ApplicativeLens.Aux[Data, ApplicativeLens[Data, P1] :: ApplicativeLens[Data, P] :: HNil, P1 :: P :: HNil],
                                                        tplP:Tupler.Aux[P1 :: P :: HNil, O],
                                                        genP: Generic.Aux[O, P1 :: P :: HNil]) =
    LensList(prev :: lens :: HNil, matchers, ev, tplP, genP)

  def @:[O](prev:ApplicativeMatcher[Data]) =
    MatcherSingle(prev :: matchers, lens)
}
