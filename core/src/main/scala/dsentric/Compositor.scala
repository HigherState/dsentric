package dsentric

import shapeless.ops.hlist.Tupler
import shapeless.{::, HNil, HList}

private [dsentric] trait ComposableLens[Data, Out] {

  def unapply(data:Data):Option[Out]

  //def $set(value:In):Data => Data

  def @:[S, O](prev:ComposableLens[Data, S])(implicit ev: Compositor.Aux[Data, ComposableLens[Data, S] :: ComposableLens[Data, Out] :: HNil, S :: Out :: HNil], tpl:Tupler.Aux[S :: Out :: HNil, O]) =
    LensList(prev :: this.asInstanceOf[ComposableLens[Data, Out]] :: HNil, ev, tpl)
}

trait Evaluator[Data, L <: HList] {
  type Out <: HList

  def apply(json:Data, l:L):Option[Out]
}

object Compositor {

  type Aux[Data, T <: HList, O <: HList] = Evaluator[Data, T]{ type Out = O }

  type E[Data, T] = ComposableLens[Data, T]

  implicit def evaluatorHNil[Data]:Aux[Data, HNil, HNil] = new Evaluator[Data, HNil] {
    type Out = HNil

    def apply(json:Data, l:HNil) = Some(HNil)
  }

  implicit def evalHCons[Data, H, T <: HList](implicit evalT: Evaluator[Data, T]): Aux[Data, E[Data, H] :: T, H :: evalT.Out] =
    new Evaluator[Data, E[Data, H] :: T] {
      type Out = H :: evalT.Out

      def apply(data:Data, l: E[Data, H] :: T):Option[Out] =
        for {
          h <- l.head.unapply(data)
          t <- evalT(data, l.tail)
        } yield h :: t
    }

  implicit class HListExt[Data, T <: HList](val t:T) extends AnyVal {
    def @:[S](prev:ComposableLens[Data, S]):ComposableLens[Data, S] :: T =
      prev :: t
  }
}

case class LensList[Data, L <: HList, O <: HList, T](maybes: L, ev: Compositor.Aux[Data, L, O], tpl:Tupler.Aux[O, T]) {

  def unapply(data:Data):Option[T] = {
    ev.apply(data, maybes).map{hl =>
      tpl.apply(hl)
    }
  }

  //TODO: add $set method, look at FnFromProduct

  def @:[S, T2](prev:ComposableLens[Data,S])(implicit tpl2:Tupler.Aux[S :: O, T2]) =
    LensList(prev :: maybes, Compositor.evalHCons[Data, S, L](ev), tpl2)
}
