package org.lolczak.alacarte.approach1.expression

import org.lolczak.alacarte.approach1.control.{Inr, Inl, Coproduct}

import scalaz.Functor

trait Eval[F[_]] {
  def evalAlgebra(expr: F[Int])(implicit fun: Functor[F]): Int
}

object Eval {

  implicit def coprodEval[F[_], G[_]](implicit fEval: Eval[F], gEval: Eval[G], fFun: Functor[F], gFun: Functor[G]) = new Eval[Coproduct[F, G, ?]] {
    override def evalAlgebra(expr: Coproduct[F, G, Int])(implicit fun: Functor[Coproduct[F, G, ?]]): Int = expr match {
      case Inl(x) => fEval.evalAlgebra(x)
      case Inr(y) => gEval.evalAlgebra(y)
    }
  }

  //hack
  implicit def coprodNestedEval[F[_], G[_], H[_]](implicit fEval: Eval[F], nestEval: Eval[Coproduct[G,H, ?]], fFun: Functor[F], nestFun: Functor[Coproduct[G, H, ?]]) = new Eval[Coproduct[F, Coproduct[G, H, ?], ?]] {
    override def evalAlgebra(expr: Coproduct[F, Coproduct[G,H, ?], Int])(implicit fun: Functor[Coproduct[F, Coproduct[G,H, ?], ?]]): Int = expr match {
      case left: Inl[F, Coproduct[G,H, ?], Int] => fEval.evalAlgebra(left.f)
      case right: Inr[F, Coproduct[G,H, ?], Int] => nestEval.evalAlgebra(right.g)
    }
  }

}
