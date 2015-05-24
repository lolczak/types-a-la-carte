package org.lolczak.alacarte.approach1.expression

import org.lolczak.alacarte.approach1.control.{Inr, Inl, Coproduct}

import scalaz.Functor

trait Eval[F[_]] {
  def evalAlgebra(expr: F[Int])(implicit fun: Functor[F]): Int
}

object Eval {

  implicit def coprodEval[F[_], G[_]](implicit fEval: Eval[F], gEval: Eval[G], fFun: Functor[F], gFun: Functor[G]) = new Eval[({type C[A] = Coproduct[F, G, A]})#C] {

    //    override def evalAlgebra(expr: :+:[F, G]#Plus[Int])(implicit fun: Functor[:+:[F, G]#Plus]): Int = expr match {
    //      case Left(x) => fEval.evalAlgebra(x)
    //      case Right(y) => gEval.evalAlgebra(y)
    //    }
    override def evalAlgebra(expr: Coproduct[F, G, Int])(implicit fun: Functor[({type C[A] = Coproduct[F, G, A]})#C]): Int = expr match {
      case Inl(x) => fEval.evalAlgebra(x)
      case Inr(y) => gEval.evalAlgebra(y)
    }
  }

}
