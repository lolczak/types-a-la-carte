package org.lolczak.alacarte.approach1.expression

import org.lolczak.alacarte.approach1.control.:+:

import scalaz.Functor

trait Eval[F[_]] {
  def evalAlgebra(expr: F[Int])(implicit fun: Functor[F]): Int
}

object Eval {

  implicit def coprodEval[F[_], G[_]](implicit fEval: Eval[F], gEval: Eval[G], fFun: Functor[F], gFun: Functor[G]) = new Eval[(F :+: G)#Plus] {

    override def evalAlgebra(expr: :+:[F, G]#Plus[Int])(implicit fun: Functor[:+:[F, G]#Plus]): Int = expr match {
      case Left(x) => fEval.evalAlgebra(x)
      case Right(y) => gEval.evalAlgebra(y)
    }

  }

}
