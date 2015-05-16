package org.lolczak.alacarte.approach1.expression

import scala.languageFeature.{existentials, higherKinds}
import scalaz.Functor

case class Expr[F[_]](in: F[Expr[F]])

object Expr {

  def foldExpr[A, F[_] : Functor](algebra: F[A] => A)(expr: Expr[F]): A = {
    val functor = implicitly[Functor[F]]
    algebra(functor.map(expr.in)(foldExpr(algebra)))
  }

  def eval[F[_]](expr: Expr[F])(implicit eval: Eval[F], fun:Functor[F]): Int = foldExpr(eval.evalAlgebra)(expr)

}
