package org.lolczak.alacarte.paper.expression

import org.lolczak.alacarte.paper.control.:<:

import scalaz.Functor

case class Val[T](value: Int)

object Val {

  implicit val valFunctor = new Functor[Val] {
    override def map[A, B](fa: Val[A])(f: (A) => B): Val[B] = Val[B](fa.value)
  }

  implicit val valEval = new Eval[Val] {
    override def evalAlgebra(expr: Val[Int])(implicit fun: Functor[Val]): Int = expr.value
  }

  def valOf[F[_]](x: Int)(implicit ev: Val :<: F): Expr[F] = Expr.inject(Val[Expr[F]](x))

  implicit val valRender = new Render[Val] {
    override def render[G[_]](expr: Val[Expr[G]])(implicit G0: Render[G]): String = expr.value.toString
  }

}