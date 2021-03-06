package org.lolczak.alacarte.paper.expression

import org.lolczak.alacarte.paper.control.:<:
import org.lolczak.alacarte.paper.expression.Render

import scalaz.Functor

case class Add[T](x: T, y: T)

object Add {

  implicit val addFunctor = new Functor[Add] {
    override def map[A, B](fa: Add[A])(f: (A) => B): Add[B] = Add(f(fa.x), f(fa.y))
  }

  implicit val addEval = new Eval[Add] {
    override def evalAlgebra(expr: Add[Int])(implicit fun: Functor[Add]): Int = expr.x + expr.y
  }

  def sum[F[_]](x: Expr[F], y: Expr[F])(implicit ev: Add :<: F): Expr[F] = Expr.inject(Add(x,y))

  implicit class AddOps[F[_]](x: Expr[F])(implicit ev: Add :<: F) {
    def |+|(y: Expr[F]) = sum[F](x,y)
  }

  implicit val addRender = new Render[Add] {
    override def render[G[_]](expr: Add[Expr[G]])(implicit G0: Render[G]): String =
      "(" + Render.pretty(expr.x) + " + " + Render.pretty(expr.y) + ")"
  }

}
