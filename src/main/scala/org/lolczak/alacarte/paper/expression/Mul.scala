package org.lolczak.alacarte.paper.expression

import org.lolczak.alacarte.paper.control.:<:

import scalaz.Functor

case class Mul[F](x: F, y: F)

object Mul {

  implicit val mulFunctor = new Functor[Mul] {
    override def map[A, B](fa: Mul[A])(f: (A) => B): Mul[B] = Mul(f(fa.x), f(fa.y))
  }

  implicit val mulAlgebra = new Eval[Mul] {
    override def evalAlgebra(expr: Mul[Int])(implicit fun: Functor[Mul]): Int = expr.x * expr.y
  }

  def mul[F[_]](x: Expr[F], y: Expr[F])(implicit ev: Mul :<: F): Expr[F] = Expr.inject(Mul(x,y))

  implicit class MulOps[F[_]](x: Expr[F])(implicit ev: Mul :<: F) {

    def |*|(y: Expr[F]): Expr[F] = mul[F](x,y)

  }

  implicit val mulRender = new Render[Mul] {
    override def render[G[_]](expr: Mul[Expr[G]])(implicit G0: Render[G]): String =
      "(" + Render.pretty(expr.x) + " * " + Render.pretty(expr.y) + ")"
  }

}