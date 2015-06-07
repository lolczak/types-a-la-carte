package org.lolczak.alacarte.paper.expression

trait Render[F[_]] {

  def render[G[_]](expr: F[Expr[G]])(implicit G0: Render[G]): String

}

object Render {

  def pretty[F[_]](expr: Expr[F])(implicit R0: Render[F]): String = R0.render(expr.in)

}
