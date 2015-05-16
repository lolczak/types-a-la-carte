package org.lolczak.alacarte.approach1.expression

import scalaz.Functor

case class Val[T](value: Int)

object Val {

  implicit val valFunctor = new Functor[Val] {
    override def map[A, B](fa: Val[A])(f: (A) => B): Val[B] = Val[B](fa.value)
  }

  implicit val valEval = new Eval[Val] {
    override def evalAlgebra(expr: Val[Int])(implicit fun: Functor[Val]): Int = expr.value
  }

}