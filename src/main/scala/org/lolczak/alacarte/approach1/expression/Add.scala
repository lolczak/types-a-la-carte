package org.lolczak.alacarte.approach1.expression

import scalaz.Functor

case class Add[T](x: T, y: T)

object Add {

  implicit val addFunctor = new Functor[Add] {
    override def map[A, B](fa: Add[A])(f: (A) => B): Add[B] = Add(f(fa.x), f(fa.y))
  }

  implicit val addEval = new Eval[Add] {
    override def evalAlgebra(expr: Add[Int])(implicit fun: Functor[Add]): Int = expr.x + expr.y
  }

}
