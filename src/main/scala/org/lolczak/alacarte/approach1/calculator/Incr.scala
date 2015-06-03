package org.lolczak.alacarte.approach1.calculator

import org.lolczak.alacarte.approach1.control.:<:

import scalaz.Functor

case class Incr[T](i: Int, t: T)

object Incr {

  def incr[F[_]](i: Int)(implicit I0: Incr :<: F): Term[F, Unit] = Term.inject[Incr, F, Unit](Incr(i, Pure(())))

}

object IncrInstances {

  implicit val incrFunctor = new Functor[Incr] {
    override def map[A, B](fa: Incr[A])(f: (A) => B): Incr[B] = Incr(fa.i, f(fa.t))
  }

}

