package org.lolczak.alacarte.approach1.calculator

import org.lolczak.alacarte.approach1.control.:<:

import scalaz.Functor

case class Recall[T](r: Int => T)

object Recall extends RecallInstances {

  def recall[F[_]](implicit I0: Recall :<: F): Term[F, Int] = Term.inject[Recall, F, Int](Recall(Pure(_)))

}

trait RecallInstances {

  implicit val recallFunctor = new Functor[Recall] {
    override def map[A, B](fa: Recall[A])(f: (A) => B): Recall[B] = Recall(fa.r andThen f)
  }

  implicit val recallRun = new Run[Recall] {
    override def runAlgebra[A](expr: Recall[(Mem) => (A, Mem)])(mem: Mem): (A, Mem) = expr.r(mem.i)(mem)
  }

}