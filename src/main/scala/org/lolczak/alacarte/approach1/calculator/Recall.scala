package org.lolczak.alacarte.approach1.calculator

import org.lolczak.alacarte.approach1.control.:<:

import scalaz.Functor

case class Recall[T](f: Int => T)

object Recall {

  def recall[F[_]](implicit I0: Recall :<: F): Term[F, Int] = Term.inject[Recall, F, Int](Recall(Pure(_)))

}

object RecallInstances {

  implicit val recallFunctor = new Functor[Recall] {
    override def map[A, B](fa: Recall[A])(f: (A) => B): Recall[B] = Recall(fa.f andThen f)
  }

}