package org.lolczak.alacarte.paper.calculator

import org.lolczak.alacarte.paper.control.{Inr, Inl, Coproduct}

import scalaz.Functor

trait Run[F[_]] {

  def runAlgebra[A](expr: F[Mem => (A, Mem)])(mem: Mem): (A, Mem)

}

object Run extends RunInstances {

  def run[F[_], A](expr: Term[F, A])(mem: Mem)(implicit F0: Run[F], F1: Functor[F]): (A, Mem) = {
    val pure= (a:A) => (m: Mem) => (a, m)
    val impure = (fa: F[Mem => (A, Mem)]) => F0.runAlgebra(fa)(_)
    Term.foldTerm[F, A, Mem => (A, Mem)](pure)(impure)(expr).apply(mem)
  }

}

trait RunInstances {

  implicit def coprodRun[F[_], G[_]](implicit F0: Run[F], G0: Run[G]) = new Run[Coproduct[F, G, ?]] {

    override def runAlgebra[A](expr: Coproduct[F, G, (Mem) => (A, Mem)])(mem: Mem): (A, Mem) = expr match {
      case Inl(r) => F0.runAlgebra(r)(mem)
      case Inr(r) => G0.runAlgebra(r)(mem)
    }
  }

}
