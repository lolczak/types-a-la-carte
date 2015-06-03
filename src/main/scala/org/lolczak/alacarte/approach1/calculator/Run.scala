package org.lolczak.alacarte.approach1.calculator

import org.lolczak.alacarte.approach1.control.{Inr, Inl, Coproduct}

trait Run[F[_]] {

  def runAlgebra[A](expr: F[Mem => (A, Mem)])(mem: Mem): (A, Mem)

}

object Run {

  implicit def coprodRun[F[_], G[_]](implicit F0: Run[F], G0: Run[G]) = new Run[Coproduct[F, G, ?]] {


    //    override def runAlgebra[A](expr: Coproduct[F, G, (Mem) => (A, Mem)]): (Mem) => (A, Mem) = expr match {
    //      case Inl(r) => F0.runAlgebra(r)
    //      case Inr(r) => G0.runAlgebra(r)
    //    }
    override def runAlgebra[A](expr: Coproduct[F, G, (Mem) => (A, Mem)])(mem: Mem): (A, Mem) = expr match {
      case Inl(r) => F0.runAlgebra(r)(mem)
      case Inr(r) => G0.runAlgebra(r)(mem)
    }
  }

}
