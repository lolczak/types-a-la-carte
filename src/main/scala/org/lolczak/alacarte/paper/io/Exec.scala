package org.lolczak.alacarte.paper.io


import org.lolczak.alacarte.paper.calculator.Term
import org.lolczak.alacarte.paper.control.{Inr, Inl, Coproduct}

import scalaz.Functor
import scalaz.effect.IO

trait Exec[F[_]] {

  def functor: Functor[F]

  def execAlgebra[A](expr: F[IO[A]]): IO[A]

}

object Exec extends ExecInstances {

  def exec[F[_], A](term: Term[F, A])(implicit F0: Exec[F]): IO[A] = {
    val pure = (a: A) => IO(a)
    val impure = (fa: F[IO[A]]) => F0.execAlgebra(fa)
    Term.foldTerm[F, A, IO[A]](pure)(impure)(term)(F0.functor)
  }

}

trait ExecInstances {

  implicit def coprodExec[F[_], G[_]](implicit F0: Exec[F], G0: Exec[G]) = new Exec[Coproduct[F, G, ?]] {
    implicit val F1 = F0.functor
    implicit val G1 = G0.functor

    override def functor = implicitly[Functor[Coproduct[F, G, ?]]]

    override def execAlgebra[A](expr: Coproduct[F, G, IO[A]]): IO[A] = expr match {
      case Inl(f) => F0.execAlgebra(f)
      case Inr(g) => G0.execAlgebra(g)
    }
  }

}
