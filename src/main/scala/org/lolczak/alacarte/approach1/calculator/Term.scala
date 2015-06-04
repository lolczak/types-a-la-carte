package org.lolczak.alacarte.approach1.calculator

import org.lolczak.alacarte.approach1.control.:<:

import scalaz.{Functor, Monad}

trait Term[F[_], A] {

  def map[B](f: A => B)(implicit F0: Functor[F]): Term[F, B] = Term.functor[F].map(this)(f)

  def flatMap[B](f: A => Term[F, B])(implicit F0: Functor[F]): Term[F, B] = Term.monad[F].bind(this)(f)

}

case class Pure[F[_], A](a: A) extends Term[F, A]

case class Impure[F[_], A](f: F[Term[F, A]]) extends Term[F, A]

object Term extends TermInstances {

  def inject[G[_], F[_], A](in: G[Term[F, A]])(implicit I0: G :<: F): Term[F, A] = Impure[F, A](I0.inj(in))

  def foldTerm[F[_], A, B](pure: A => B)(imp: F[B] => B)(term: Term[F, A])(implicit F0: Functor[F]): B = term match {
    case Pure(x) => pure(x)
    case Impure(x) => imp(F0.map(x)(foldTerm(pure)(imp)))
  }

}

trait TermInstances {

  implicit def functor[F[_]](implicit F0: Functor[F]) = new Functor[Term[F, ?]] {
    override def map[A, B](fa: Term[F, A])(f: (A) => B): Term[F, B] = fa match {
      case Pure(x) => Pure(f(x))
      case i: Impure[F, A] => Impure[F, B](F0.map(i.f)(map(_)(f)))
    }
  }

  implicit def monad[F[_]](implicit F0: Functor[F]) = new Monad[Term[F, ?]] {
    override def bind[A, B](fa: Term[F, A])(f: (A) => Term[F, B]): Term[F, B] = fa match {
      case Pure(x) => f(x)
      case Impure(t) => Impure[F, B](F0.map(t)(bind(_)(f)))
    }

    override def point[A](a: => A): Term[F, A] = Pure(a)
  }

}