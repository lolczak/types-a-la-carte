package org.lolczak.alacarte.approach1.control

import scala.languageFeature.{existentials, higherKinds, reflectiveCalls}
import scalaz.Functor

sealed trait PlusSyntax {
  type Plus[A]
}

sealed trait :+:[H[_], T <: PlusSyntax] extends PlusSyntax {
  type Plus[A] = Coproduct[H, T#Plus, A]
}

sealed trait CNil extends PlusSyntax {
  type Plus[A] = CNone[A]
}

class CNone[A]

sealed trait Coproduct[F[_], G[_], A]
case class Inl[F[_], G[_], A](f: F[A]) extends Coproduct[F, G, A]
case class Inr[F[_], G[_], A](g: G[A]) extends Coproduct[F, G, A]


object CoproductInstances {

  implicit def coFunctor[F[_], G[_]](implicit F0: Functor[F], G0: Functor[G]) = new Functor[({type C[A] = Coproduct[F,G,A]})#C] {
    override def map[A, B](fa: Coproduct[F, G, A])(f: (A) => B): Coproduct[F, G, B] = fa match {
      case Inl(ff) => Inl[F,G,B](F0.map(ff)(f))
      case Inr(gg) => Inr[F,G,B](G0.map(gg)(f))
    }
  }

  //hack
  implicit def coprodNestedEval[F[_], G[_], H[_]](implicit F0: Functor[F], G0: Functor[Coproduct[G, H, ?]]) = new Functor[Coproduct[F, Coproduct[G, H, ?], ?]] {
    override def map[A, B](fa: Coproduct[F, Coproduct[G, H, ?], A])(f: (A) => B): Coproduct[F, Coproduct[G, H, ?], B] = fa match {
      case left : Inl[F, Coproduct[G, H, ?], A] => Inl[F, Coproduct[G, H, ?], B](F0.map(left.f)(f))
      case right: Inr[F, Coproduct[G, H, ?], A] => Inr[F, Coproduct[G, H, ?], B](G0.map(right.g)(f))
    }
  }

}



