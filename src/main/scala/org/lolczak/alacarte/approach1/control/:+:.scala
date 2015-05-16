package org.lolczak.alacarte.approach1.control

import scala.languageFeature.{existentials, higherKinds}
import scalaz.Functor

trait :+:[F[_], G[_]] {

  type Plus[B] = Either[F[B], G[B]]

}


object :+: {

  //  def Inl[F[_], G](fa: F[_]): (F :+: G)#Plus = Left(fa)
  ////
  ////  def Inr[G[_], A](ga: G[A])(implicit ev: G <:< G[A]): (Nothing :+: G)#Plus[A] = \/-(ga)
  //
  implicit def coFunctor[F[_], G[_]](implicit fFun: Functor[F], gFun: Functor[G]) = new Functor[:+:[F, G]#Plus] {

    override def map[A, B](plus: Either[F[A], G[A]])(f: (A) => B): Either[F[B], G[B]] = plus match {
      case Left(fa) => Left(fFun.map(fa)(f))
      case Right(fb) => Right(gFun.map(fb)(f))
    }
  }

}




