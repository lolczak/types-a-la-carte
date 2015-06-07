package org.lolczak.alacarte.paper.io

import org.lolczak.alacarte.paper.calculator.{Pure, Term}
import org.lolczak.alacarte.paper.control._

import scalaz.Functor
import scalaz.effect.IO

sealed trait Teletype[A]

case class GetChar[A](f: Char => A) extends Teletype[A]

case class PutChar[A](c: Char, a: A) extends Teletype[A]

object Teletype extends TeletypeInstances {

  def putChar[F[_]](char: Char)(implicit ev: Teletype :<: F): Term[F, Unit] = Term.inject[Teletype, F, Unit](PutChar(char, Pure()))

}

trait TeletypeInstances {

 implicit val teletypeFunctor = new Functor[Teletype] {
   override def map[A, B](fa: Teletype[A])(f: (A) => B): Teletype[B] = fa match {
     case PutChar(c, a) => PutChar(c, f(a))
     case GetChar(g) => ???
   }
 }

  implicit val teletypeExec = new Exec[Teletype] {
    override def functor: Functor[Teletype] = teletypeFunctor

    override def execAlgebra[A](expr: Teletype[IO[A]]): IO[A] = expr match {
      case PutChar(c, a) => IO.putChar(c) flatMap(_ => a)
      case GetChar(g) => ???
    }
  }

}


