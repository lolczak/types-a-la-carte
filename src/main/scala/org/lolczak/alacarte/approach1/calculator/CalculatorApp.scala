package org.lolczak.alacarte.approach1.calculator

import Incr._
import Recall._
import org.lolczak.alacarte.approach1.calculator.Run.run
import org.lolczak.alacarte.approach1.control._

import scala.languageFeature.{existentials, higherKinds, reflectiveCalls}

object CalculatorApp extends App {

  type T[A] = Coproduct[Recall, Incr, A]

  val tick: Term[T, Int] = for {
    m <- recall[T]
    _ <- incr[T](1)
  } yield m

  println(run(tick)(Mem(4)))

}
