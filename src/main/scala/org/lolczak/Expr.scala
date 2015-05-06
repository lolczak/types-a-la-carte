package org.lolczak

import scala.languageFeature.higherKinds

trait Expr[F[_]]

case class Val[T](value: T) extends Expr[Val]

case class Add[T](val1:T, val2: T) extends Expr[Add]

