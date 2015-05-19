package org.lolczak.alacarte.approach1.expression

import org.lolczak.alacarte.approach1.control.{InjectInstances, :+:}
import Expr._
import Val._
import Add._
import :+:._
import InjectInstances._
//todo get rid of this import

object ExprApp extends App {

  type AddExample = Expr[(Val :+: Add)#Plus]

  val addExample= Expr[(Val :+: Add)#Plus](Right(
    Add(Expr[(Val :+: Add)#Plus](Left(Val(118))), Expr[(Val :+: Add)#Plus](Left(Val(1219))))
  ))

  println("Add example: " + eval[(Val :+: Add)#Plus](addExample))

  val dslExample: Expr[(Val :+: Add)#Plus] = sum[(Val :+: Add)#Plus](sum[(Val :+: Add)#Plus](toVal[(Val :+: Add)#Plus](30000), toVal[(Val :+: Add)#Plus](1330)), toVal[(Val :+: Add)#Plus](7))

  println("Dsl example: " + eval[(Val :+: Add)#Plus](dslExample))

}
