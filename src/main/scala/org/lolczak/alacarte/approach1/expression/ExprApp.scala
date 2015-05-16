package org.lolczak.alacarte.approach1.expression

import org.lolczak.alacarte.approach1.control.:+:
import Expr._
import :+:._
//todo get rid of this import

object ExprApp extends App {

  type AddExample = Expr[(Val :+: Add)#Plus]

//  val test = Expr[(Val :+: Add)#Plus](Left(Val(1219)))

  val addExample= Expr[(Val :+: Add)#Plus](Right(
    Add(Expr[(Val :+: Add)#Plus](Left(Val(118))), Expr[(Val :+: Add)#Plus](Left(Val(1219))))
  ))

  println("Add example: " + eval[(Val :+: Add)#Plus](addExample))

}
