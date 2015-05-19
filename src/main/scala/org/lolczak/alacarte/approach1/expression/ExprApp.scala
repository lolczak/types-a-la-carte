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

  type T[A] = (Val :+: Add)#Plus[A]

  val dsl1: Expr[T] = sum[T](sum[T](valOf[T](30000), valOf[T](1330)), valOf[T](7))

  println("Dsl example: " + eval[T](dsl1))

  val dsl2: Expr[T]= valOf[T](30000) + valOf[T](1330) + valOf[T](7)

  println("Dsl example: " + eval[T](dsl2))
  
}
