package org.lolczak.alacarte.paper.expression

import org.lolczak.alacarte.paper.control._
import org.lolczak.alacarte.paper.expression.Add._
import org.lolczak.alacarte.paper.expression.Expr._
import org.lolczak.alacarte.paper.expression.Mul._
import org.lolczak.alacarte.paper.expression.Val._
import Render._

import scala.languageFeature.{existentials, higherKinds, reflectiveCalls}

object ExprApp extends App {

  type T[A] =  (Val :+: Add)#Plus[A]

  //  val addExample= Expr[T](Inr[Val, Add, Int](
  //    Add(Expr[T](Inl[Val, Add, Int](Val(118))), Expr[T](Inl[Val, Add, Int](Val(1219))))
  //  ))
  //
  //  println("Add example: " + eval[(Val :+: Add)#Plus](addExample))
  //  type U[A] = (Val :+: (Add :+: Mul)#Plus)#Plus[A]
  //
  //  val x = valOf[(Val :+: (Add :+: Mul)#Plus)#Plus](13)
  //
//  type U[A] = Coproduct[Val, ({type C[x] = Coproduct[Add, Mul, x]})#C, A]
//  type U[A] = Coproduct[Val,  Coproduct[Add, Mul, ?], A]
  type U[A] = (Val :++: Add :+: Mul)#Plus[A]


  val addExample1: Expr[T] = sum[T](sum[T](valOf[T](30000), valOf[T](1330)), valOf[T](7))
  println("Add example: " + pretty(addExample1) + " = " + eval(addExample1))

  val addExample2: Expr[T] = valOf[T](30000) |+| valOf[T](1330) |+| valOf[T](7)
  println("Add example (with ops): " + pretty(addExample2) + " = " + eval(addExample2))

  val mulExample1 = sum[U](mul[U](valOf[U](80), valOf[U](5)), valOf[U](4))
  println("Mul example: " + pretty(mulExample1) + " = " + eval[U](mulExample1))

  val mulExample2 = valOf[U](80) |*| valOf[U](5) |+| valOf[U](4)
  println("Mul example (with ops): " + pretty(mulExample2) + " = " + eval[U](mulExample2))

}
