package org.lolczak.alacarte.approach1.expression

import org.lolczak.alacarte.approach1.control.CoproductInstances._
import org.lolczak.alacarte.approach1.control.InjectInstances._
import org.lolczak.alacarte.approach1.control._
import org.lolczak.alacarte.approach1.expression.Add._
import org.lolczak.alacarte.approach1.expression.Expr._
import org.lolczak.alacarte.approach1.expression.Mul._
import org.lolczak.alacarte.approach1.expression.Val._
import Render._

import scala.languageFeature.{existentials, higherKinds, reflectiveCalls}

//todo get rid of this import

object ExprApp extends App {

  //  type T[A] = (Val :+: Add :+: CNil)#Plus[A]

  type T[A] = Coproduct[Val, Add, A]

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
  type U[A] = Coproduct[Val,  Coproduct[Add, Mul, ?], A]


  val addExample1: Expr[T] = sum[T](sum[T](valOf[T](30000), valOf[T](1330)), valOf[T](7))
  println("Add example: " + pretty(addExample1) + " = "+ eval[T](addExample1))

  val addExample2: Expr[T] = valOf[T](30000) |+| valOf[T](1330) |+| valOf[T](7)
  println("Add example (with ops): " + eval[T](addExample2))

  val test = valOf[U](23)

  val mulExample1 = sum[U](mul[U](valOf[U](80), valOf[U](5)), valOf[U](4))

  println("Mul example: " + eval[U](mulExample1))

  val mulExample2 = valOf[U](80) |*| valOf[U](5) |+| valOf[U](4)

  println("Mul example (with ops): " + eval[U](mulExample2))

}
