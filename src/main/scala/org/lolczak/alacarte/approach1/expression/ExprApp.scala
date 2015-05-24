package org.lolczak.alacarte.approach1.expression

import org.lolczak.alacarte.approach1.control._
import Expr._
import Val._
import Add._
import Mul._
import CoproductInstances._
import InjectInstances._

//todo get rid of this import

object ExprApp extends App {

//  type T[A] = (Val :+: Add :+: CNil)#Plus[A]


  type T[A] = Coproduct[Val, Add, A]

//  val addExample= Expr[T](Inr[Val, Add, Int](
//    Add(Expr[T](Inl[Val, Add, Int](Val(118))), Expr[T](Inl[Val, Add, Int](Val(1219))))
//  ))
//
//  println("Add example: " + eval[(Val :+: Add)#Plus](addExample))

  val dsl1: Expr[T] = sum[T](sum[T](valOf[T](30000), valOf[T](1330)), valOf[T](7))

  println("Dsl example: " + eval[T](dsl1))

  val dsl2: Expr[T]= valOf[T](30000) |+| valOf[T](1330) |+| valOf[T](7)

  println("Dsl example: " + eval[T](dsl2))

//  type U[A] = (Val :+: (Add :+: Mul)#Plus)#Plus[A]
//
//  val x = valOf[(Val :+: (Add :+: Mul)#Plus)#Plus](13)
//
//  type U[A] = Coproduct[Val, ({type C[x] = Coproduct[Add,Mul,x]})#C, A]
//  val test = valOf[({ type G[A]=Coproduct[Val, ({type C[x] = Coproduct[Add,Mul,x]})#C, A]})#G](123)(leftExplicitlySupportingInject[Val, ({type C[x] = Coproduct[Add,Mul,x]})#C])
//  val mulExample1 = sum[U](mul[U](valOf[U](80), valOf[U](5)), valOf[U](4))
////  val mulExample = valOf[U](80) * valOf[U](5) + valOf[U](4)
//
//  println("Mul example: " + eval[U](mulExample1))


}
