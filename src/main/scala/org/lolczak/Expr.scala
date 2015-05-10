package org.lolczak

import scala.languageFeature.higherKinds

object expr {

  case class In[+F](in: F)

  type Expr[+A] = In[A]

  case class Val[T](value: T)
  case class Add[T](val1:T, val2: T)

  type AddExample = Expr[Val[_] :+: Add[_]]

  val addExample: AddExample = In(Inr(
    Add(In(Inl(Val(118))), In(Inl(Val(1219))))
  ))

}

