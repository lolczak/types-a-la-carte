package org.lolczak

import scala.languageFeature.{existentials, higherKinds}

sealed trait :+:[+F, +G]

case class Inl[+F, +G](head: F) extends :+:[F, G]

case class Inr[+F, +G](tail: G) extends :+:[F, G]
