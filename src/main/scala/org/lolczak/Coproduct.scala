package org.lolczak

import scala.languageFeature.{existentials, higherKinds}

sealed trait :+:[+F[_], +G[_]]

case class Inl[+F[_], +G[_]](head: F[_]) extends :+:[F, G]

case class Inr[+F[_], +G[_]](tail: G[_]) extends :+:[F, G]
