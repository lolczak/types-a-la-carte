package org.lolczak.alacarte.paper.control

sealed trait CList {
  type Plus[A]
}

class :++:[F[_], G <: CList] extends CList {

  type Plus[A] = Coproduct[F, G#Plus, A]

}

class :+:[F[_], G[_]] extends CList {

  type Plus[A] = Coproduct[F, G, A]

}


