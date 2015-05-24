package org.lolczak.alacarte.approach1.control


trait Inject[F[_], G[_]] {

  def inj[A](f: F[A]): G[A]

}

object InjectInstances {

  implicit def reflexiveInject[F[_]] = new (F :<: F) {
    println("found1")
    override def inj[A](f: F[A]): F[A] = f
  }

  implicit def leftExplicitlySupportingInject[F[_], G[_]]: :<:[F, ({type C[A] = Coproduct[F, G, A]})#C] = new (F :<: ({type C[A] = Coproduct[F, G, A]})#C) {
    println("found2")
    //    override def inj[A](f: F[A]): :+:[F, G]#Plus[A] = Left(f)
    override def inj[A](f: F[A]): Coproduct[F, G, A] = Inl[F, G, A](f)
  }

  implicit def rightImplicitlySupportingInject[F[_], G[_], H[_]](implicit ev: F :<: G) = new (F :<: ({type C[A] = Coproduct[H, G, A]})#C) {
    print("found3")
    //    override def inj[A](f: F[A]): :+:[H, G]#Plus[A] = Right(ev.inj(f))
    override def inj[A](f: F[A]): Coproduct[H, G, A] = Inr[H, G, A](ev.inj(f))
  }

}
