package org.lolczak.alacarte.paper.control

trait Inject[F[_], G[_]] {

  def inj[A](f: F[A]): G[A]

}

object Inject extends InjectInstances

trait InjectInstances {

  implicit def reflexiveInject[F[_]] = new (F :<: F) {
    override def inj[A](f: F[A]): F[A] = f
  }

  implicit def leftExplicitlySupportingInject[F[_], G[_]] = new (F :<: Coproduct[F, G, ?]) {
    override def inj[A](f: F[A]): Coproduct[F, G, A] = Inl[F, G, A](f)
  }

  implicit def rightImplicitlySupportingInject[F[_], G[_], H[_]](implicit ev: F :<: G) = new (F :<: Coproduct[H, G, ?]) {
    override def inj[A](f: F[A]): Coproduct[H, G, A] = Inr[H, G, A](ev.inj(f))
  }

  //hacks
  implicit def leftExplicitNested[F[_], G[_], H[_]] = new (F :<: Coproduct[F, Coproduct[G,H, ?], ?]) {
    override def inj[A](f: F[A]) = Inl[F, Coproduct[G, H, ?], A](f)
  }

  implicit def rightNested[F[_], G[_], H[_], I[_]](implicit ev: F :<: Coproduct[H,I, ?]) = new (F :<: Coproduct[G, Coproduct[H,I, ?], ?]) {
    override def inj[A](f: F[A]) = Inr[G, Coproduct[H,I, ?], A](ev.inj(f))
  }

}
