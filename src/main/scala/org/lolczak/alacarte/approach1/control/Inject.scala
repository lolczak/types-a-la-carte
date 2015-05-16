package org.lolczak.alacarte.approach1.control

trait Inject[F[_], G[_]] {

  def inj[A](f : F[A]): G[A]

}

object InjectInstances {

  implicit def reflexiveInject[F[_]] = new (F :<: F) {
    override def inj[A](f: F[A]): F[A] = f
  }

  implicit def leftExplicitlySupportingInject[F[_],G[_]] = new (F :<: (F :+: G)#Plus) {
    override def inj[A](f: F[A]): :+:[F, G]#Plus[A] = Left(f)
  }

  implicit def rightImplicitlySupportingInject[F[_], G[_], H[_]](implicit ev: F :<: G) = new (F :<: (H :+: G)#Plus) {
    override def inj[A](f: F[A]): :+:[H, G]#Plus[A] = Right(ev.inj(f))
  }

}
