package org.lolczak.alacarte.approach1.control

trait Inject[F[_], G[_]] {

  def inj[A](f : F[A]): G[A]

}

object InjectInstances {

  implicit def reflexiveInject[F[_]] = new (F :<: F) {
    override def inj[A](f: F[A]): F[A] = f
  }



}
