package io.npj.scheme.cat

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit f: Functor[F]): Functor[F] = f

  object syntax {
    implicit class FunctorOps[F[_]: Functor, A](self: F[A]) {
      def map[B](f: A => B): F[B] =
        Functor[F].map(self)(f)
    }
  }
}

