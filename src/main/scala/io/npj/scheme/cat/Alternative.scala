package io.npj.scheme.cat

trait Alternative[F[_]] {
  def empty[A]: F[A]
  def orElse[A](fa1: F[A])(fa2: F[A]): F[A]
}

object Alternative {
  def apply[F[_]: Functor: Applicative](implicit f: Alternative[F]): Alternative[F] = f

  object syntax {
    implicit class AlternativeOps[F[_]: Functor: Applicative: Alternative, A](self: F[A]) {
      def <|>(fa: F[A]): F[A] =
        Alternative[F].orElse(self)(fa)
    }
  }
}
