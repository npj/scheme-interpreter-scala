package io.npj.scheme

object cat {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
  }

  trait Alternative[F[_]] extends Applicative[F] {
    def empty[A]: F[A]
    def orElse[A](fa1: F[A])(fa2: F[A]): F[A]
  }

  trait Monad[M[_]] extends Applicative[M] {
    import Function.const
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def andThen[A, B](ma: M[A])(mb: M[B]): M[B] = flatMap(ma)(const(mb))
  }

  object implicits {
    implicit class FunctorOps[F[_], A](private val self: F[A])(implicit val F: Functor[F]) {
      def map[B](f: A => B): F[B] = F.map(self)(f)
    }

    implicit class MonadOps[M[_], A](private val self: M[A])(implicit val M: Monad[M]) {
      def >>=[B](f: A => M[B]): M[B] = M.flatMap(self)(f)
      def >>[B](m: M[B]): M[B] = M.andThen(self)(m)
    }
  }
}
