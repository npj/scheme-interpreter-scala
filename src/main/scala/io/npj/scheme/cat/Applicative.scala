package io.npj.scheme.cat

trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
}

object Applicative {
  def apply[F[_]: Functor](implicit f: Applicative[F]): Applicative[F] = f

  object syntax {
    implicit class ApplicativeFunctionOps[F[_]: Functor: Applicative, A, B](self: F[A => B]) {
      def <*>(fa: F[A]): F[B] =
        Applicative[F].ap(self)(fa)
    }

    implicit class ApplicativeOps[F[_]: Functor: Applicative, A](self: F[A]) {
      import Function.const
      import Functor.syntax._

      def *>[B](fb: F[B]): F[B] =
        self.map { (a: A) => (b: B) => const(b)(a) } <*> fb
    }

    def replicateA[F[_]: Functor: Applicative, A](times: Int, action: F[A]): F[Seq[A]] =
      if (times == 0) {
        Applicative[F].pure(Seq())
      } else {
        Applicative[F].pure({ (a: A) => (as: Seq[A]) => a +: as }) <*> action <*> replicateA(times - 1, action)
      }
  }
}
