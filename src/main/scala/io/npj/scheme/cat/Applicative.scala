package io.npj.scheme.cat

trait Applicative[F[_]] {
  val F: Functor[F]
  def pure[A](a: A): F[A]
  def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
}

object Applicative {
  import Cons.cons

  def apply[F[_]](implicit f: Applicative[F]): Applicative[F] = f

  object syntax {
    def pure[F[_]: Functor: Applicative, A](a: A): F[A] =
      Applicative[F].pure(a)

    implicit class ApplicativeFunctionOps[F[_]: Applicative, A, B](self: F[A => B]) {
      def <*>(fa: F[A]): F[B] =
        Applicative[F].ap(self)(fa)
    }

    implicit class ApplicativeOps[F[_]: Applicative, A](self: F[A]) {
      private val Ap = Applicative[F]

      import Function.const

      def *>[B](fb: F[B]): F[B] =
        Ap.F.map(self) { (a: A) => (b: B) => const(b)(a) } <*> fb

      def <*[B](fb: F[B]): F[A] =
        Ap.F.map(self) { (a: A) => (b: B) => const(a)(b) } <*> fb
    }

    def replicateA[F[_]: Applicative, A](times: Int, action: F[A]): F[Seq[A]] = {
      if (times == 0) {
        Applicative[F].pure(Seq())
      } else {
        Applicative[F].F.map(action)(cons) <*> replicateA(times - 1, action)
      }
    }
  }
}
