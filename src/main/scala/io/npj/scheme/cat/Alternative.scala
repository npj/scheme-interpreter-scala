package io.npj.scheme.cat

trait Alternative[F[_]] {
  val Ap: Applicative[F]

  def empty[A]: F[A]
  def orElse[A](fa1: F[A])(fa2: F[A]): F[A]

  def some[A](fa: F[A]): F[Seq[A]] = {
    def prepend(a: A)(as: Seq[A]): Seq[A] = as.prepended(a)
    Ap.ap(Ap.F.map(fa)(prepend))(many(fa))
  }

  def many[A](fa: F[A]): F[Seq[A]] =
    orElse(some(fa))(Ap.pure(Seq[A]()))
}

object Alternative {
  def apply[F[_]](implicit f: Alternative[F]): Alternative[F] = f

  object syntax {
    def many[F[_]: Alternative, A](fa: F[A]): F[Seq[A]] =
      Alternative[F].many(fa)

    def some[F[_]: Alternative, A](fa: F[A]): F[Seq[A]] =
      Alternative[F].some(fa)

    implicit class AlternativeOps[F[_]: Alternative, A](self: F[A]) {
      private val Alt = Alternative[F]

      def <|>(fa: F[A]): F[A] =
        Alt.orElse(self)(fa)

      def some: F[Seq[A]] =
        Alt.some(self)

    }
  }
}
