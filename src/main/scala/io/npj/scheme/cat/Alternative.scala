package io.npj.scheme.cat

trait Alternative[F[_]] {
  import Cons._

  val Ap: Applicative[F]

  def empty[A]: F[A]

  def orElse[A](fa1: => F[A])(fa2: => F[A]): F[A]

  def many[A](fa: F[A]): F[Seq[A]] =
    orElse(some(fa))(Ap.pure(Seq()))

  def some[A](fa: F[A]): F[Seq[A]] =
    Ap.ap(Ap.F.map(fa)(+:))(many(fa))
}

object Alternative {
  def apply[F[_]](implicit f: Alternative[F]): Alternative[F] = f

  object syntax {
    def many[F[_]: Alternative, A](fa: F[A]): F[Seq[A]] =
      Alternative[F].many(fa)

    def some[F[_]: Alternative, A](fa: F[A]): F[Seq[A]] =
      Alternative[F].some(fa)

    implicit class AlternativeOps[F[_]: Alternative, A](self: => F[A]) {
      def <|>(fa: => F[A]): F[A] =
        Alternative[F].orElse(self)(fa)
    }
  }
}
