package io.npj.scheme.cat

trait Alternative[F[_]] {
  val Ap: Applicative[F]

  def empty[A]: F[A]
  def orElse[A](fa1: F[A])(fa2: F[A]): F[A]

  def many[A](fa: F[A]): F[Seq[A]] =
    orElse(defer(some(fa)))(Ap.pure(Seq()))

  def some[A](fa: F[A]): F[Seq[A]] = {
    def prepend(a: A)(as: Seq[A]): Seq[A] = a +: as
    Ap.ap(Ap.F.map(fa)(prepend))(defer(many(fa)))
  }

  private def defer[A](a: => A): A = a
}

object Alternative {
  def apply[F[_]](implicit f: Alternative[F]): Alternative[F] = f

  object syntax {
    implicit class AlternativeOps[F[_]: Alternative, A](self: F[A]) {
      private val Alt = Alternative[F]

      def <|>(fa: F[A]): F[A] =
        Alt.orElse(self)(fa)
    }
  }
}
