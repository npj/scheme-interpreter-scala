package io.npj.scheme.cat

trait Monad[M[_]] {
  val Ap: Applicative[M]
  def flatMap[A, B](ma: => M[A])(f: A => M[B]): M[B]
}

object Monad {
  def apply[M[_]](implicit m: Monad[M]): Monad[M] = m

  object syntax {
    implicit class MonadOps[M[_]: Monad, A](self: => M[A]) {
      import Function.const

      def >>=[B](f: => A => M[B]): M[B] =
        Monad[M].flatMap(self)(f)

      def >>[B](mb: => M[B]): M[B] =
        Monad[M].flatMap(self)(const(mb))
    }
  }
}
