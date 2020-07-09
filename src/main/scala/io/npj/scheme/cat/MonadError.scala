package io.npj.scheme.cat

trait MonadError[M[_]] {
  def throwError[A](message: String): M[A]
  def catchError[A](ma: M[A])(f: String => M[A]): M[A]
}

object MonadError {
  def apply[M[_]: Functor: Applicative: Monad](implicit m: MonadError[M]): MonadError[M] = m
}
