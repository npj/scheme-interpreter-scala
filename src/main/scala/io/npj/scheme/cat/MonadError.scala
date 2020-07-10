package io.npj.scheme.cat

trait MonadError[M[_]] {
  val M: Monad[M]
  def throwError[A](message: String): M[A]
  def catchError[A](ma: M[A])(f: String => M[A]): M[A]
}

object MonadError {
  def apply[M[_]](implicit m: MonadError[M]): MonadError[M] = m
}
