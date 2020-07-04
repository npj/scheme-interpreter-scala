package io.npj.scheme.cat

trait MonadFail[M[_]] {
  def fail[A](message: String): M[A]
}

object MonadFail {
  def apply[M[_]: Monad](implicit m: MonadFail[M]): MonadFail[M] = m
}
