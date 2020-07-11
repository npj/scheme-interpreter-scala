package io.npj.scheme.cat

case class Identity[A](private val a: A)

object Identity {
  implicit object IdentityFunctor extends Functor[Identity] {
    def map[A, B](fa: => Identity[A])(f: A => B): Identity[B] =
      Identity(f(fa.a))
  }

  implicit object IdentityApplicative extends Applicative[Identity] {
    val F: Functor[Identity] = IdentityFunctor

    def pure[A](a: => A): Identity[A] =
      Identity(a)

    def ap[A, B](fab: => Identity[A => B])(fa: => Identity[A]): Identity[B] =
      Identity(fab.a(fa.a))
  }

  implicit object IdentityMonad extends Monad[Identity] {
    val Ap: Applicative[Identity] = IdentityApplicative

    def flatMap[A, B](ma: => Identity[A])(f: A => Identity[B]): Identity[B] =
      f(ma.a)
  }

  def runIdentity[A](i: Identity[A]): A = i.a
}
