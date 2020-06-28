package io.npj.scheme

import io.npj.scheme.cat.{Applicative, Functor, Monad}

case class Identity[A](private val a: A)

object Identity {
  implicit object IdentityFunctor extends Functor[Identity] {
    def map[A, B](fa: Identity[A])(f: A => B): Identity[B] =
      Identity(f(fa.a))
  }

  implicit object IdentityApplicative extends Applicative[Identity] {
    def map[A, B](fa: Identity[A])(f: A => B): Identity[B] =
      IdentityFunctor.map(fa)(f)

    def pure[A](a: A): Identity[A] =
      Identity(a)

    def ap[A, B](fab: Identity[A => B])(fa: Identity[A]): Identity[B] =
      Identity(fab.a(fa.a))
  }

  implicit object IdentityMonad extends Monad[Identity] {
    def map[A, B](fa: Identity[A])(f: A => B): Identity[B] =
      IdentityApplicative.map(fa)(f)

    def pure[A](a: A): Identity[A] =
      IdentityApplicative.pure(a)

    def ap[A, B](fab: Identity[A => B])(fa: Identity[A]): Identity[B] =
      IdentityApplicative.ap(fab)(fa)

    def flatMap[A, B](ma: Identity[A])(f: A => Identity[B]): Identity[B] =
      f(ma.a)
  }

  def runIdentity[A](i: Identity[A]): A = i.a
}
