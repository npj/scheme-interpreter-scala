package io.npj.scheme

import io.npj.scheme.cat.{Applicative, Functor, Monad}

case class EitherT[M[_], E, A](private val runEitherT: M[Either[E, A]])

object EitherT {
  import cat.implicits._

  def runEitherT[M[_]: Monad, E, A](either: EitherT[M, E, A]): M[Either[E, A]] = either.runEitherT

  implicit def EitherFunctor[M[_]: Monad, E] = new Functor[({ type lam[A] = EitherT[M, E, A] })#lam] {
    override def map[A, B](fa: EitherT[M, E, A])(f: A => B): EitherT[M, E, B] =
      EitherT(runEitherT(fa).map(_.map(f)))
  }

  implicit def EitherApplicative[M[_]: Monad, E] = new Applicative[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val M: Monad[M] = implicitly[Monad[M]]

    override def map[A, B](fa: EitherT[M, E, A])(f: A => B): EitherT[M, E, B] =
      EitherFunctor[M, E].map(fa)(f)

    override def pure[A](a: A): EitherT[M, E, A] =
      EitherT(M.pure(Right(a)))

    override def ap[A, B](fab: EitherT[M, E, A => B])(fa: EitherT[M, E, A]): EitherT[M, E, B] = {
      EitherT(
        runEitherT(fab) >>= {
          case Left(err) => M.pure(Left(err))
          case Right(ab) => runEitherT(fa) >>= {
            case Left(err) => M.pure(Left(err))
            case Right(a) => M.pure(Right(ab(a)))
          }
        }
      )
    }
  }

  implicit def EitherMonad[M[_]: Monad, E] = new Monad[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val M: Monad[M] = implicitly[Monad[M]]

    override def map[A, B](fa: EitherT[M, E, A])(f: A => B): EitherT[M, E, B] =
      EitherFunctor[M, E].map(fa)(f)

    override def pure[A](a: A): EitherT[M, E, A] =
      EitherApplicative[M, E].pure(a)

    override def ap[A, B](fab: EitherT[M, E, A => B])(fa: EitherT[M, E, A]): EitherT[M, E, B] =
      EitherApplicative[M, E].ap(fab)(fa)

    override def flatMap[A, B](ma: EitherT[M, E, A])(f: A => EitherT[M, E, B]): EitherT[M, E, B] =
      EitherT(
        M.flatMap(runEitherT(ma)) {
          case Left(err) => M.pure(Left(err))
          case Right(ok) => runEitherT(f(ok))
        }
      )
  }
}
