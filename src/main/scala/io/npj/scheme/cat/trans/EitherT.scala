package io.npj.scheme.cat.trans

import io.npj.scheme.cat.{Alternative, MonadFail, Monoid}

case class EitherT[M[_], E, A](private val runEitherT: M[Either[E, A]])

object EitherT {
  import io.npj.scheme.cat.{Functor, Applicative, Monad}
  import Functor.syntax._
  import Monad.syntax._

  def runEitherT[M[_]: Functor: Applicative: Monad, E, A](either: EitherT[M, E, A]): M[Either[E, A]] =
    either.runEitherT

  implicit def EitherFunctor[M[_]: Functor: Applicative: Monad, E] = new Functor[({ type lam[A] = EitherT[M, E, A] })#lam] {
    override def map[A, B](fa: EitherT[M, E, A])(f: A => B): EitherT[M, E, B] =
      EitherT(runEitherT(fa).map(_.map(f)))
  }

  implicit def EitherApplicative[M[_]: Functor: Applicative: Monad, E] = new Applicative[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val A = Applicative[M]
    override def pure[A](a: A): EitherT[M, E, A] =
      EitherT(A.pure(Right(a)))

    override def ap[A, B](fab: EitherT[M, E, A => B])(fa: EitherT[M, E, A]): EitherT[M, E, B] = {
      EitherT(
        runEitherT(fab) >>= {
          case Left(err) => A.pure(Left(err))
          case Right(ab) => runEitherT(fa) >>= {
            case Left(err) => A.pure(Left(err))
            case Right(a) => A.pure(Right(ab(a)))
          }
        }
      )
    }
  }

  implicit def EitherMonad[M[_]: Functor: Applicative: Monad, E] = new Monad[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val A = Applicative[M]
    private val M = Monad[M]

    override def flatMap[A, B](ma: EitherT[M, E, A])(f: A => EitherT[M, E, B]): EitherT[M, E, B] =
      EitherT(
        runEitherT(ma) >>= {
          case Left(err) => A.pure(Left(err))
          case Right(ok) => runEitherT(f(ok))
        }
      )
  }

  implicit def EitherFail[M[_]: Functor: Applicative: Monad] = new MonadFail[({ type lam[A] = EitherT[M, String, A] })#lam] {
    private val A = Applicative[M]

     def fail[A](message: String): EitherT[M, String, A] =
       EitherT(A.pure(Left(message)))
  }

  implicit def EitherAlternative[M[_]: Functor: Applicative: Monad, E: Monoid] = new Alternative[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val A = Applicative[M]

    def empty[A]: EitherT[M, E, A] =
      EitherT(A.pure(Left(Monoid[E].empty)))

    def orElse[A](fa1: EitherT[M, E, A])(fa2: EitherT[M, E, A]): EitherT[M, E, A] =
      EitherT(
        runEitherT(fa1) >>= {
          case Left(_) => runEitherT(fa2)
          case Right(ok) => A.pure(Right(ok))
        }
      )
  }
}
