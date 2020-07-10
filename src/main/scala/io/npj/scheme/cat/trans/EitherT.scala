package io.npj.scheme.cat.trans

case class EitherT[M[_], E, A](private val runEitherT: M[Either[E, A]])

object EitherT {
  import io.npj.scheme.cat.{Functor, Applicative, Alternative, Monad, MonadError, Monoid}
  import Functor.syntax._
  import Monad.syntax._

  def runEitherT[M[_]: Monad, E, A](either: EitherT[M, E, A]): M[Either[E, A]] =
    either.runEitherT

  implicit def EitherFunctor[M[_]: Monad, E] = new Functor[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val M = Monad[M]
    override def map[A, B](fa: EitherT[M, E, A])(f: A => B): EitherT[M, E, B] =
      EitherT(M.Ap.F.map(runEitherT(fa))(_.map(f)))
  }

  implicit def EitherApplicative[M[_]: Monad, E] = new Applicative[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val M = Monad[M]

    val F: Functor[({ type lam[A] = EitherT[M, E, A] })#lam] = EitherFunctor

    def pure[A](a: A): EitherT[M, E, A] =
      EitherT(M.Ap.pure(Right(a)))

    def ap[A, B](fab: EitherT[M, E, A => B])(fa: EitherT[M, E, A]): EitherT[M, E, B] = {
      EitherT(
        runEitherT(fab) >>= {
          case Left(err) => M.Ap.pure(Left(err))
          case Right(ab) => runEitherT(fa) >>= {
            case Left(err) => M.Ap.pure(Left(err))
            case Right(a) => M.Ap.pure(Right(ab(a)))
          }
        }
      )
    }
  }

  implicit def EitherMonad[M[_]: Monad, E] = new Monad[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val M = Monad[M]

    val Ap: Applicative[({ type lam[A] = EitherT[M, E, A] })#lam] = EitherApplicative

    override def flatMap[A, B](ma: EitherT[M, E, A])(f: A => EitherT[M, E, B]): EitherT[M, E, B] =
      EitherT(
        runEitherT(ma) >>= {
          case Left(err) => M.Ap.pure(Left(err))
          case Right(ok) => runEitherT(f(ok))
        }
      )
  }

  implicit def EitherError[M[_]: Monad] = new MonadError[({ type lam[A] = EitherT[M, String, A] })#lam] {
    private val _M = Monad[M]

    val M: Monad[({ type lam[A] = EitherT[M, String, A] })#lam] = EitherMonad

    def throwError[A](message: String): EitherT[M, String, A] =
      EitherT(_M.Ap.pure(Left(message)))

    def catchError[A](ma: EitherT[M, String, A])(f: String => EitherT[M, String, A]): EitherT[M, String, A] =
      EitherT(
        runEitherT(ma) >>= {
          case Left(err) => runEitherT(f(err))
          case Right(ok) => _M.Ap.pure(Right(ok))
        }
      )
  }

  implicit def EitherAlternative[M[_]: Monad, E: Monoid] = new Alternative[({ type lam[A] = EitherT[M, E, A] })#lam] {
    private val M = Monad[M]

    val Ap: Applicative[({ type lam[A] = EitherT[M, E, A] })#lam] = EitherApplicative

    def empty[A]: EitherT[M, E, A] =
      EitherT(M.Ap.pure(Left(Monoid[E].empty)))

    def orElse[A](fa1: EitherT[M, E, A])(fa2: EitherT[M, E, A]): EitherT[M, E, A] =
      EitherT(
        runEitherT(fa1) >>= {
          case Left(_) => runEitherT(fa2)
          case Right(ok) => M.Ap.pure(Right(ok))
        }
      )
  }
}
