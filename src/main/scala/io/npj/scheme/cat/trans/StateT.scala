package io.npj.scheme.cat.trans

import io.npj.scheme.cat.MonadFail

case class StateT[M[_], S, A](private val runStateT: S => M[(A, S)])

object StateT {
  import Function.const
  import io.npj.scheme.cat.{Functor, Applicative, Monad}
  import Functor.syntax._
  import Monad.syntax._

  def runStateT[M[_]: Functor: Applicative: Monad, S, A](st: StateT[M, S, A], init: S): M[(A, S)] =
    st.runStateT(init)

  def evalStateT[M[_]: Functor: Applicative: Monad, S, A](st: StateT[M, S, A], init: S): M[A] =
    runStateT(st, init).map(_._1)

  def execStateT[M[_]: Functor: Applicative: Monad, S, A](st: StateT[M, S, A], init: S): M[S] =
    runStateT(st, init).map(_._2)

  implicit def StateFunctor[M[_]: Functor: Applicative: Monad, S] = new Functor[({ type lam[A] = StateT[M, S, A] })#lam] {
    def map[A, B](fa: StateT[M, S, A])(f: A => B): StateT[M, S, B] =
      StateT { s =>
        runStateT(fa, s).map { case (a, ns) => (f(a), ns) }
      }
  }

  implicit def StateApplicative[M[_]: Functor: Applicative: Monad, S] = new Applicative[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val A = Applicative[M]

    def pure[A](a: A): StateT[M, S, A] =
      StateT { s => A.pure((a, s)) }

    def ap[A, B](fab: StateT[M, S, A => B])(fa: StateT[M, S, A]): StateT[M, S, B] =
      StateT { s =>
        runStateT(fab, s) >>= { case (ab, ns) =>
          runStateT(fa, ns) >>= { case (a, nns) =>
            A.pure(ab(a), nns)
          }
        }
      }
  }

  implicit def StateMonad[M[_]: Functor: Applicative: Monad, S] = new Monad[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val M = Monad[M]

    def flatMap[A, B](sma: StateT[M, S, A])(f: A => StateT[M, S, B]): StateT[M, S, B] =
      StateT { s =>
        M.flatMap(runStateT(sma, s)) { case (a, ns) =>
            runStateT(f(a), ns)
        }
      }
  }

  implicit def StateFail[M[_]: Functor: Applicative: Monad: MonadFail, S] = new MonadFail[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val M = MonadFail[M]
    def fail[A](message: String): StateT[M, S, A] = StateT(const(M.fail(message)))
  }

  def get[M[_]: Functor: Applicative: Monad, S]: StateT[M, S, S] =
    StateT { s => Applicative[M].pure(s, s) }

  def modify[M[_]: Functor: Applicative: Monad, S](f: S => S): StateT[M, S, ()] =
    StateT { s => Applicative[M].pure((), f(s)) }

  def put[M[_]: Functor: Applicative: Monad, S](newState: S): StateT[M, S, ()] =
    modify(const(newState))
}


