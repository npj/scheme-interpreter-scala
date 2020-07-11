package io.npj.scheme.cat.trans

import io.npj.scheme.cat.{Alternative, MonadError}

case class StateT[M[_], S, A](private val runStateT: S => M[(A, S)])

object StateT {
  import Function.const
  import io.npj.scheme.cat.{Functor, Applicative, Monad}
  import Functor.syntax._
  import Monad.syntax._
  import Alternative.syntax._

  def runStateT[M[_]: Monad, S, A](st: StateT[M, S, A], init: S): M[(A, S)] =
    st.runStateT(init)

  def evalStateT[M[_]: Monad, S, A](st: StateT[M, S, A], init: S): M[A] =
    Monad[M].Ap.F.map(runStateT(st, init))(_._1)

  def execStateT[M[_]: Monad, S, A](st: StateT[M, S, A], init: S): M[S] =
    Monad[M].Ap.F.map(runStateT(st, init))(_._2)

  implicit def StateFunctor[M[_]: Monad, S] = new Functor[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val M = Monad[M]

    def map[A, B](fa: StateT[M, S, A])(f: A => B): StateT[M, S, B] =
      StateT { s =>
        M.Ap.F.map(runStateT(fa, s)) { case (a, ns) => (f(a), ns) }
      }
  }

  implicit def StateApplicative[M[_]: Monad, S] = new Applicative[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val M = Monad[M]

    val F: Functor[({ type lam[A] = StateT[M, S, A] })#lam] = StateFunctor

    def pure[A](a: A): StateT[M, S, A] =
      StateT { s => M.Ap.pure((a, s)) }

    def ap[A, B](fab: StateT[M, S, A => B])(fa: StateT[M, S, A]): StateT[M, S, B] =
      StateT { s =>
        runStateT(fab, s) >>= { case (ab, ns) =>
          runStateT(fa, ns) >>= { case (a, nns) =>
            M.Ap.pure(ab(a), nns)
          }
        }
      }
  }

  implicit def StateMonad[M[_]: Monad, S] = new Monad[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val M = Monad[M]

    val Ap: Applicative[({ type lam[A] = StateT[M, S, A] })#lam] = StateApplicative

    def flatMap[A, B](sma: StateT[M, S, A])(f: A => StateT[M, S, B]): StateT[M, S, B] =
      StateT { s =>
        M.flatMap(runStateT(sma, s)) { case (a, ns) =>
            runStateT(f(a), ns)
        }
      }

  }

  implicit def StateError[M[_]: Monad: MonadError, S] = new MonadError[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val ME = MonadError[M]

    val M: Monad[({ type lam[A] = StateT[M, S, A] })#lam] = StateMonad

    def throwError[A](message: String): StateT[M, S, A] =
      StateT(const(ME.throwError(message)))

    def catchError[A](ma: StateT[M, S, A])(f: String => StateT[M, S, A]): StateT[M, S, A] = StateT { s =>
      ME.catchError(runStateT(ma, s)) { msg => runStateT(f(msg), s) }
    }
  }

  implicit def StateAlternative[M[_]: Monad: Alternative, S] = new Alternative[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val Alt = Alternative[M]
    val Ap: Applicative[({ type lam[A] = StateT[M, S, A] })#lam] = StateApplicative

    def empty[A]: StateT[M, S, A] = StateT(const(Alternative[M].empty))

    def orElse[A](fa1: => StateT[M, S, A])(fa2: => StateT[M, S, A]): StateT[M, S, A] =
      StateT { s =>
        Alt.orElse(runStateT(fa1, s))(runStateT(fa2, s))
      }
  }

  def get[M[_]: Functor: Applicative: Monad, S]: StateT[M, S, S] =
    StateT { s => Applicative[M].pure(s, s) }

  def modify[M[_]: Functor: Applicative: Monad, S](f: S => S): StateT[M, S, ()] =
    StateT { s => Applicative[M].pure((), f(s)) }

  def put[M[_]: Functor: Applicative: Monad, S](newState: S): StateT[M, S, ()] =
    modify(const(newState))
}


