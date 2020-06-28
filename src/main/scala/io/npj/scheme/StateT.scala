package io.npj.scheme

import io.npj.scheme.cat.{Applicative, Functor, Monad}

case class StateT[M[_], S, A](private val runStateT: S => M[(A, S)])

object StateT {
  import Function.const
  import cat.implicits._

  def runStateT[M[_]: Monad, S, A](st: StateT[M, S, A], init: S): M[(A, S)] = st.runStateT(init)
  def evalStateT[M[_]: Monad, S, A](st: StateT[M, S, A], init: S): M[A] = runStateT(st, init).map(_._1)
  def execStateT[M[_]: Monad, S, A](st: StateT[M, S, A], init: S): M[S] = runStateT(st, init).map(_._2)

  implicit def StateFunctor[M[_]: Monad, S] = new Functor[({ type lam[A] = StateT[M, S, A] })#lam] {
    def map[A, B](fa: StateT[M, S, A])(f: A => B): StateT[M, S, B] =
      StateT { s =>
        runStateT(fa, s).map { case (a, ns) => (f(a), ns) }
      }
  }

  implicit def StateApplicative[M[_]: Monad, S] = new Applicative[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val M: Monad[M] = implicitly[Monad[M]]

    def map[A, B](fa: StateT[M, S, A])(f: A => B): StateT[M, S, B] =
      StateFunctor[M, S].map(fa)(f)

    def pure[A](a: A): StateT[M, S, A] =
      StateT { s => M.pure((a, s)) }

    def ap[A, B](fab: StateT[M, S, A => B])(fa: StateT[M, S, A]): StateT[M, S, B] =
      StateT { s =>
        runStateT(fab, s) >>= { case (ab, ns) =>
          runStateT(fa, ns) >>= { case (a, nns) =>
            M.pure(ab(a), nns)
          }
        }
      }
  }

  implicit def StateMonad[M[_]: Monad, S] = new Monad[({ type lam[A] = StateT[M, S, A] })#lam] {
    private val M: Monad[M] = implicitly[Monad[M]]

    def map[A, B](fa: StateT[M, S, A])(f: A => B): StateT[M, S, B] =
      StateApplicative[M, S].map(fa)(f)

    def pure[A](a: A): StateT[M, S, A] =
      StateApplicative[M, S].pure(a)

    def ap[A, B](fab: StateT[M, S, A => B])(fa: StateT[M, S, A]): StateT[M, S, B] =
      StateApplicative[M, S].ap(fab)(fa)

    def flatMap[A, B](sma: StateT[M, S, A])(f: A => StateT[M, S, B]): StateT[M, S, B] =
      StateT { s =>
        M.flatMap(runStateT(sma, s)) { case (a, ns) =>
            runStateT(f(a), ns)
        }
      }
  }

  def get[M[_]: Monad, S]: StateT[M, S, S] = StateT { s => implicitly[Monad[M]].pure(s, s) }
  def modify[M[_]: Monad, S](f: S => S): StateT[M, S, ()] = StateT { s => implicitly[Monad[M]].pure((), f(s)) }
  def put[M[_]: Monad, S](newState: S): StateT[M, S, ()] = modify(const(newState))
}


