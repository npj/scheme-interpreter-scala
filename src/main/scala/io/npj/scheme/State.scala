package io.npj.scheme

import io.npj.scheme.cat.{Applicative, Functor, Monad}

case class State[S, A](runState: S => (A, S))

object State {
  import Function.const

  def runState[S, A](st: State[S, A], init: S): (A, S) = st.runState(init)
  def evalState[S, A](st: State[S, A], init: S): A = runState(st, init)._1
  def execState[S, A](st: State[S, A], init: S): S = runState(st, init)._2

  implicit def StateFunctor[S] = new Functor[({ type lam[A] = State[S, A] })#lam] {
    def map[A, B](fa: State[S, A])(f: A => B): State[S, B] =
      State { s =>
        runState(fa, s) match {
          case (a, ns) => (f(a), ns)
        }
      }
  }

  implicit def StateApplicative[S] = new Applicative[({ type lam[A] = State[S, A] })#lam] {
    def map[A, B](fa: State[S, A])(f: A => B): State[S, B] =
      StateFunctor.map(fa)(f)

    def pure[A](a: A): State[S, A] =
      State { s => (a, s) }

    def ap[A, B](fab: State[S, A => B])(fa: State[S, A]): State[S, B] =
      State { s =>
        runState(fab, s) match {
          case (g, ns) => runState(fa, ns) match {
            case (a, nns) => (g(a), nns)
          }
        }
      }
  }

  implicit def StateMonad[S] = new Monad[({ type lam[A] = State[S, A] })#lam] {
    def map[A, B](fa: State[S, A])(f: A => B): State[S, B] =
      StateApplicative.map(fa)(f)

    def pure[A](a: A): State[S, A] =
      StateApplicative.pure(a)

    def ap[A, B](fab: State[S, A => B])(fa: State[S, A]): State[S, B] =
      StateApplicative.ap(fab)(fa)

    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      State { s =>
        runState(ma, s) match {
          case (a, ns) => runState(f(a), ns)
        }
      }
  }

  def get[S]: State[S, S] = State { s => (s, s) }
  def modify[S](f: S => S): State[S, ()] = State { s => ((), f(s)) }
  def put[S](newState: S): State[S, ()] = modify(const(newState))
}


