package io.npj.scheme

import io.npj.scheme.cat.{Applicative, Functor, Monad}

case class ParseState(charNum: Int, lineNum: Int, pos: Int, input: String)
case class Parser[A](private val parserState: StateT[({ type lam[T] = EitherT[Identity, String, T] })#lam, ParseState, A])

object Parser {
  import StateT._
  import EitherT._
  import Identity._
  import cat.implicits._

  private def S = StateMonad[({ type lam[T] = EitherT[Identity, String, T] })#lam, ParseState]

  implicit object ParserFunctor extends Functor[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser(
      S.map(fa.parserState)(f)
    )
  }

  implicit object ParserApplicative extends Applicative[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      ParserFunctor.map(fa)(f)

    def pure[A](a: A): Parser[A] =
      Parser(S.pure(a))

    def ap[A, B](fab: Parser[A => B])(fa: Parser[A]): Parser[B] = Parser(
      S.ap(fab.parserState)(fa.parserState)
    )
  }

  implicit object ParserMonad extends Monad[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      ParserApplicative.map(fa)(f)

    def pure[A](a: A): Parser[A] =
      ParserApplicative.pure(a)

    def ap[A, B](fab: Parser[A => B])(fa: Parser[A]): Parser[B] =
      ParserApplicative.ap(fab)(fa)

    def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] = Parser(
      S.flatMap(ma.parserState)(f(_).parserState)
    )
  }

  def runParser[A](parser: Parser[A], input: String): Either[String, A] =
    runIdentity(
      runEitherT(
        evalStateT(parser.parserState, ParseState(
          charNum = 0,
          lineNum = 0,
          pos = 0,
          input = input
        ))
      )
    )

  def peek: Parser[Option[Char]] = Parser(
    S.flatMap(get) { s =>
      S.pure(
        if (s.pos >= s.input.length) {
          None
        } else {
          Some(s.input.charAt(s.pos))
        }
      )
    }
  )
  def advance(i: Int): Parser[()] = Parser(
    S.flatMap(modify { s => s.copy(pos = s.pos + i) })(S.pure)
  )
}
