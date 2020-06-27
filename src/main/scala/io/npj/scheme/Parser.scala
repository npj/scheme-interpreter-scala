package io.npj.scheme

import io.npj.scheme.cat.{Applicative, Functor, Monad}
import io.npj.scheme.State._

case class ParseState(charNum: Int, lineNum: Int, pos: Int, input: String)
case class Parser[A](private val parserState: State[ParseState, Either[String, A]])

object Parser {
  private val SM = StateMonad[ParseState]
  import SM._

  implicit object ParserFunctor extends Functor[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      Parser(SM.map(fa.parserState)(_.map(f)))
  }

  implicit object ParserApplicative extends Applicative[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      ParserFunctor.map(fa)(f)

    def pure[A](a: A): Parser[A] =
      Parser(SM.pure(Right(a)))

    def ap[A, B](fab: Parser[A => B])(fa: Parser[A]): Parser[B] = Parser(
      State { s =>
        runState(fab.parserState, s) match {
          case (Left(err), ns) => (Left(err), ns)
          case (Right(f), ns) => runState(fa.parserState, ns) match {
            case (Left(err), nns) => (Left(err), nns)
            case (Right(a), nns) => (Right(f(a)), nns)
          }
        }
      }
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
      SM.flatMap(ma.parserState) {
        case Left(err) => SM.pure(Left(err))
        case Right(ok) => f(ok).parserState
      })
  }

  def runParser[A](parser: Parser[A], input: String): Either[String, A] =
    evalState(parser.parserState, ParseState(
      charNum = 0,
      lineNum = 0,
      pos = 0,
      input = input
    ))

  def peek: Parser[Option[Char]] =
    Parser(flatMap(get) { s => pure(
      Right(
        if (s.pos >= s.input.length) {
          None
        } else {
          Some(s.input.charAt(s.pos))
        }
    ))})

  def advance(i: Int): Parser[()] = {
    val modified = modify[ParseState] { s =>
      s.copy(pos = s.pos + i, charNum = s.charNum + i)
    }
    Parser(map(modified)(Right(_)))
  }
}
