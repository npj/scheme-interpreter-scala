package io.npj.scheme

import io.npj.scheme.Parser.ParserState
import io.npj.scheme.cat.{Alternative, Identity, MonadFail}
import io.npj.scheme.cat.trans.{EitherT, StateT}

object types {
}

case class ParseState(charNum: Int, lineNum: Int, pos: Int, input: String)
case class Parser[A](private val parserState: ParserState[A])

object Parser {
  import io.npj.scheme.cat.{Functor, Applicative, Monad, MonadFail}
  import Functor.syntax._
  import Applicative.syntax._
  import Alternative.syntax._
  import Monad.syntax._
  import StateT._
  import EitherT._

  type ParserState[A] = StateT[({ type lam[T] = EitherT[Identity, String, T] })#lam, ParseState, A]

  implicit object ParserFunctor extends Functor[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      Parser(fa.parserState.map(f))
  }

  implicit object ParserApplicative extends Applicative[Parser] {
    def pure[A](a: A): Parser[A] =
      Parser(Applicative[ParserState].pure(a))

    def ap[A, B](fab: Parser[A => B])(fa: Parser[A]): Parser[B] =
      Parser(fab.parserState <*> fa.parserState)
  }

  implicit object ParserMonad extends Monad[Parser] {
    def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] =
      Parser(ma.parserState >>= { a => f(a).parserState })
  }

  implicit object ParserFail extends MonadFail[Parser] {
    def fail[A](message: String): Parser[A] =
      Parser(MonadFail[ParserState].fail(message))
  }

  implicit object ParserAlternative extends Alternative[Parser] {
    override def empty[A]: Parser[A] =
      Parser(Alternative[ParserState].empty)

    override def orElse[A](fa1: Parser[A])(fa2: Parser[A]): Parser[A] =
      Parser(fa1.parserState <|> fa2.parserState)
  }

  def runParser[A](parser: Parser[A], input: String): Either[String, A] = {
    Identity.runIdentity(
      EitherT.runEitherT(
        StateT.evalStateT(parser.parserState, ParseState(
          charNum = 0,
          lineNum = 0,
          pos = 0,
          input = input
        ))
      )
    )
  }

  private val A = Applicative[Parser]
  private val M = Monad[Parser]
  private val MF = MonadFail[Parser]

  private def get: Parser[ParseState] = Parser(StateT.get)
  private def modify(f: ParseState => ParseState): Parser[()] = Parser(StateT.modify(f))

  def peek: Parser[Option[Char]] = get.map(curr)

  def advance(i: Int): Parser[()] =
    replicateA(i, advance1) *> A.pure(())

  def advance1: Parser[()] = get >>= { state =>
    if (state.pos < state.input.length - 1) {
      Parser(put(step(state)))
    } else {
      MF.fail("end of input")
    }
  }

  private def curr(state: ParseState): Option[Char] =
    if (state.pos >= state.input.length) {
      None
    } else {
      Some(state.input.charAt(state.pos))
    }

  private def step(state: ParseState): ParseState = {
    val newPos = state.pos + 1
    if (state.input.charAt(newPos) == '\n') {
      state.copy(pos = newPos, charNum = 0, lineNum = state.lineNum + 1)
    } else {
      state.copy(pos = newPos, charNum = state.charNum + 1)
    }
  }
}
