package io.npj.scheme

import io.npj.scheme.Parser.ParserStateM
import io.npj.scheme.cat.trans.{EitherT, StateT}
import io.npj.scheme.cat.{Alternative, Identity}

case class ParseState(charNum: Int, lineNum: Int, pos: Int, input: String)
case class Parser[A](private val parserState: ParserStateM[A])

object ParseState {
  def init(input: String): ParseState = ParseState(
    charNum = 1,
    lineNum = 1,
    pos = 0,
    input = input
  )
}

object Parser {
  import Alternative.syntax._
  import io.npj.scheme.cat.{Applicative, Functor, Monad, MonadFail}
  import Applicative.syntax._
  import EitherT._
  import Functor.syntax._
  import Monad.syntax._
  import StateT._

  type ParserStateM[A] = StateT[({ type lam[T] = EitherT[Identity, String, T] })#lam, ParseState, A]

  implicit object ParserFunctor extends Functor[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      Parser(fa.parserState.map(f))
  }

  implicit object ParserApplicative extends Applicative[Parser] {
    def pure[A](a: A): Parser[A] =
      Parser(Applicative[ParserStateM].pure(a))

    def ap[A, B](fab: Parser[A => B])(fa: Parser[A]): Parser[B] =
      Parser(fab.parserState <*> fa.parserState)
  }

  implicit object ParserMonad extends Monad[Parser] {
    def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] =
      Parser(ma.parserState >>= { a => f(a).parserState })
  }

  implicit object ParserFail extends MonadFail[Parser] {
    def fail[A](message: String): Parser[A] =
      Parser(MonadFail[ParserStateM].fail(message))
  }

  implicit object ParserAlternative extends Alternative[Parser] {
    override def empty[A]: Parser[A] =
      Parser(Alternative[ParserStateM].empty)

    override def orElse[A](fa1: Parser[A])(fa2: Parser[A]): Parser[A] =
      Parser(fa1.parserState <|> fa2.parserState)
  }

  def runParser[A](parser: Parser[A], input: String): Either[String, A] = {
    Identity.runIdentity(
      EitherT.runEitherT(
        StateT.evalStateT(parser.parserState, ParseState.init(input))
      )
    )
  }

  private val A = Applicative[Parser]
  private val M = Monad[Parser]
  private val MF = MonadFail[Parser]

  private def get: Parser[ParseState] = Parser(StateT.get)

  private def modify(f: ParseState => ParseState): Parser[()] = Parser(StateT.modify(f))

  private def fail[A](message: String): Parser[A] = get >>= { state =>
    MF.fail(s"$message: line = ${state.lineNum}, char = ${state.charNum}")
  }

  def char(c: Char): Parser[Char] = peek >>= {
    case Some(`c`) => A.pure(c)
    case _ => fail(s"expected '$c'")
  }

  def peek: Parser[Option[Char]] = get.map(curr)

  def advance(i: Int): Parser[()] =
    replicateA(i, advance1) *> A.pure(())

  def advance1: Parser[()] = get >>= { state =>
    if (state.pos < state.input.length - 1) {
      Parser(put(step(state)))
    } else {
      fail(s"end of input")
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
    if (state.input.charAt(state.pos) == '\n') {
      state.copy(pos = newPos, charNum = 1, lineNum = state.lineNum + 1)
    } else {
      state.copy(pos = newPos, charNum = state.charNum + 1)
    }
  }
}
