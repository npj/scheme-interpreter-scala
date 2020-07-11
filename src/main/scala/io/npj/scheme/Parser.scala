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
  import io.npj.scheme.cat.{Applicative, Functor, Monad, MonadError}
  import Functor.syntax._
  import Applicative._
  import Applicative.syntax._
  import Alternative.syntax._
  import Monad.syntax._

  type ParserStateM[A] = StateT[({ type lam[T] = EitherT[Identity, String, T] })#lam, ParseState, A]

  private val M = Monad[ParserStateM]
  private val Alt = Alternative[ParserStateM]

  implicit object ParserFunctor extends Functor[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      Parser(fa.parserState.map(f))
  }

  implicit object ParserApplicative extends Applicative[Parser] {
    val F: Functor[Parser] = ParserFunctor

    def pure[A](a: A): Parser[A] =
      Parser(M.Ap.pure(a))

    def ap[A, B](fab: Parser[A => B])(fa: Parser[A]): Parser[B] =
      Parser(fab.parserState <*> fa.parserState)

  }

  implicit object ParserMonad extends Monad[Parser] {
    val Ap: Applicative[Parser] = ParserApplicative

    def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] =
      Parser(ma.parserState >>= { a => f(a).parserState })
  }

  implicit object ParserError extends MonadError[Parser] {
    val M: Monad[Parser] = ParserMonad

    def throwError[A](message: String): Parser[A] =
      Parser(MonadError[ParserStateM].throwError(message))

    def catchError[A](ma: Parser[A])(f: String => Parser[A]): Parser[A] =
      Parser(MonadError[ParserStateM].catchError(ma.parserState)(f(_).parserState))

  }

  implicit object ParserAlternative extends Alternative[Parser] {
    val Ap: Applicative[Parser] = ParserApplicative

    def empty[A]: Parser[A] =
      Parser(Alt.empty)

    def orElse[A](fa1: => Parser[A])(fa2: => Parser[A]): Parser[A] =
      Parser(fa1.parserState <|> fa2.parserState)
  }

  def parse[A](parser: Parser[A], input: String): Either[String, A] = {
    runParser(parser <* endOfInput, input).map(_._1)
  }

  def parseSome[A](parser: Parser[A], input: String): Either[String, (A, String)] = {
    runParser(parser, input) match {
      case Left(err) => Left(err)
      case Right((result, state)) => Right(result, state.input.substring(state.pos))
    }
  }

  private def runParser[A](parser: Parser[A], input: String): Either[String, (A, ParseState)] = {
    import Identity.runIdentity
    import EitherT.runEitherT
    import StateT.runStateT
    runIdentity(runEitherT(runStateT(parser.parserState, ParseState.init(input))))
  }

  private def get: Parser[ParseState] = Parser(StateT.get)
  private def put(state: ParseState): Parser[()] = Parser(StateT.put(state))
  private def modify(f: ParseState => ParseState): Parser[()] = Parser(StateT.modify(f))

  def throwError[A](message: String): Parser[A] =
    get >>= { state =>
      ParserError.throwError(s"$message at line = ${state.lineNum}, char = ${state.charNum}")
    }

  def catchError[A](ma: Parser[A])(f: String => Parser[A]): Parser[A] =
    ParserError.catchError(ma)(f)

  object syntax {
    implicit class ParserOps[A](self: Parser[A]) {
      def named(name: String): Parser[A] =
        ParserError.catchError(self) { msg => ParserError.throwError(s"$name: $msg") }
    }
  }

  import syntax._

  def char(c: Char): Parser[Char] =
    catchError(satisfy(_ == c)) { _ =>
      throwError(s"char: expected '$c'")
    }

  def space: Parser[Char] =
    catchError(satisfy(_.isSpaceChar)) { _ =>
      throwError(s"space: expected space character")
    }

  def endOfInput: Parser[Unit] = {
    val p: Parser[Unit] = peek >>= {
      case None => pure(())
      case _ => throwError("expected end of input")
    }
    p.named("endOfInput")
  }

  def satisfy(f: Char => Boolean): Parser[Char] = {
    val p: Parser[Char] = peek >>= {
      case Some(c) if f(c) => advance1 *> pure(c)
      case _ => throwError("predicate failed")
    }
    p.named("satisfy")
  }

  def peek: Parser[Option[Char]] = {
    val p: Parser[Option[Char]] = get >>= { state =>
      if (state.pos == state.input.length) {
        pure(None)
      } else if (state.pos > state.input.length) {
        throwError("end of input")
      } else {
        pure(Some(state.input.charAt(state.pos)))
      }
    }
    p.named("peek")
  }

  def takeWhile(f: Char => Boolean): Parser[String] = {
    def collect(s: String): Parser[String] =
      peek >>= {
        case Some(c) if f(c) => advance(1) >> collect(s :+ c)
        case _ => pure(s)
      }

    collect("").named("takeWhile")
  }

  def takeWhile1(f: Char => Boolean): Parser[String] = {
    val p: Parser[String] = peek >>= {
      case Some(c) if f(c) => advance(1) >> takeWhile(f).map(c +: _)
      case Some(c) if !f(c) => throwError("predicate failed")
      case None => throwError("end of input")
    }
    p.named("takeWhile1")
  }

  def advance(i: Int): Parser[Unit] =
    replicateA(i, advance1).named("advance") *> pure(())

  def advance1: Parser[Unit] = {
    val p: Parser[Unit] = get >>= { state =>
      if (state.pos < state.input.length) {
        put(step(state))
      } else {
        throwError("end of input")
      }
    }
    p.named("advance1")
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
