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
  import io.npj.scheme.cat.{Applicative, Functor, Monad, MonadError}
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

  implicit object ParserError extends MonadError[Parser] {
    def throwError[A](message: String): Parser[A] =
      Parser(MonadError[ParserStateM].throwError(message))

    def catchError[A](ma: Parser[A])(f: String => Parser[A]): Parser[A] =
      Parser(MonadError[ParserStateM].catchError(ma.parserState)(f(_).parserState))
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
  private val AL = Alternative[Parser]
  private val ME = MonadError[Parser]

  private def get: Parser[ParseState] = Parser(StateT.get)
  private def put(state: ParseState): Parser[()] = Parser(StateT.put(state))
  private def modify(f: ParseState => ParseState): Parser[()] = Parser(StateT.modify(f))

  private def throwError[A](message: String): Parser[A] =
    get >>= { state =>
      ME.throwError(s"$message at line = ${state.lineNum}, char = ${state.charNum}")
    }

  object syntax {
    implicit class ParserOps[A](self: Parser[A]) {
      def named(name: String): Parser[A] =
        ME.catchError(self) { msg => ME.throwError(s"$name: $msg") }
    }
  }

  import syntax._

  def char(c: Char): Parser[Char] = {
    val p: Parser[Char] = peek >>= {
      case Some(`c`) => A.pure(c) <* advance(1)
      case _ => throwError(s"expected '$c'")
    }
    p.named("char")
  }

  def peek: Parser[Option[Char]] = {
    val p: Parser[Option[Char]] = get >>= { state =>
      if (state.pos == state.input.length) {
        A.pure(None)
      } else if (state.pos > state.input.length) {
        throwError("end of input")
      } else {
        A.pure(Some(state.input.charAt(state.pos)))
      }
    }
    p.named("peek")
  }

  def takeWhile(f: Char => Boolean): Parser[String] = {
    def collect(s: String): Parser[String] =
      peek >>= {
        case Some(c) if f(c) => advance(1) >> collect(s :+ c)
        case _ => A.pure(s)
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

  def decimal: Parser[Int] = {
    val p: Parser[Int] = takeWhile1(_.isDigit).map { s =>
      s.foldLeft(0) { (sum, c) => (sum * 10) + (c.toInt - 48) }
    }
    p.named("decimal")
  }

  def advance(i: Int): Parser[Unit] =
    replicateA(i, advance1).named("advance") *> A.pure(())

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
