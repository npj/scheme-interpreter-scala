package io.npj.scheme

import io.npj.scheme.Parser.ParserStateM
import io.npj.scheme.cat.trans.{EitherT, StateT}
import io.npj.scheme.cat.{Alternative, Identity, Monoid}

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
  import io.npj.scheme.cat.Cons.+:

  type ParserStateM[A] = StateT[({ type lam[T] = EitherT[Identity, String, T] })#lam, ParseState, A]

  private val M = Monad[ParserStateM]
  private val Alt = Alternative[ParserStateM]

  implicit object ParserFunctor extends Functor[Parser] {
    def map[A, B](fa: => Parser[A])(f: A => B): Parser[B] =
      Parser(fa.parserState.map(f))
  }

  implicit object ParserApplicative extends Applicative[Parser] {
    val F: Functor[Parser] = ParserFunctor

    def pure[A](a: => A): Parser[A] =
      Parser(M.Ap.pure(a))

    def ap[A, B](fab: => Parser[A => B])(fa: => Parser[A]): Parser[B] =
      Parser(fab.parserState <*> fa.parserState)
  }

  implicit object ParserMonad extends Monad[Parser] {
    val Ap: Applicative[Parser] = ParserApplicative

    def flatMap[A, B](ma: => Parser[A])(f: A => Parser[B]): Parser[B] =
      Parser(ma.parserState >>= { a => f(a).parserState })
  }

  implicit object ParserError extends MonadError[Parser] {
    val M: Monad[Parser] = ParserMonad

    def throwError[A](message: String): Parser[A] =
      Parser(MonadError[ParserStateM].throwError(message))

    def catchError[A](ma: => Parser[A])(f: String => Parser[A]): Parser[A] =
      Parser(MonadError[ParserStateM].catchError(ma.parserState)(f(_).parserState))
  }

  implicit object ParserAlternative extends Alternative[Parser] {
    val Ap: Applicative[Parser] = ParserApplicative

    def empty[A]: Parser[A] =
      Parser(Alt.empty)

    def orElse[A](fa1: => Parser[A])(fa2: => Parser[A]): Parser[A] =
      Parser(fa1.parserState <|> fa2.parserState)
  }

  object syntax {
    implicit class ParserOps[A](self: Parser[A]) {
      def named(name: String): Parser[A] =
        Parser.named(self)(name)

      def sepBy[U](s: Parser[U]): Parser[Seq[A]] =
        Parser.sepBy(self)(s)

      def sepBy1[U](s: Parser[U]): Parser[Seq[A]] =
        Parser.sepBy1(self)(s)
    }
  }

  import syntax._

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

  def char(c: Char): Parser[Char] =
    catchError(satisfy(_ == c)) { _ =>
      throwError(s"char: expected '$c'")
    }

  def space: Parser[Char] =
    catchError(satisfy(_.isWhitespace)) { _ =>
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

  def ensure: Parser[Char] = {
    val p: Parser[Char] = peek >>= {
      case Some(c) => pure(c)
      case _ => throwError("end of input")
    }
    p.named("ensure")
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
      case Some(c) if f(c) => advance1 *> takeWhile(f).map(c +: _)
      case Some(c) if !f(c) => throwError("predicate failed")
      case None => throwError("end of input")
    }
    p.named("takeWhile1")
  }

  def digits: Parser[String] =
    takeWhile1(_.isDigit)

  def string(toMatch: String): Parser[String] = {
    def collect(s: String, i: Int): Parser[String] = {
      if (s.charAt(i) == toMatch.charAt(i)) {
        if (s.length == toMatch.length) {
          pure(s) <* advance1
        } else {
          advance1 >> (ensure >>= { c => collect(s"$s$c", i + 1) })
        }
      } else {
        throwError(s"expected '${toMatch.charAt(i)}'")
      }
    }
    (ensure >>= { c => collect(s"$c", 0) }).named("string")
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

  def inClass(charClass: String)(c: Char): Boolean = {
    def parseClass(str: String): Seq[Char] =
      (str: Seq[Char]) match {
        case a +: '-' +: b +: rest => a.to(b) ++ parseClass(rest.toString)
        case a +: rest => a +: parseClass(rest.toString)
        case _ => ""
      }
    Set.from(parseClass(charClass)).contains(c)
  }

  def named[A](p: Parser[A])(name: String): Parser[A] =
    ParserError.catchError(p) { msg => ParserError.throwError(s"$name: $msg") }

  def sepBy[A, S](p: Parser[A])(s: Parser[S]): Parser[Seq[A]] =
    (p.map(+:) <*> (s *> sepBy1(p)(s) <|> pure(Seq()))) <|> pure(Seq())

  def sepBy1[A, S](p: Parser[A])(s: Parser[S]): Parser[Seq[A]] =
    p.map(+:) <*> ((s *> sepBy1(p)(s)) <|> pure(Seq()))

  private def step(state: ParseState): ParseState = {
    val newPos = state.pos + 1
    if (state.input.charAt(state.pos) == '\n') {
      state.copy(pos = newPos, charNum = 1, lineNum = state.lineNum + 1)
    } else {
      state.copy(pos = newPos, charNum = state.charNum + 1)
    }
  }

  implicit def ParserMonoid[A: Monoid]: Monoid[Parser[A]] = new Monoid[Parser[A]] {
    private val M = Monoid[A]

    def empty: Parser[A] = ParserApplicative.pure(M.empty)

    def append(a1: => Parser[A])(a2: => Parser[A]): Parser[A] =
      a1.map { a: A => b: A => M.append(a)(b) } <*> a2
  }
}
