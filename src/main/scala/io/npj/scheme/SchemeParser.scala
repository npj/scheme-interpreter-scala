package io.npj.scheme

case class SchemeParser(parser: Parser[Program])
case class Program(expressions: Seq[Expression])

sealed abstract class Expression
case class ListExp(expressions: Seq[Expression]) extends Expression
case class Quoted(value: Expression) extends Expression
case class Symbol(value: String) extends Expression
case class Identifier(value: String) extends Expression

sealed abstract class SelfEvaluating extends Expression
case class StringToken(value: String) extends SelfEvaluating
case class IntegerToken(value: BigInt) extends SelfEvaluating
case class FloatToken(value: BigDecimal) extends SelfEvaluating
case class BooleanToken(value: Boolean) extends SelfEvaluating
case class CharToken(value: Char) extends SelfEvaluating

object Quoted {
  def apply(v: Expression): Quoted =
    v match {
      case i: Identifier => new Quoted(Symbol(i.value))
      case _ => new Quoted(v)
    }
}

object SchemeParser {
  import Function.const
  import Parser._
  import Parser.syntax._
  import io.npj.scheme.cat.Functor.syntax._
  import io.npj.scheme.cat.Applicative.syntax._
  import io.npj.scheme.cat.Monad.syntax._
  import io.npj.scheme.cat.Alternative.syntax._
  import io.npj.scheme.cat.Monoid.syntax._

  private val escapeChars = Map(
    'n' -> '\n',
    'r' -> '\r',
    't' -> '\t',
    '\"' -> '\"',
    '\\' -> '\\'
  )

  private val escapeCodes = escapeChars.keys.mkString

  def token: Parser[Expression] = (
      numOrIdent <|>
      boolean.map(BooleanToken) <|>
      character.map(CharToken) <|>
      stringLit.map(StringToken)
  ).named("token")

  def numOrIdent: Parser[Expression] = {
    val nonNum: Parser[Unit] = ensure(notInClass("a-zA-Z.")) >>= const(pure())
    val tryFloat: Parser[Expression] = (float <* (nonNum <|> endOfInput)).map(FloatToken)
    val tryInt: Parser[Expression] = (integer <* (nonNum <|> endOfInput)).map(IntegerToken)
    val tries: Parser[Expression] = tryFloat <|> tryInt <|> identifier.map(Identifier)
    tries.named("numOrIdent")
  }

  def integer: Parser[BigInt] =
    digits.map(BigInt(_)).named("integer")

  def float: Parser[BigDecimal] = {
    val exponent: Parser[String] =
      (string("e") <|> string("E")) <> signed(integer).map(_.toString)

    val significand: Parser[String] =
      (digits <> string(".") <> digits) <|>
      (digits <> string(".")) <|>
      (string(".") <> digits)

    val parser: Parser[String] =
      (significand <> exponent) <|>
      (digits <> exponent) <|>
      significand

    parser.map(BigDecimal(_)).named("float")
  }

  def signed[A: Numeric](p: Parser[A]): Parser[A] =
    (char('-') *> p).map(Numeric[A].negate) <|>
    (char('+') *> p) <|>
    p

  def boolean: Parser[Boolean] =
    (string("#t") *> pure(true)) <|> (string("#f") *> pure(false))

  def character: Parser[Char] =
    char('#') *> char('\\') *> anyChar

  def stringLit: Parser[String] = {
    def escape: Parser[Char] = satisfy(inClass(escapeCodes)) >>= { c => pure(escapeChars(c)) }

    def escapeChar: Parser[Char] =
      char('\\') *> escape

    def stringChar: Parser[Char] =
      escapeChar <|> satisfy(notInClass("\"\n"))

    (char('"') *> many(stringChar) <* char('"')).named("stringLit").map(_.mkString)
  }

  def identifier: Parser[String] = {
    def legal: Char => Boolean = inClass("a-zA-Z0-9!@#$%^&*-_+=~,.<>?:")

    val parser: Parser[String] = satisfy(legal) >>= { first =>
      takeWhile(legal) >>= { rest =>
        if (first == '#' || first == ',' || first == '"') {
          throwError("may not start with '#', ',', or '\"'")
        } else {
          pure(first +: rest)
        }
      }
    }
    parser.named("identifier")
  }

  def expression: Parser[Expression] = {
    val q: Parser[Expression] =
      quoted.asInstanceOf[Parser[Expression]]

    val exp: Parser[Expression] =
      (expression <|> q <|> token).sepBy(some(space)).map(ListExp)

    (between('(', ')', exp) <|> q <|> token).named("expression")
  }

  def quoted: Parser[Quoted] =
    (char('\'') *> (expression <|> token).map(Quoted(_))).named("quoted")

  def program: Parser[Program] =
    ignoreSpace(expression.sepBy(some(space))).map(Program).named("program")
}