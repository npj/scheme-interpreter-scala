package io.npj.scheme

case class SchemeParser(parser: Parser[Program])
case class Program(expressions: Seq[Expression])
case class Expression(tokens: Seq[Token])

sealed abstract class Token
case class IdentifierToken(value: String) extends Token
case class StringToken(value: String) extends Token
case class IntegerToken(value: BigInt) extends Token
case class FloatToken(value: BigDecimal) extends Token
case class BooleanToken(value: Boolean) extends Token
case class CharToken(value: Char) extends Token

object SchemeParser {
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

  def token: Parser[Token] = {
    val bool: Parser[Token] = boolean.map(BooleanToken)
    val char: Parser[Token] = character.map(CharToken)
    val int: Parser[Token] = integer.map(IntegerToken)
    val fl: Parser[Token] = float.map(FloatToken)
    val ident: Parser[Token] = identifier.map(IdentifierToken)
    val str: Parser[Token] = stringLit.map(StringToken)
    bool <|> char <|> int <|> fl <|> ident <|> str
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

    (char('"') *> many(stringChar).map(_.mkString) <* char('"')).named("stringLit")
  }

  def identifier: Parser[String] =
    anyChar >>= { first =>
      takeWhile1(inClass("a-zA-Z0-9!@#$%^&*-_+=~,.<>?:")) >>= { rest =>
        if (first == '#' || first == ',') {
          throwError("identifiers may not start with '#' or ','")
        } else {
          pure(first +: rest)
        }
      }
    }
}