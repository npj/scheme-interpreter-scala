package io.npj.scheme

import scala.math.ScalaNumber

case class SchemeParser(parser: Parser[Program])
case class Program(expressions: Seq[Expression])
case class Expression(tokens: Seq[Token])

sealed abstract class Token
case class IdentToken(value: String) extends Token
case class StringToken(value: String) extends Token
case class IntegerToken(value: BigInt) extends Token
case class DecimalToken(value: BigDecimal) extends Token

object SchemeParser {
  import Parser._
  import Parser.syntax._
  import io.npj.scheme.cat.Functor.syntax._
  import io.npj.scheme.cat.Applicative.syntax._
  import io.npj.scheme.cat.Monad.syntax._
  import io.npj.scheme.cat.Alternative.syntax._
  import io.npj.scheme.cat.Monoid.syntax._

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
    char('#') *> char('\\') *> ensure <* advance1

  def identifier: Parser[String] =
    (ensure <* advance1) >>= { first =>
      takeWhile1(inClass("a-zA-Z0-9!@#$%^&*-_+=~,.<>?:")) >>= { rest =>
        if (first == '#' || first == ',') {
          throwError("identifiers may not start with '#' or ','")
        } else {
          pure(first +: rest)
        }
      }
    }
}