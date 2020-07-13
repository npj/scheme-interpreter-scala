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
  import io.npj.scheme.cat.Alternative.syntax._
  import io.npj.scheme.cat.Monoid.syntax._

  def integer: Parser[BigInt] = {
    val p: Parser[BigInt] = digits.map { s =>
      s.foldLeft(BigInt(0)) { (sum, c) => (sum * 10) + (c.toInt - 48) }
    }
    p.named("integer")
  }

  def float: Parser[BigDecimal] = {
    val exponent: Parser[String] =
      (string("e") <|> string("E")) <> signed(integer).map(_.toString)

    val significand: Parser[String] =
      (digits <> string(".") <> digits) <|>
      (digits <> string(".")) <|>
      (string(".") <> digits)

    ((significand <> exponent) <|> significand).map(BigDecimal(_)).named("float")
  }

  def signed[A: Numeric](p: Parser[A]): Parser[A] =
    (char('-') *> p).map(implicitly[Numeric[A]].negate) <|>
    (char('+') *> p) <|>
    p
}