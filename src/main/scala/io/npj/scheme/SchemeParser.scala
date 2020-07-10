package io.npj.scheme

case class SchemeParser(parser: Parser[Seq[Expression]])
case class Expression(tokens: Seq[Token])

sealed abstract class Token
case class IdentToken(value: String) extends Token
case class StringToken(value: String) extends Token
case class IntegerToken(value: BigInt) extends Token
case class DoubleToken(value: BigDecimal) extends Token

object SchemeParser {
  import Parser._
  import Parser.syntax._
  import io.npj.scheme.cat.Functor.syntax._
  import io.npj.scheme.cat.Applicative.syntax._

  def expression: Parser[Expression] = ???
    ///char('(') *> many(token) <* char(')')

  def token: Parser[Token] = ???

  def decimal: Parser[Int] = {
    val p: Parser[Int] = takeWhile1(_.isDigit).map { s =>
      s.foldLeft(0) { (sum, c) => (sum * 10) + (c.toInt - 48) }
    }
    p.named("decimal")
  }
}