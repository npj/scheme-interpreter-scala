package io.npj.scheme

class ParserTest extends org.scalatest.FunSuite {
  import Parser._
  import io.npj.scheme.cat.Alternative.syntax._
  import io.npj.scheme.cat.Applicative.syntax._
  import io.npj.scheme.cat.Monad.syntax._

  test("peek") {
    assert(runParser(peek, "bcd") == Right(Some('b')))
  }

  test("advance and peek") {
    assert(runParser(advance(1) >> peek, input = "bcd") == Right(Some('c')))
    assert(runParser(advance(2) >> peek, input = "bcd") == Right(Some('d')))
  }

  test("peek, no input") {
    assert(runParser(peek, "") == Right(None))
  }

  test("char") {
    assert(runParser(char('('), input = "(") == Right('('))
    assert(runParser(char('('), input = ")") == Left("expected '(': line = 1, char = 1"))
  }

  test("takeWhile") {
    assert(runParser(char('(') *> takeWhile(_ != ')') <* char(')'), input = "(some) (stuff)") == Right("some"))
  }

  test("alternative") {
    assert(runParser((advance(3) >> peek) <|> pure(Some('x')), input = "bcd") == Right(None))
    assert(runParser((advance(4) >> peek) <|> pure(Some('x')), input = "bcd") == Right(Some('x')))
    assert(runParser((advance(4) >> peek) <|> (advance(1) >> peek), input = "bcd") == Right(Some('c')))
  }

  test("line and char tracking") {
    assert(runParser(advance(4) >> peek, input = "bcd") == Left("end of input: line = 1, char = 4"))
    assert(runParser(advance(5) >> peek, input = "\nbcd") == Left("end of input: line = 2, char = 4"))
    assert(runParser(advance(5) >> peek, input = "bc\nd") == Left("end of input: line = 2, char = 2"))
  }
}
