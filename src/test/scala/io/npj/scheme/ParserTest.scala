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
    assert(runParser(advance(3) >> peek, input = "bcd") == Left("end of input"))
  }

  test("peek, no input") {
    assert(runParser(peek, "") == Right(None))
  }

  test("alternative") {
    assert(runParser((advance(3) >> peek) <|> pure(Some('x')), input = "bcd") == Right(Some('x')))
    assert(runParser((advance(3) >> peek) <|> (advance(1) >> peek), input = "bcd") == Right(Some('c')))
  }
}
