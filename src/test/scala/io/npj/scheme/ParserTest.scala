package io.npj.scheme

class ParserTest extends org.scalatest.FunSuite {
  import Parser._
  import cat.implicits._

  test("peek") {
    assert(runParser(peek, "bcd") == Right(Some('b')))
  }

  test("advance and peek") {
    assert(runParser(advance(1) >> peek, input = "bcd") == Right(Some('c')))
  }

  test("peek, no input") {
    assert(runParser(peek, "") == Right(None))
  }

}
