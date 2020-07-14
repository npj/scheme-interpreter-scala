package io.npj.scheme

import io.npj.scheme.Parser.{parse, parseSome}
import org.scalatest.FunSuite

class SchemeParserTest extends FunSuite {
  import SchemeParser._
  test("integer") {
    assert(parse(integer, "1234") == Right(1234))
    assert(parse(integer, "12341234123412341234123412341234") == Right(BigInt("12341234123412341234123412341234")))
    assert(parseSome(integer, "1234abcd") == Right(1234, "abcd"))
    assert(parse(integer, "00001234") == Right(1234))
    assert(parse(integer, "a1234") == Left("integer: takeWhile1: predicate failed at line = 1, char = 1"))
  }

  test("signed integer") {
    assert(parse(signed(integer), "-1234") == Right(-1234))
    assert(parse(signed(integer), "+12341234123412341234123412341234") == Right(BigInt("12341234123412341234123412341234")))
    assert(parseSome(signed(integer), "1234abcd") == Right(1234, "abcd"))
    assert(parse(signed(integer), "-00001234") == Right(-1234))
    assert(parse(signed(integer), "+a1234") == Left("integer: takeWhile1: predicate failed at line = 1, char = 1"))
  }

  test("float") {
    assert(parse(float, input = "123.") == Right(123f))
    assert(parse(float, input = ".123") == Right(0.123))
    assert(parse(float, input = "123.123") == Right(123.123))
    assert(parse(float, input = "123.123e123") == Right(123.123e123))
    assert(parse(float, input = "123.123E123") == Right(123.123e123))
    assert(parse(float, input = "123.123E-123") == Right(123.123e-123))
    assert(parse(float, input = "123.123E+123") == Right(123.123e123))
    assert(parse(float, input = "123.e123") == Right(123.0e123))
    assert(parse(float, input = "123.E123") == Right(123.0e123))
    assert(parse(float, input = "123.E-123") == Right(123.0e-123))
    assert(parse(float, input = "123.E+123") == Right(123.0e123))
    assert(parse(float, input = "a123.123E+123") == Left("float: string: expected '.' at line = 1, char = 1"))
  }

  test("signed float") {
    assert(parse(signed(float), input = "-123.") == Right(-123f))
    assert(parse(signed(float), input = "+.123") == Right(0.123))
    assert(parse(signed(float), input = "123.123") == Right(123.123))
    assert(parse(signed(float), input = "+123.123e123") == Right(123.123e123))
    assert(parse(signed(float), input = "-123.123E123") == Right(-123.123e123))
    assert(parse(signed(float), input = "123.123E-123") == Right(123.123e-123))
    assert(parse(signed(float), input = "123.123E+123") == Right(123.123e123))
    assert(parse(signed(float), input = "+123.e123") == Right(123.0e123))
    assert(parse(signed(float), input = "-123.E123") == Right(-123.0e123))
    assert(parse(signed(float), input = "123.E-123") == Right(123.0e-123))
    assert(parse(signed(float), input = "123.E+123") == Right(123.0e123))
    assert(parse(signed(float), input = "-a123.123E+123") == Left("float: string: expected '.' at line = 1, char = 1"))
  }

  test("boolean") {
    assert(parse(boolean, input = "#t") == Right(true))
    assert(parse(boolean, input = "#f") == Right(false))
    assert(parse(boolean, input = "#c") == Left("string: expected 'f' at line = 1, char = 2"))
  }

  test("character") {
    assert(parse(character, input = "#\\c") == Right('c'))
    assert(parse(character, input = "#\\h") == Right('h'))
    assert(parse(character, input = "#\\\\") == Right('\\'))
  }
}
