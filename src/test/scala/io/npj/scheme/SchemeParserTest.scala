package io.npj.scheme

import io.npj.scheme.Parser.parse
import org.scalatest.FunSuite

class SchemeParserTest extends FunSuite {
  import SchemeParser._
  test("decimal") {
    assert(parse(decimal, "1234") == Right(1234))
    assert(parse(decimal, "1234abcd") == Right(1234))
    assert(parse(decimal, "00001234") == Right(1234))
    assert(parse(decimal, "a1234") == Left("decimal: takeWhile1: predicate failed at line = 1, char = 1"))
  }
}
