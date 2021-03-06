package io.npj.scheme

import org.scalatest.FunSuite

import scala.io.Source

class SchemeParserTest extends FunSuite {
  import SchemeParser._
  import Parser._
  import Parser.syntax._

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
    assert(parse(float, input = "123E0") == Right(123e0))
    assert(parse(float, input = "123.e") == Left("endOfInput: expected end of input at line = 1, char = 5"))
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
    assert(parse(signed(float), input = "123E0") == Right(123e0))
    assert(parse(signed(float), input = "123.e") == Left("endOfInput: expected end of input at line = 1, char = 5"))
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
    assert(parse(character.sepBy(space), input = "#\\c #\\h #\\a #\\r") == Right(Seq('c', 'h', 'a', 'r')))
  }

  test("identifier") {
    assert(parse(identifier, input = "a-valid-scheme-var?") == Right("a-valid-scheme-var?"))
    assert(parse(identifier, input = "a-valid-scheme-var!") == Right("a-valid-scheme-var!"))
    assert(parse(identifier, input = "a-valid->scheme-var") == Right("a-valid->scheme-var"))
    assert(parse(identifier, input = "a-valid-1234567890-scheme-var") == Right("a-valid-1234567890-scheme-var"))
    assert(parse(identifier, input = "a-valid-#scheme-var") == Right("a-valid-#scheme-var"))
    assert(parse(identifier, input = "a-valid,scheme-var") == Right("a-valid,scheme-var"))
    assert(parse(identifier, input = "12345.e") == Right("12345.e"))
    assert(parse(identifier, input = "n") == Right("n"))
    assert(parse(identifier, input = "#a-valid-scheme-var!") == Left("identifier: may not start with '#', ',', or '\"' at line = 1, char = 21"))
    assert(parse(identifier, input = ",a-valid-scheme-var!") == Left("identifier: may not start with '#', ',', or '\"' at line = 1, char = 21"))
  }

  test("string") {
    assert(parse(stringLit, input = "\"a quoted string\"") == Right("a quoted string"))
    assert(parse(stringLit, input = "\"a quoted string\\nwith escape\"") == Right("a quoted string\nwith escape"))
    assert(parse(stringLit, input = "\"a quoted \\\"string\\\" with escape\"") == Right("a quoted \"string\" with escape"))
    assert(parse(stringLit, input = "\"a 'quoted' \\\"string\\\" with\\tescape\\r\\n\"") == Right("a 'quoted' \"string\" with\tescape\r\n"))
    assert(parse(stringLit, input = "\"a non-terminated 'quoted' \\\"string\\\" with\\tescape\\r\\n") == Left("stringLit: char: expected '\"' at line = 1, char = 55"))
    assert(parse(stringLit, input = "\"a non-terminated multiline\n'quoted' \\\"string\\\" with\\tescape\\r\\n") == Left("stringLit: char: expected '\"' at line = 1, char = 28"))
  }

  test("numOrIdent") {
    assert(parse(numOrIdent, input = "12345e0") == Right(FloatToken(12345e0)))
    assert(parse(numOrIdent, input = "12345.e") == Right(Identifier("12345.e")))
    assert(parse(numOrIdent, input = "12345e") == Right(Identifier("12345e")))
    assert(parse(numOrIdent, input = "12345") == Right(IntegerToken(12345)))
  }

  test("token") {
    assert(parseSome(token, input = "#t rest") == Right(BooleanToken(true), " rest"))
    assert(parseSome(token, input = "#f rest") == Right(BooleanToken(false), " rest"))
    assert(parseSome(token, input = "#\\t rest") == Right(CharToken('t'), " rest"))
    assert(parseSome(token, input = "#\\f rest") == Right(CharToken('f'), " rest"))
    assert(parseSome(token, input = "12345. rest") == Right(FloatToken(12345), " rest"))
    assert(parseSome(token, input = "12345.6789 rest") == Right(FloatToken(12345.6789), " rest"))
    assert(parseSome(token, input = "12345 rest") == Right(IntegerToken(12345), " rest"))
    assert(parseSome(token, input = "first rest") == Right(Identifier("first"), " rest"))
    assert(parseSome(token, input = "123first rest") == Right(Identifier("123first"), " rest"))
    assert(parseSome(token, input = "\"123\\tfirst\" rest") == Right(StringToken("123\tfirst"), " rest"))
    assert(parseSome(token, input = "\"123\nfirst\" rest") == Left("token: stringLit: char: expected '\"' at line = 1, char = 5"))
  }

  test("quoted") {
    assert(parse(quoted, input = "'(1 2 3 4)") == Right(Quoted(ListExp(Seq(1, 2, 3, 4).map(IntegerToken(_))))))
    assert(parse(quoted, input = "'1") == Right(Quoted(IntegerToken(1))))
    assert(parse(quoted, input = "'1.1e10") == Right(Quoted(FloatToken(1.1e10))))
    assert(parse(quoted, input = "'#t") == Right(Quoted(BooleanToken(true))))
    assert(parse(quoted, input = "'#\\c") == Right(Quoted(CharToken('c'))))
    assert(parse(quoted, input = "'()") == Right(Quoted(ListExp(Seq()))))
    assert(parse(quoted, input = "'im-a-symbol") == Right(Quoted(Symbol("im-a-symbol"))))
    assert(parse(quoted, input = "'\"a quoted string\"") == Right(Quoted(StringToken("a quoted string"))))
  }

  test("expression") {
    assert(parse(expression, input = "(abba baab abab baba)") == Right(ListExp(Seq(Identifier("abba"), Identifier("baab"), Identifier("abab"), Identifier("baba")))))
    assert(parse(expression, input = "(abba baab\nabab baba)") == Right(ListExp(Seq(Identifier("abba"), Identifier("baab"), Identifier("abab"), Identifier("baba")))))
    assert(parse(expression, input = "(abba (baab abab) baba)") == Right(ListExp(Seq(Identifier("abba"), ListExp(Seq(Identifier("baab"), Identifier("abab"))), Identifier("baba")))))
    assert(parse(expression, input = "(abba (baab\nabab) baba)") == Right(ListExp(Seq(Identifier("abba"), ListExp(Seq(Identifier("baab"), Identifier("abab"))), Identifier("baba")))))
    assert(parse(expression, input = "(abba (baab abab))") == Right(ListExp(Seq(Identifier("abba"), ListExp(Seq(Identifier("baab"), Identifier("abab")))))))
    assert(parse(expression, input = "(abba\n(baab abab))") == Right(ListExp(Seq(Identifier("abba"), ListExp(Seq(Identifier("baab"), Identifier("abab")))))))
    assert(parse(expression, input = "(abba (baab abab)\n)") == Right(ListExp(Seq(Identifier("abba"), ListExp(Seq(Identifier("baab"), Identifier("abab")))))))
    assert(parse(expression, input = "(abba\n(baab abab)\n)") == Right(ListExp(Seq(Identifier("abba"), ListExp(Seq(Identifier("baab"), Identifier("abab")))))))
    assert(parse(expression, input = "(map fact '(1 2 3 4 5))") == Right(ListExp(Seq(Identifier("map"), Identifier("fact"), Quoted(ListExp(Seq(IntegerToken(1), IntegerToken(2), IntegerToken(3), IntegerToken(4), IntegerToken(5))))))))
  }

  test("program") {
    val exp = ListExp(Seq(Identifier("abba"), Identifier("baab"), Identifier("abab"), Identifier("baba")))
    assert(parse(program, input = "(abba baab abab baba)\n\n(abba baab abab baba)") == Right(Program(Seq(exp, exp))))

    val inputStream = getClass.getClassLoader.getResourceAsStream("test.scm")
    val input = Source.fromInputStream(inputStream).getLines().mkString("\n")
    val ast = Program(
      Seq(
        ListExp(
          Seq(
            Identifier("define"),
            ListExp(
              Seq(
                Identifier("fact"),
                Identifier("n")
              )
            ),
            ListExp(
              Seq(
                Identifier("if"),
                ListExp(
                  Seq(
                    Identifier("<="),
                    Identifier("n"),
                    IntegerToken(0)
                  )
                ),
                IntegerToken(1),
                ListExp(
                  Seq(
                    Identifier("*"),
                    Identifier("n"),
                    ListExp(
                      Seq(
                        Identifier("fact"),
                        ListExp(
                          Seq(
                            Identifier("-"),
                            Identifier("n"),
                            IntegerToken(1)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        ListExp(
          Seq(
            Identifier("define"),
            ListExp(
              Seq(
                Identifier("outer"),
                Identifier("n")
              )
            ),
            ListExp(
              Seq(
                Identifier("define"),
                ListExp(
                  Seq(
                    Identifier("inner"),
                    Identifier("x")
                  )
                ),
                ListExp(
                  Seq(
                    Identifier("if"),
                    ListExp(
                      Seq(
                        Identifier("="),
                        Identifier("n"),
                        Identifier("x")
                      )
                    ),
                    Identifier("x"),
                    IntegerToken(20)
                  )
                )
              )
            ),
            ListExp(
              Seq(
                Identifier("inner"),
                IntegerToken(5)
              )
            )
          )
        ),
        ListExp(
          Seq(
            Identifier("map"),
            Identifier("fact"),
            Quoted(
              ListExp(
                Seq(
                  IntegerToken(1),
                  IntegerToken(2),
                  IntegerToken(3),
                  IntegerToken(4),
                  IntegerToken(5),
                  IntegerToken(6)
                )
              )
            )
          )
        )
      )
    )
    assert(parse(program, input) == Right(ast))
  }
}
