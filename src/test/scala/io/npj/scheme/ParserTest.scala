package io.npj.scheme

class ParserTest extends org.scalatest.FunSuite {

  import io.npj.scheme.cat.Functor.syntax._
  import io.npj.scheme.cat.Applicative.syntax._
  import io.npj.scheme.cat.Monad.syntax._
  import io.npj.scheme.cat.Alternative.syntax._
  import Parser._
  import Parser.syntax._

  test("peek") {
    assert(parseSome(peek, "bcd") == Right(Some('b'), "bcd"))
  }

  test("advance and peek") {
    assert(parseSome(advance(1) >> peek, input = "bcd") == Right(Some('c'), "cd"))
    assert(parseSome(advance(2) >> peek, input = "bcd") == Right(Some('d'), "d"))
  }

  test("peek, no input") {
    assert(parse(peek, "") == Right(None))
  }

  test("char") {
    assert(parse(char('('), input = "(") == Right('('))
    assert(parse(char('('), input = ")") == Left("char: expected '(' at line = 1, char = 1"))
  }

  test("notChar") {
    assert(parse(notChar('('), input = ")") == Right(')'))
    assert(parse(notChar('('), input = "(") == Left("notChar: unexpected '(' at line = 1, char = 1"))
  }

  test("anyChar") {
    assert(parse(anyChar, input = "a") == Right('a'))
    assert(parse(anyChar, input = "b") == Right('b'))
    assert(parse(anyChar, input = "") == Left("anyChar: ensure: end of input at line = 1, char = 1"))
  }

  test("takeWhile") {
    assert(parseSome(char('(') *> takeWhile(_ != ')') <* char(')'), input = "(some) (stuff)") == Right("some", " (stuff)"))
    assert(parse(char('(') *> takeWhile(_ != ')') <* char(')'), input = "(some") == Left("char: expected ')' at line = 1, char = 6"))
    assert(parse(char('(') *> takeWhile(_ != ')') <* char(')'), input = "()") == Right(""))
  }

  test("takeWhile1") {
    assert(parseSome(char('(') *> takeWhile1(_ != ')') <* char(')'), input = "(some) (stuff)") == Right("some", " (stuff)"))
    assert(parse(char('(') *> takeWhile1(_ != ')') <* char(')'), input = "(some") == Left("char: expected ')' at line = 1, char = 6"))
    assert(parse(char('(') *> takeWhile1(_ != ')') <* char(')'), input = "()") == Left("takeWhile1: predicate failed at line = 1, char = 2"))
    assert(parse(char('(') *> takeWhile1(_ != ')') <* char(')'), input = "(a)") == Right("a"))
  }

  test("alternative") {
    assert(parse((advance(3) >> peek) <|> pure(Some('x')), input = "bcd") == Right(None))
    assert(parseSome((advance(4) >> peek) <|> pure(Some('x')), input = "bcd") == Right(Some('x'), "bcd"))
    assert(parseSome((advance(4) >> peek) <|> (advance(1) >> peek), input = "bcd") == Right(Some('c'), "cd"))
  }

  test("line and char tracking") {
    assert(parse(advance(4) >> peek, input = "bcd") == Left("advance: advance1: end of input at line = 1, char = 4"))
    assert(parse(advance(5) >> peek, input = "\nbcd") == Left("advance: advance1: end of input at line = 2, char = 4"))
    assert(parse(advance(5) >> peek, input = "bc\nd") == Left("advance: advance1: end of input at line = 2, char = 2"))
  }

  test("many") {
    val parser = many(space)
    assert(parse(parser, input = "    ") == Right(Seq(' ', ' ', ' ', ' ')))
    assert(parse(char('(') *> parser <* char(')'), input = "(    )") == Right(Seq(' ', ' ', ' ', ' ')))
    assert(parse(parser, input = "") == Right(Seq()))
  }

  test("some") {
    val parser = some(space)
    assert(parse(parser, input = "    ") == Right(Seq(' ', ' ', ' ', ' ')))
    assert(parse(char('(') *> parser <* char(')'), input = "(    )") == Right(Seq(' ', ' ', ' ', ' ')))
    assert(parse(parser, input = "") == Left("space: expected space character at line = 1, char = 1"))
  }

  test("inClass") {
    assert(parseSome(satisfy(inClass("abc")), "bcd") == Right('b', "cd"))
    assert(parse(takeWhile(inClass("a-zA-Z123-")), "The-3") == Right("The-3"))
    assert(parse(takeWhile(inClass("-a-zA-Z123")), "The-3") == Right("The-3"))
    assert(parseSome(takeWhile(inClass("a-zA-Z123")), "The4") == Right("The", "4"))
    assert(parseSome(satisfy(inClass("abc")), "xyz") == Left("satisfy: predicate failed at line = 1, char = 1"))
  }

  test("notInClass") {
    assert(parseSome(satisfy(notInClass("abc")), "bcd") == Left("satisfy: predicate failed at line = 1, char = 1"))
    assert(parseSome(satisfy(notInClass("a-zA-Z123-")), "The-3") == Left("satisfy: predicate failed at line = 1, char = 1"))
    assert(parseSome(satisfy(notInClass("-a-zA-Z123")), "The-3") == Left("satisfy: predicate failed at line = 1, char = 1"))
    assert(parseSome(satisfy(notInClass("a-zA-Z123")), "The4") == Left("satisfy: predicate failed at line = 1, char = 1"))
    assert(parse(takeWhile(notInClass("abc")), "xyz") == Right("xyz"))
  }

  test("sepBy") {
    case class Sentence(words: Seq[String])
    val word = takeWhile1(inClass("a-zA-Z"))
    val spaces = many(space)
    val parser = (spaces *> word.sepBy(spaces) <* char('.')).map(Sentence)
    assert(parse(parser, input = "The quick brown fox     jumped over the lazy dog.") == Right(Sentence(Seq("The", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog"))))
    assert(parse(parser, input = "The quick brown fox \n\n    jumped\tover the lazy dog.") == Right(Sentence(Seq("The", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog"))))
    assert(parse(parser, input = "     .") == Right(Sentence(Seq())))
  }

  test("sepBy1") {
    case class Sentence(words: Seq[String])
    val word = takeWhile1(inClass("a-zA-Z"))
    val spaces = many(space)
    val parser = (spaces *> word.sepBy1(spaces) <* char('.')).map(Sentence)
    assert(parse(parser, input = "The quick brown fox     jumped over the lazy dog.") == Right(Sentence(Seq("The", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog"))))
    assert(parse(parser, input = "The quick brown fox \n\n    jumped\tover the lazy dog.") == Right(Sentence(Seq("The", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog"))))
    assert(parse(parser, input = "     .") == Left("takeWhile1: predicate failed at line = 1, char = 6"))
  }

  test("string") {
    assert(parseSome(string("test"), input = "testString") == Right("test", "String"))
    assert(parseSome(string("test"), input = "tesString") == Left("string: expected 't' at line = 1, char = 4"))
    assert(parse(string("test").sepBy(space), input = "test test") == Right(Seq("test", "test")))
  }
}
