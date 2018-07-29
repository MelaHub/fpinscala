package com.functionalprogramming.chapt09

import com.functionalprogramming.chapt09.Parsers.ParseError
import org.scalatest._

class ParserSpec
  extends FlatSpec
    with Matchers {

  type Parser[A] = String => A

  val parserImpl = new ParsersImpl()

  import parserImpl._

  "Running a parser on a character" should "work" in {
    parserImpl.run(char('c'))("c") == Right('c')
    parserImpl.run(char('c'))("def").isLeft
  }

  "Running a parser on a string" should "work" in {
    parserImpl.run(string("aString"))("aString") == Right("aString")
    parserImpl.run(string("aString"))("anotherString").isLeft
  }

  "Running or on two strings" should "work" in {
    parserImpl.run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
    parserImpl.run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")
    parserImpl.run(or(string("abra"), string("cadabra")))("simsalbim").isLeft
  }

  // "Running or a list" should "work" in {
  //   run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  //   run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  //   run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
  // }

  "Running countChar" should "return all consecutive characters found from the beginning" in {
    parserImpl.run(countChar('a'))("aa") == Right(2)
    parserImpl.run(countChar('a'))(" ") == Right(0)
    parserImpl.run(countChar('a'))("b123") == Right(0)
    parserImpl.run(countChar('a'))("b1a3") == Right(0)
  }

  "Running countCharOrFail" should "give an appropriate error" in {
    parserImpl.run(countCharOrFail('a'))("aa") == Right(2)
    parserImpl.run(countCharOrFail('a'))(" ").isLeft
    parserImpl.run(countCharOrFail('a'))("b123").isLeft
  }

  "Running countConsecutiveChars" should "work" in {
    parserImpl.run(countConsecutiveChars('a', 'b'))("aa") == Right((2, 0))
    parserImpl.run(countConsecutiveChars('a', 'b'))("bbb") == Right((0, 3))
    parserImpl.run(countConsecutiveChars('a', 'b'))("aaaab") == Right((4, 1))
  }
}
