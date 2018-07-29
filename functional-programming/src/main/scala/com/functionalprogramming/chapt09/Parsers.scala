package com.functionalprogramming.chapt09

import com.functionalprogramming.chapt09.Parsers.{ParseError, Parser}


trait Parsers[ParseError, Parser[+_]] {

  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def char(c: Char): Parser[Char]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStrigParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def countChar(c: Char): Parser[Int]
  def countCharOrFail(c: Char): Parser[Int]
  def countConsecutiveChars(c1: Char, c2: Char): Parser[(Int, Int)]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]
  def many[A](p: Parser[A]): Parser[List[A]]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)
  }
}

class ParsersImpl extends Parsers[ParseError, Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    try {
      Right(p(input))
    } catch {
      case (e: Throwable) =>
        Left[ParseError, A](e.getMessage)
    }

  override implicit def char(c: Char): Parser[Char] =
    (s: String) =>
      if (s.equals(c.toString))
        c
      else
        throw new Error("Input string does not match")

  override implicit def string(s: String): Parser[String] =
    (x: String) =>
      if (x.equals(s))
        x
      else
        throw new Error("Input string does not match")

  override def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] =
    (s: String) =>
      try {
        s1(s)
      } catch {
        case _: Throwable => s2(s)
      }

  override def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

  def many[A](p: Parser[A]): Parser[List[A]] = ???

  def numA: Parser[Int] = char('a').many.map(_.size)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    (s: String) => f(a(s))

  override def countChar(c: Char): Parser[Int] = {
    def tailRecCountChar(currCount: Int, stringLeft: String): Int =
      stringLeft.headOption match {
        case Some(x) if x == c => tailRecCountChar(currCount + 1, stringLeft.tail)
        case _ => currCount
      }
    (s: String) => tailRecCountChar(0, s)
  }


  override def countCharOrFail(c: Char): Parser[Int] =
    (s: String) => {
      val occurrences = countChar(c)(s)
      if (occurrences == 0)
        throw new Error("No occurrences found")
      else
        occurrences
    }

  override def countConsecutiveChars(c1: Char, c2: Char): Parser[(Int, Int)] =
    (s: String) =>
      (countChar(c1)(s), countChar(c2)(s.reverse.takeWhile(_ != c1).reverse))
}

object Parsers {

  type Parser[+A] = String => A
  type ParseError = String

  def apply() = new ParsersImpl()

}


