package com.functionalprogramming.chapt04

import org.scalatest._

class EitherSpec
  extends FlatSpec
    with Matchers {

  "The map on Either" should "work" in {
    val f = (x: Int) => x + 1
    Left(1).map(f) shouldEqual Left(1)
    Right(1) map f shouldEqual Right(2)
  }

  "The flatmap on Either" should "work" in {
    val f = (x: String) => try {
      Right(x.toInt)
    } catch {case e: Exception => Left(x)}

    Right("1").flatMap(f) shouldEqual Right(1)
    Right("hello").flatMap(f) shouldEqual Left("hello")
    Left("banana").flatMap(f) shouldEqual Left("banana")
  }

  "The orElse on Either" should "work" in {
    val default = Right(42)

    Right(4) orElse default shouldEqual Right(4)
    Left(4) orElse default shouldEqual default
  }

  "The map2 on Either" should "work" in {
    val f = (x: Int, y: Int) => x + y
    Right(4).map2(Right(5))(f) shouldEqual Right(9)
    Right(5).map2(Left(4))(f) shouldEqual Left(4)
    Left(2).map2(Left(3))(f) shouldEqual Left(2)
    Left(1).map2(Left(6))(f) shouldEqual Left(1)
  }

  "The traverse method" should "work" in {
    val f = (x: String) => try {
      Right(x.toInt)
    } catch {case e: Exception => Left(x)}
    Either.traverse(List())(f) shouldEqual Right(List())
  }


}
