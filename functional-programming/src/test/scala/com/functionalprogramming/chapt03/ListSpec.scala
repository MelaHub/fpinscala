package com.functionalprogramming.chapt03

import com.functionalprogramming.chapt03.List._
import org.scalatest._

class ListSpec
  extends FlatSpec
    with Matchers {

  "The pattern matching on lists " should "return the expected value" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x shouldEqual 3
  }

  "The tail method" should "work" in {
    tail(Nil) shouldEqual Nil
    tail(List(1)) shouldEqual Nil
    tail(List(1, 2)) shouldEqual List(2)
    tail(List(1, 2, 3, 4)) shouldEqual List(2, 3, 4)
  }

  "The setHead method" should "work" in {
    setHead(1, Nil) shouldEqual Nil
    setHead(1, List(2)) shouldEqual List(1)
    setHead(1, List(2, 3)) shouldEqual List(1, 3)
  }

  "The drop method" should "work" in {
    drop(Nil, 7) shouldEqual Nil
    drop(List(1, 2, 3), 1) shouldEqual List(2, 3)
    drop(List(1, 2, 3, 4, 5, 6), 4) shouldEqual List(5, 6)
  }

  "The dropWhile method" should "work" in {
    def f(i: Int) = i % 2 == 0
    dropWhile(Nil, f) shouldEqual Nil
    dropWhile(List(2, 4, 6), f) shouldEqual Nil
    dropWhile(List(2, 4, 3, 5, 4, 6), f) shouldEqual List(3, 5, 4, 6)
  }

  "The init method" should "work" in {
    init(Nil) shouldEqual Nil
    init(List(1)) shouldEqual Nil
    init(List(1, 2)) shouldEqual List(1)
    init(List(1, 2, 3)) shouldEqual List(1, 2)
  }

  "The length method" should "work" in {
    List.length(Nil) shouldEqual 0
    List.length(List(1)) shouldEqual 1
    List.length(List(1, 2, 3)) shouldEqual 3
  }

  "The foldLeft method" should "work" in {
    def f(a: String, b: Int): String = s"$a|$b"
    foldLeft(Nil, "")(f) shouldEqual ""
    foldLeft(List(1), "List = ")(f) shouldEqual "List = |1"
    foldLeft(List(1, 2, 3), "as = ")(f) shouldEqual "as = |1|2|3"
  }

  "The sumFoldLeft method" should "work" in {
    sumFoldLeft(Nil) shouldEqual 0
    sumFoldLeft(List(1)) shouldEqual 1
    sumFoldLeft(List(2, 4, 6)) shouldEqual 12
  }

  "The productFoldLeft method" should "work" in {
    productFoldLeft(Nil) shouldEqual 1
    productFoldLeft(List(3)) shouldEqual 3
    productFoldLeft(List(2, 4, 6)) shouldEqual 48
  }

  "The lengthFoldLeft method" should "work" in {
    lengthFoldLeft(Nil) shouldEqual 0
    lengthFoldLeft(List(1)) shouldEqual 1
    lengthFoldLeft(List(1, 2, 3)) shouldEqual 3
  }

  "The reverse method" should "work" in {
    reverse(Nil) shouldEqual Nil
    reverse(List(1)) shouldEqual List(1)
    reverse(List(1, 2)) shouldEqual List(2, 1)
  }

  "The foldRightViaFoldLeft method" should "work" in {
    def f(a: Double, b: Double): Double = b / a
    foldRightViaFoldLeft(Nil, 1.1)(f) shouldEqual 1.1
    foldRightViaFoldLeft(List(5.0), 25.0)(f) shouldEqual 5.0
    foldRightViaFoldLeft(List(2.0, 2.0), 16.0)(f) shouldEqual 4.0
  }

  "The foldLeftViaFoldRight method" should "work" in {
    def f(a: Double, b: Double): Double = a / b
    foldLeftViaFoldRight(Nil, 1.1)(f) shouldEqual 1.1
    foldLeftViaFoldRight(List(5.0), 25.0)(f) shouldEqual 5.0
    foldLeftViaFoldRight(List(2.0, 2.0), 16.0)(f) shouldEqual 4.0
  }

  "The append method" should "work" in {
    append(Nil, Nil) shouldEqual Nil
    append(List(1), Nil) shouldEqual List(1)
    append(Nil, List(1)) shouldEqual List(1)
    append(List(3, 4), List(1)) shouldEqual List(3, 4, 1)
  }

  "The concat method" should "work" in {
    concat(Nil) shouldEqual Nil
    concat(List(List(1, 2, 3))) shouldEqual List(1, 2, 3)
    concat(List(List(1, 2, 3), List(4, 5, 6))) shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  "The adds one method" should "work" in {
    addsOne(Nil) shouldEqual Nil
    addsOne(List(1)) shouldEqual List(2)
    addsOne(List(1, 2, 3)) shouldEqual List(2, 3, 4)
  }

  "The double2String method" should "work" in {
    double2String(Nil) shouldEqual Nil
    double2String(List(1.0)) shouldEqual List("1.0")
    double2String(List(1.0, 2.0, 3.0)) shouldEqual List("1.0", "2.0", "3.0")
  }

  "The map method" should "work" in {
    def f(x: Int): String = x.toString
    map(Nil)(f) shouldEqual Nil
    map(List(1))(f) shouldEqual List("1")
    map(List(1, 2, 3))(f) shouldEqual List("1", "2", "3")
  }

  "The filter method" should "work" in {
    def removeOdd(x: Int): Boolean = (x % 2) == 0
    filter(Nil)(removeOdd) shouldEqual Nil
    filter(List(1, 3))(removeOdd) shouldEqual Nil
    filter(List(1, 2, 3, 4, 5))(removeOdd) shouldEqual List(2, 4)
    filter(List(2, 4))(removeOdd) shouldEqual List(2, 4)
    filterViaFlatMap(Nil)(removeOdd) shouldEqual Nil
    filterViaFlatMap(List(1, 3))(removeOdd) shouldEqual Nil
    filterViaFlatMap(List(1, 2, 3, 4, 5))(removeOdd) shouldEqual List(2, 4)
    filterViaFlatMap(List(2, 4))(removeOdd) shouldEqual List(2, 4)
  }

  "The flatmap method" should "work" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
  }

  "The add list method" should "work" in {
    addLists(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
  }

  "The zipWith function" should "work" in {
    zipWith(List(1, 2, 3), List(4, 5, 6))((x, y) => x + y) shouldEqual List(5, 7, 9)
    zipWith(List("1", "2", "3"), List("4", "5", "6"))((x, y) => s"$x$y") shouldEqual List("14", "25", "36")
    zipWith(List(1, 2, 3), List(4.0, 5.0, 6.0))((x, y) => s"${x.toString}${y.toString}") shouldEqual List("14.0", "25.0", "36.0")
  }

  "The has subsequence method" should "work" in {
    hasSubSequence(List(1, 2, 3, 4), List(1, 2)) shouldEqual true
    hasSubSequence(List(1, 2, 3, 4), List(2, 3, 4)) shouldEqual true
    hasSubSequence(List(1, 2, 3, 4), List(4)) shouldEqual true
    hasSubSequence(List(1, 2, 3, 4), List(1, 3)) shouldEqual false
  }
}
