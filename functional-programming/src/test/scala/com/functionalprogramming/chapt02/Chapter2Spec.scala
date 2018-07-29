package com.functionalprogramming.chapt02

import org.scalatest._

class Chapter2Spec
  extends FlatSpec
    with Matchers {

  "The fib function" should "get the 0th Fibonacci number" in {
    Chapter2.fib(0) shouldEqual 0
  }
  it should "get the 1st Fibonacci number" in {
    Chapter2.fib(1) shouldEqual 1
  }
  it should "get the 2nd Fibonacci number" in {
    Chapter2.fib(2) shouldEqual 1
  }
  it should "get the 8th Fibonacci number" in {
    Chapter2.fib(8) shouldEqual 13
  }
  it should "get the 7th Fibonacci number" in {
    Chapter2.fib(7) shouldEqual 8
  }

  "The isSorted function" should "identify an ordered array" in {
    val sortedArray = Array(1, 4, 7, 10, 13, 16)
    Chapter2.isSorted(sortedArray, (a: Int, b: Int) => b == a + 3) shouldEqual true
  }
  it should "identify an unordered array" in {
    val unorderedArray = Array(1, 4, 7, 2, 4)
    Chapter2.isSorted(unorderedArray, (a: Int, b: Int) => a < b) shouldEqual false
  }

  "The curry function" should "work" in {
    def f(a: Int, b: Int): Int = a + b

    Chapter2.curry(f)(4)(5) shouldEqual 9
  }

  "The uncurry function" should "work" in {
    def f(a: Int)(b: Int): Int = a - b

    Chapter2.uncurry(f)(7, 3) shouldEqual 4
  }

  "The compose function" should "work" in {
    def f(a: String): String = a.reverse
    def g(a: String): String = s"$a!!!"

    Chapter2.compose(f, g)("Hello world") shouldEqual "!!!dlrow olleH"
  }

}
