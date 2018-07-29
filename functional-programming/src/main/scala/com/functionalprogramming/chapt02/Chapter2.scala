package com.functionalprogramming.chapt02

object Chapter2 {

  def fib(n: Int): Int ={
    def go(first: Int, second: Int, n: Int): Int = {
      if (n == 0)
        second
      else
        go(second, first + second, n - 1)
    }

    n match {
      case 0 => 0
      case 1 => 1
      case _ => go(0, 1, n - 2)
    }

  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(elementsToCompare: Array[A]): Boolean = {
      if (elementsToCompare.length <= 1)
        true
      else
        ordered(elementsToCompare.head, elementsToCompare.tail.head) && go(elementsToCompare.tail)
    }
    go(as)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => ((b: B) => f(a, b))

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}
