package com.functionalprogramming.chapt08

case class SGen[A](forSize: Int => Gen[A])

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN1(n, g))

}
