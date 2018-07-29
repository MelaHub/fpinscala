package com.functionalprogramming.chapt08

import com.functionalprogramming.chapt06.RNG
import com.functionalprogramming.chapt06.RNG.State

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(i, this))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val choseG1 = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double).flatMap(d => if (d < choseG1) g1._1.sample else g2._1.sample))
  }

  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.int).map(_ % 2 == 0))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.int).map(_ * (stopExclusive - start) + start))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill[State[RNG, A]](n)(g.sample)))

  def listOfN1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    listOfN(n max 1, g)
}

