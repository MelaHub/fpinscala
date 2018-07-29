package com.functionalprogramming.chapt08

import com.functionalprogramming.chapt05.Stream
import com.functionalprogramming.chapt06.RNG


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop (
    (m: MaxSize, n: TestCases, r: RNG) => run(m, n, r) match  {
      case Passed | Proved => p.run(m, n, r)
      case failed => failed
    }
  )

  def ||(p: Prop): Prop = Prop (
    (m: MaxSize, n: TestCases, r: RNG) => run(m, n, r) match  {
      case Passed | Proved => Passed
      case _: Falsified => p.run(m, n, r)
    }
  )

}

object Prop {

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_: MaxSize, n: TestCases, rng: RNG) =>
      randomStream(as)(rng)
        .zipWith(Stream.from(0))((_, _))
        .take(n)
        .map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(s"Failed with $a because of ${e.getMessage}", i)
          }
        }
        .filter(_.isFalsified)
        .headOption
        .getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (m: MaxSize, n: TestCases, rng: RNG) =>
      val casesPerSize = (n + (m - 1)) / m
      val props: Stream[Prop] =
        Stream
          .from(0)
          .take((n min m) + 1)
          .map(i => Prop.forAll(g(i))(f))

      val prop: Prop =
        props
          .map(p => Prop{
            (max, _, rng) => p.run(max, casesPerSize, rng)
          })
          .toList
          .reduce(_ && _)

      prop.run(m, n, rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def check(p: => Boolean): Prop = Prop(
    (_: MaxSize, _: TestCases, _: RNG) => if (p) Passed else Falsified("()", 0)
  )
}



