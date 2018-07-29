package com.functionalprogramming.chapt10

import com.functionalprogramming.chapt06.RNG.State
import com.functionalprogramming.chapt06.{RNG, SimpleRNG}
import com.functionalprogramming.chapt08.{Gen, Passed, Prop, Result}
import org.scalatest._

class MonoidSpec
  extends FlatSpec
    with Matchers {

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop
      .forAll(
        for {
          x <- gen
          y <- gen
          z <- gen
        } yield (x, y, z)){
        case (x, y, z) => m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
      } && Prop.forAll(gen)(x => m.op(x, m.zero) == x)

  "Monoid laws " should " be valid for intAddition" in {
    val result = monoidLaws(Monoid.intAddition, Gen.choose(0, 1000)).run(10, 100, SimpleRNG(System.currentTimeMillis()))
    result shouldEqual Passed
  }

  "Monoid laws " should " be valid for productMonoid" in {
    val intMonoid = Monoid.intAddition
    val boolMonoid = Monoid.booleanAnd
    val gen = Gen((State(RNG.int).map(i => (i, i % 2 == 0))))
    val result = monoidLaws(
      Monoid.productMonoid(intMonoid, boolMonoid),
      gen).run(10, 100, SimpleRNG(System.currentTimeMillis()))
    result shouldEqual Passed
  }

  "Ordered" should "work" in {
    Monoid.ordered(IndexedSeq(1, 2, 3, 4, 5, 6)) shouldEqual true
    Monoid.ordered(IndexedSeq(1, 2, 10, 4, 5, 6)) shouldEqual false
  }

  "count words" should "work" in {
    Monoid.countWords("lorem ipsum do") shouldEqual 3
    Monoid.countWords("lorem") shouldEqual 1
    Monoid.countWords("cantami o diva del pelide achille l'ira funesta") shouldEqual 8
  }

  "bag" should "work" in {
    val b = Monoid.bag(Vector("a", "rose", "is", "a", "rose"))
    b.keySet should contain theSameElementsAs Seq("a", "rose", "is")
    b("a") shouldEqual (2)
    b("rose") shouldEqual(2)
    b("is") shouldEqual(1)
  }


}
