package com.functionalprogramming.chapt08

import com.functionalprogramming.chapt06.{RNG, SimpleRNG}
import org.scalatest._

class PropSpec
  extends FlatSpec
    with Matchers {

  val smallInt = Gen.choose(-10, 10)

  def run(p: Prop, maxSize: Int=100, testCases: Int=100, rng: RNG=SimpleRNG(System.currentTimeMillis())): Result =
    p.run(maxSize, testCases, rng)

  "The max of a list" should "be greater than or equal to every other element in the list" in {
    val maxProp = Prop.forAll(SGen.listOf1(smallInt)) {
      ns =>
        val max = ns.max
        !ns.exists(_ > max)
    }
    run(maxProp) shouldEqual Passed
  }

  "List.sorted" should "return a list with sorted elements" in {
    val sortedProp = Prop.forAll(SGen.listOf(smallInt)) {
      s =>
        val sortedList = s.sorted
        s.isEmpty || s.tail.isEmpty || sortedList
          .zip(sortedList.tail)
          .forall{
            case (a, b) => a < b
          }
    }
    run(sortedProp) shouldEqual Passed
  }

}
