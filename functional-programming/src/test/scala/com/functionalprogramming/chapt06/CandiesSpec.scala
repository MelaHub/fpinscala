package com.functionalprogramming.chapt06

import org.scalatest.{FlatSpec, Matchers}

class CandiesSpec
  extends FlatSpec
    with Matchers {

  val initialState = Machine(true, 5, 10)

  "Inserting a coin into a locked machine" should "cause it to unlock if there's enough candies left" in {
    Machine.action(Coin)(initialState) shouldEqual Machine(false, 5, 11)
  }

  "Turning the knob on an unlocked machine" should "cause it to dispense candy and become unlocked" in {
    Machine.action(Turn)(Machine(false, 5, 11)) shouldEqual Machine(true, 4, 11)
  }

  "Turning the knob on a locked machine" should "do nothing" in {
    Machine.action(Turn)(initialState) shouldEqual initialState
  }

  "Inserting a coin into an unlocked machine" should "do nothing" in {
    Machine.action(Coin)(initialState.copy(locked=false)) shouldEqual initialState.copy(locked=false)
  }

  "A machine that's out of candies" should "ignore all inputs" in {
    val outOfCandies = initialState.copy(candies = 0)

    Machine.action(Turn)(outOfCandies) shouldEqual outOfCandies
    Machine.action(Coin)(outOfCandies) shouldEqual outOfCandies
  }

  "The simulate method" should "work" in {
    val expectedCoins = 14
    val expectedCandies = 1

    val inputs = List.fill(4)(List(Coin, Turn)).flatten

    val result = Machine.simulateMachine(inputs).run(initialState)

    result._1 shouldEqual (expectedCandies, expectedCoins)
  }


}
