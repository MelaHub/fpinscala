package com.functionalprogramming.chapt06

import com.functionalprogramming.chapt06.RNG.State

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def action = (i: Input) =>
    (m : Machine) =>
      (i, m) match {
        case (Coin, Machine(true, candies, coins)) if candies > 0 => m.copy(locked=false, coins=coins+1)
        case (Turn, Machine(false, candies, coins)) if candies > 0 => m.copy(locked=true, candies=candies-1)
        case _ => m
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs.map(State.modify[Machine] _ compose action))
      m <- State.get
    } yield (m.candies, m.coins)

}
