package com.functionalprogramming.chapt08


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

