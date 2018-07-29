package com.functionalprogramming.chapt07

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest._

class ParSpec
  extends FlatSpec
    with Matchers {

  import Par._

  val pool: ExecutorService = Executors.newFixedThreadPool(4)

  "The parallel sum function" should "work on IndexedSeq" in {
    val a = IndexedSeq(1, 2, 3, 4, 5, 6)
    sum(a)(pool).get() shouldEqual 21
  }

  "The max function" should "work on IndexedSeq" in {
    val a = IndexedSeq(1, 6, 4, 2, 7, 3)
    max(a)(pool).get() shouldEqual 7
  }

  "The countWords function" should "work" in {
    val a = List(
      "ciao sto studiando fp",
      "hello",
      "not a long paragraph uh?"
    )
    countWords(a)(pool).get() shouldEqual 10
  }



}
