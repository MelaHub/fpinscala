package com.functionalprogramming.chapt05

import org.scalatest._

class StreamSpec
  extends FlatSpec
    with Matchers {

  "The toList method" should "work" in {
    Stream(1, 2, 3, 4).toList shouldEqual List(1, 2, 3, 4)
    Empty.toList shouldEqual Nil
  }

  "The take method" should "return a stream with the first n values" in {
    def checkTake(fn: (Stream[Int], Int) => Stream[Int]) = {
      fn(Empty, 3) shouldEqual Empty
      fn(Stream(1, 2, 3, 4, 5), 3).toList shouldEqual List(1, 2, 3)
      fn(Stream(1), 3).toList shouldEqual List(1)
    }
    checkTake((s: Stream[Int], n: Int) => s.take(n))
    checkTake((s: Stream[Int], n: Int) => s.takeViaUnfold(n))
  }

  "The drop method" should "return a stream without the first n values" in {
    Empty.drop(1) shouldEqual Empty
    Stream(1, 2, 3).drop(5).toList shouldEqual Nil
    Stream(1, 2, 3, 4, 5).drop(3).toList shouldEqual List(4, 5)
  }

  "The take while method" should "return a stream of elements matching the predicate" in {
    val f = (x: Int) => x % 2 == 0
    def checkTakeWhile(fn: Stream[Int] => Stream[Int]) = {
      fn(Empty) shouldEqual Empty
      fn(Stream(2, 4, 6, 5, 4, 2)).toList shouldEqual List(2, 4, 6)
      fn(Stream(1, 3, 5)) shouldEqual Empty
    }

    checkTakeWhile((s: Stream[Int]) => s.takeWhile(f))
    checkTakeWhile((s: Stream[Int]) => s.takeWhileFoldRight(f))
    checkTakeWhile((s: Stream[Int]) => s.takeWhileViaUnfold(f))
  }

  "The forall method" should "return true only f the predicate is true for all elements" in {
    val f = (x: Int) => x % 2 == 0
    Stream(1, 2, 3, 4) forAll f shouldEqual false
    Stream(2, 4, 6, 8) forAll f shouldEqual true
  }

  "The headOptionFoldRight method" should "return the first element if not empty" in {
    Empty.headOptionFoldRight shouldEqual None
    Stream(1, 2, 3).headOptionFoldRight shouldEqual Some(1)
  }

  "The map method" should "work on streams" in {
    val f = (x: Int) => x.toString
    Empty.map(f) shouldEqual Empty
    Stream(1, 2).map(f).toList shouldEqual List("1", "2")
    Empty.mapViaUnfold(f) shouldEqual Empty
    Stream(1, 2).mapViaUnfold(f).toList shouldEqual List("1", "2")
  }

  "The append method" should "work on streams" in {
    Empty.append(Empty) shouldEqual Empty
    Empty.append(Stream(1, 2)).toList shouldEqual List(1, 2)
    Stream(3, 4).append(Empty).toList shouldEqual List(3, 4)
    Stream(1, 2).append(Stream(3, 4)).toList shouldEqual List(1, 2, 3, 4)
  }

  "The filter method" should "work on streams" in  {
    val f = (x: Int) => x % 2 == 0
    Empty.filter(f) shouldEqual Empty
    Stream(3, 5).filter(f) shouldEqual Empty
    Stream(1, 2, 3, 4, 5).filter(f).toList shouldEqual List(2, 4)
  }

  "The flatmap method" should "work on streams" in {
    val f = (x: Int) => Stream.cons(x.toString, Empty)
    Stream(2, 4, 5).flatMap(f).toList shouldEqual List("2", "4", "5")
  }

  "The constant method" should "return a stream of constants" in {
    def checkStreamViaFn(fn: Int => Stream[Int]) = {
      val s = fn(7).take(10).toList
      s.length shouldEqual(10)
      s.distinct.length shouldEqual(1)
      s.head shouldEqual(7)
    }
    checkStreamViaFn(Stream.constant)
    checkStreamViaFn(Stream.constantViaUnfold)
  }

  "The from method" should "work" in {
    Stream.from(7).take(5).toList shouldEqual(List(7, 8, 9, 10, 11))
    Stream.fromViaUnfold(7).take(5).toList shouldEqual(List(7, 8, 9, 10, 11))
  }

  "The fibs method" should "work" in {
    Stream.fibs.take(7).toList shouldEqual(List(0, 1, 1, 2, 3, 5, 8))
    Stream.fibsViaUnfold.take(7).toList shouldEqual(List(0, 1, 1, 2, 3, 5, 8))
  }

  "The zip method" should "work" in {
    Stream.constant(3).zipWith(Stream.constant(4))((x: Int, y: Int) => x + y).take(5).toList shouldEqual List.fill(5)(7)
  }

  "The zipall method" should "work" in {
    Stream(1, 2, 3, 4).zipAll(Stream(5, 6, 7, 8)).toList shouldEqual List((Some(1), Some(5)), (Some(2), Some(6)), (Some(3), Some(7)), (Some(4), Some(8)))
    Stream(1, 2).zipAll(Stream(5, 6, 7, 8)).toList shouldEqual List((Some(1), Some(5)), (Some(2), Some(6)), (None, Some(7)), (None, Some(8)))
    Stream(1, 2, 3, 4).zipAll(Stream(5)).toList shouldEqual List((Some(1), Some(5)), (Some(2), None), (Some(3), None), (Some(4), None))
  }

  "The startsWith method" should "work" in {
    Stream(1, 2, 3) startsWith Stream(1, 2) shouldEqual true
    Stream(1, 2, 3) startsWith Stream(1, 3) shouldEqual false
  }

  "The tails method" should "return all tails" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldEqual List(List(1, 2, 3), List(2, 3), List(3))
  }

  "The scan right method" should "work" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldEqual List(6, 5, 3, 0)
  }

}
