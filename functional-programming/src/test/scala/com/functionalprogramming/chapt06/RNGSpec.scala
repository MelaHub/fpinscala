package com.functionalprogramming.chapt06

import com.functionalprogramming.chapt06.RNG.Rand
import org.scalatest._

class RNGSpec
  extends FlatSpec
    with Matchers {

  val simpleRNG = SimpleRNG(0)

  def getStreamValue[A](currRNG: RNG, fn: (RNG) => (A, RNG))(): Stream[(A, RNG)] = {
    val (nextVal, nextRNG) = fn(currRNG)
    Stream.cons((nextVal, nextRNG), getStreamValue(nextRNG, fn))
  }

  "The nonNegativeInt method" should "actually generate only non negative numbers" in {
    val randomInts = getStreamValue(simpleRNG, (rng: RNG) => rng.nextInt)()
    val nonNegativeInts = getStreamValue(simpleRNG, RNG.nonNegativeInt)()
    val first20RandomInts = randomInts.take(20).toList
    first20RandomInts.exists(_._1 > 0)
    first20RandomInts.exists(_._1 < 0)
    nonNegativeInts.take(20).forall(_._1 > 0)
  }

  "The double method" should "work" in {
    def checkDouble(s: Stream[(Double, RNG)]) =
      s.take(20).forall{case (v, _) => v >= 0 && v < 1}

    val onlyDoubles = getStreamValue(simpleRNG, RNG.double)()
    val onlyDoublesViaMap = getStreamValue(simpleRNG, RNG.doubleViaMap)()
    checkDouble(onlyDoubles)
    checkDouble(onlyDoublesViaMap)
  }

  "The ints method" should "give back a list of random integers" in {
    RNG.ints(7)(SimpleRNG(7))._1.distinct.length shouldEqual(7)
  }

  "The map2 method" should "work" in {
    val rnd1 = RNG.nonNegativeInt(_)
    val rnd2 = RNG.double(_)
    val rnd3 = RNG.map2(rnd1, rnd2)((_, _))
    val rnd4 = RNG.map2ViaFlatMap(rnd1, rnd2)((_, _))
    def testMap2(rng: Rand[(Int, Double)]) = {
      val checkMap2 = getStreamValue(simpleRNG, rng)
      checkMap2.take(20).forall {
        case ((i, d), _) => i > 0 && d < 1 && d >= 0
      }
    }
    testMap2(rnd3)
    testMap2(rnd4)
  }

  "The sequence method" should "work" in {
    val s = RNG.sequence(List(RNG.int, RNG.int, RNG.int))
    getStreamValue(simpleRNG, s).take(20).forall{
      case (List(a, b, c), _) => a.isInstanceOf[Int] &&
        b.isInstanceOf[Int] && c.isInstanceOf[Int]
    }
  }

  "nonNegativeLessThen" should "work" in {
    getStreamValue(simpleRNG, RNG.nonNegativeLessThen(7)).take(20).forall {
      case (v, _) => v >= 0 && v < 7
    }
  }

}
