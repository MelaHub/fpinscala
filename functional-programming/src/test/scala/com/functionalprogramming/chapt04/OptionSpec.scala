package com.functionalprogramming.chapt04

import org.scalatest._

class Chapter4OptionSpec
  extends FlatSpec
    with Matchers {

  "The map method" should "correctly apply an f" in {
    val f = (x: Int) => x * 2
    Some(4).map(f) shouldEqual Some(8)
    None.map(f) shouldEqual None
  }

  "The flatmap method" should "correctly return a value" in {
    val f = (x: Int) => Some(x.toString)
    Some(4).flatMap(f) shouldEqual Some("4")
    None.flatMap(f) shouldEqual None
  }

  "The getOrElse method" should "return a default value if None" in {
    Some(42).getOrElse(7) shouldEqual(42)
    None.getOrElse(7) shouldEqual(7)
  }

  "The orElse method" should "work" in {
    Some(42) orElse Some(7) shouldEqual Some(42)
    None orElse Some(7) shouldEqual Some(7)
  }

  "The filter method" should "work" in {
    val f = (x: Int) => x % 2 == 0
    None.filter(f) shouldEqual None
    Some(42).filter(f) shouldEqual Some(42)
    Some(7).filter(f) shouldEqual None
  }

  "The variance method" should "get the variance" in {
    Option.variance(Seq(1, 2, 3, 4, 5)) shouldEqual Some(2)
    Option.variance(Seq()) shouldEqual None
  }

  "The map2 method " should "work" in {
    val f = (x: Int, y: Int) => x + y
    Option.map2(Some(3), Some(4))(f) shouldEqual Some(7)
    Option.map2(None, Some(3))(f) shouldEqual None
    Option.map2(Some(3), None)(f) shouldEqual None
    Option.map2(None, None)(f) shouldEqual None
  }

  "The sequence method" should "return None if one in the Seq is a None" in {
    Option.sequence(List()) shouldEqual Some(List())
    Option.sequence(List(Some(1))) shouldEqual Some(List(1))
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
    Option.sequence(List(Some(1), None, Some(2))) shouldEqual None
    Option.sequence(List(None)) shouldEqual None
    Option.sequenceAsTraverse(List()) shouldEqual Some(List())
    Option.sequenceAsTraverse(List(Some(1))) shouldEqual Some(List(1))
    Option.sequenceAsTraverse(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
    Option.sequenceAsTraverse(List(Some(1), None, Some(2))) shouldEqual None
    Option.sequenceAsTraverse(List(None)) shouldEqual None
  }

  "The traverse function" should "work" in {
    val f = (x: String) => Option.Try(x.toInt)
    Option.traverse(List[String]())(f) shouldEqual Some(List())
    Option.traverse(List("1", "2"))(f) shouldEqual Some(List(1, 2))
    Option.traverse(List("1", "hello", "2"))(f) shouldEqual None
  }

}
