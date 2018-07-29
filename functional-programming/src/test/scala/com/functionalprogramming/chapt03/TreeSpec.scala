package com.functionalprogramming.chapt03

import com.functionalprogramming.chapt03.Tree._
import org.scalatest._

class TreeSpec
  extends FlatSpec
    with Matchers {

  val tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(5))
  val stringTree = Branch(Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))), Leaf("5"))

  "The size method" should "return the correct size of a tree" in {
    Tree.size(tree) shouldEqual 7
  }

  "The maximum method" should "return the maximum value in a tree of integers" in {
    maximum(tree) shouldEqual 5
  }

  "The depth method" should "return the max path length from the root" in {
    depth(tree) shouldEqual 3
  }

  "The map method" should "work on a tree" in {
    map(tree)(_.toString) shouldEqual stringTree
  }

  "The fold method" should "reimplement all other methods" in {
    fold(tree)(_ => 1)((x, y) => 1 + x + y) shouldEqual 7
    fold(tree)(x => identity(x))((x, y) => x max y)
    fold(tree)(_ => 0)((x, y) => 1 + (x max y)) shouldEqual 3
    def mapZero(x: Int): Tree[String] = Leaf(x.toString)
    fold(tree)(mapZero)((x, y) => Branch(x, y)) shouldEqual stringTree
  }

}
