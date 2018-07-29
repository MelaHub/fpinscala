package com.functionalprogramming.chapt03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A, B](tree: Tree[A])(implicit f: A => B): Tree[B] =
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l), map(r))
    }

  def fold[A, B](tree: Tree[A])(zero: A => B)(f: (B, B) => B) : B =
    tree match {
      case Leaf(v) => zero(v)
      case Branch(l, r) => f(fold(l)(zero)(f), fold(r)(zero)(f))
    }

}
